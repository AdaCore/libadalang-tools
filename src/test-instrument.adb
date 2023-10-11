------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                         Copyright (C) 2023, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.SHA1;

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;    use GNATCOLL.VFS;

with Langkit_Support.Errors;
with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Common;    use Libadalang.Common;

with Test.Common;                use Test.Common;
with Test.Skeleton.Source_Table; use Test.Skeleton.Source_Table;

with TGen.LAL_Utils;
with TGen.Libgen;
with TGen.Marshalling;
with TGen.Strings; use TGen.Strings;
with TGen.Types.Translation;

with Utils.Command_Lines; use Utils.Command_Lines;

package body Test.Instrument is

   Me : constant Trace_Handle := Create ("Instrument", Default => Off);

   function Padding (N : Ada_Node'Class) return Natural is
     (Natural (First_Column_Number (N)) - 1);
   --  Necessary padding for current element

   function Image (L, R : Token_Reference; Charset : String) return String is
     (Encode (Text (L, R), Charset));
   --  Returns Image of given slice

   function Inspect_Spec (Node : Ada_Node'Class) return Visit_Status;
   --  Collects info on subprograms that need to be instrumented

   procedure Process_Package_Body (Decl : Basic_Decl);
   --  Generates instrumented package body

   procedure Process_Subprogram_Body (Decl : Base_Subp_Body);
   --  Generates instrumented subprogran body

   function From_Same_Unit (L, R : Ada_Node'Class) return Boolean is
     (not L.Is_Null and then not R.Is_Null and then
      L.P_Enclosing_Compilation_Unit = R.P_Enclosing_Compilation_Unit);

   Included_Subps : String_Ordered_Set;

   ------------------
   -- Inspect_Spec --
   ------------------

   function Inspect_Spec (Node : Ada_Node'Class) return Visit_Status is
      Errors : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Kind (Node) = Ada_Package_Decl then
         return Into;
      elsif Kind (Node) in Ada_Single_Protected_Decl |
                           Ada_Protected_Type_Decl   |
                           Ada_Single_Task_Decl      |
                           Ada_Task_Type_Decl
      then
         return Over;
      end if;

      if Kind (Node) = Ada_Subp_Decl then
         if not TGen.Libgen.Include_Subp
           (TGen_Libgen_Ctx,
            Node.As_Basic_Decl,
            Errors)
         then
            Report_Std (Ada.Strings.Unbounded.To_String (Errors));
            return Over;
         end if;

         Included_Subps.Include
           (Image (Node.As_Basic_Decl.P_Unique_Identifying_Name));

         return Over;
      end if;

      return Into;
   end Inspect_Spec;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Analysis_Unit) is
      F_Name : constant String := The_Unit.Get_Filename;
      CU     : Compilation_Unit;
      Unit   : Basic_Decl;

      Instr_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      procedure Error_Cleanup;
      --  Deletes usuccessfull attempt to instrument given source

      procedure Error_Cleanup is
         Success     : Boolean;
         Delete_Body : constant String :=
           Ada.Strings.Unbounded.To_String (Instr_File_Name);
         use Char_Sequential_IO;
      begin
         if Is_Open (Output_File) then
            Close (Output_File);
         end if;
         GNAT.OS_Lib.Delete_File (Delete_Body, Success);
         if not Success then
            Cmd_Error_No_Help
              ("cannot delete "
               & Ada.Strings.Unbounded.To_String (Instr_File_Name));
         end if;
      end Error_Cleanup;
   begin
      if The_Unit.Root.Kind /= Ada_Compilation_Unit then
         --  For example, it can be a Pragma_Node_List for a body source
         --  containing pragma No_Body.
         return;
      end if;

      CU := Root (The_Unit).As_Compilation_Unit;

      if P_Unit_Kind (CU) = Unit_Body then
         --  Only interested in specs, corresponding bodies are processed
         --  by explicit request.
         return;
      end if;

      Trace (Me, "instrumenting " & F_Name);
      Unit := CU.F_Body.As_Library_Item.F_Item;
      Included_Subps.Clear;
      Traverse (Unit, Inspect_Spec'Access);

      if Get_Source_Body (F_Name) /= ""
        and then not Included_Subps.Is_Empty
      then
         CU := CU.P_Other_Part;
         Trace (Me, "instrumenting " & CU.Unit.Get_Filename);

         declare
            F : File_Array_Access;
         begin
            Append
              (F,
               GNATCOLL.VFS.Create
                 (+(Get_Source_Instr_Dir (CU.Unit.Get_Filename))));
            Create_Dirs (F);
            Unchecked_Free (F);
         end;

         Instr_File_Name := Ada.Strings.Unbounded.To_Unbounded_String
           (Get_Source_Instr_Dir (CU.Unit.Get_Filename)
            & GNAT.OS_Lib.Directory_Separator
            & Simple_Name (CU.Unit.Get_Filename));
         Create (Ada.Strings.Unbounded.To_String (Instr_File_Name));

         S_Put (0, "pragma Style_Checks (Off); pragma Warnings (Off);");
         Put_New_Line;

         Unit := CU.F_Body.As_Library_Item.F_Item;

         if Previous (Unit.Token_Start) /= No_Token then
            --  There is some kind of header or pragmas or with-use clauses
            S_Put
              (0,
               Image
                 (CU.Token_Start,
                  Previous (Unit.Token_Start),
                  CU.Unit.Get_Charset));
         end if;

         S_Put (0, "with Ada.Streams;");
         Put_New_Line;
         S_Put (0, "with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;");
         Put_New_Line;
         S_Put (0, "with TGen.Instr_Support;");
         Put_New_Line;

         for U of TGen.Libgen.Required_Support_Packages
           (TGen_Libgen_Ctx,
            TGen.LAL_Utils.Convert_Qualified_Name
              (CU.P_Syntactic_Fully_Qualified_Name))
         loop
            S_Put (0, "with " & To_Ada (U) & ";");
            Put_New_Line;
         end loop;

         Process_Package_Body (Unit);

         Close_File;
      end if;

      TGen.Libgen.Generate
        (TGen_Libgen_Ctx,
         Part => [TGen.Libgen.Marshalling_Part => True, others => False]);

   exception
      when Ex : Langkit_Support.Errors.Property_Error =>

         Source_Processing_Failed := True;

         Report_Err
           ("lal error while instrumenting "
            & Base_Name (Ada.Strings.Unbounded.To_String (Instr_File_Name)));
         Report_Err ("source not instrumented");

         Report_Ex (Ex);
         Error_Cleanup;

      when Ex : others =>

         Source_Processing_Failed := True;

         Report_Err
           ("unexpected error while instrumenting "
            & Base_Name (Ada.Strings.Unbounded.To_String (Instr_File_Name)));

         Report_Ex (Ex);
         Error_Cleanup;
   end Process_Source;

   --------------------------
   -- Process_Package_Body --
   --------------------------

   procedure Process_Package_Body (Decl : Basic_Decl) is
   begin
      S_Put
        (Padding (Decl),
         Image
           (Decl.Token_Start,
            Previous
              (Decl.As_Package_Body.F_Decls.First_Child.Token_Start),
            Decl.Unit.Get_Charset));
      Put_New_Line;

      for D of Decl.As_Package_Body.F_Decls.F_Decls loop

         if D.Kind = Ada_Package_Body then
            if From_Same_Unit (D.As_Package_Body.P_Decl_Part, D) then
               --  Corresponding spec not visible to outside worls,
               --  nothing to do here
               S_Put (Padding (D), Node_Image (D));
               Put_New_Line;
            else
               Process_Package_Body (D.As_Basic_Decl);
            end if;

            --  Atm only the following constructs are processed (given that
            --  corresponding specifications are declare in the unit spec):
            --    *  regular subprogram body;
            --    *  expression function body;
            --    *  renaming as body;
            --    *  null procedure.
            --  Everything else is ignored.
         elsif D.Kind in Ada_Subp_Body then

            if not From_Same_Unit (D.As_Subp_Body.P_Decl_Part, D) and then
              D.As_Subp_Body.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                         (Image (D.As_Subp_Body.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         elsif D.Kind in Ada_Expr_Function then

            if not From_Same_Unit (D.As_Expr_Function.P_Decl_Part, D) and then
              D.As_Expr_Function.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                         (Image (D.As_Expr_Function.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         elsif D.Kind in Ada_Null_Subp_Decl then

            if not From_Same_Unit (D.As_Null_Subp_Decl.P_Decl_Part, D) and then
              D.As_Null_Subp_Decl.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                (Image (D.As_Null_Subp_Decl.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         elsif D.Kind in Ada_Subp_Renaming_Decl then

            if not From_Same_Unit (D.As_Subp_Renaming_Decl.P_Decl_Part, D)
              and then D.As_Subp_Renaming_Decl.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                (Image (D.As_Subp_Renaming_Decl.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         else
            S_Put (Padding (D), Node_Image (D));
            Put_New_Line;
         end if;

         Put_New_Line;

      end loop;

      S_Put
        (Padding (Decl),
         Image
           (Next
              (Decl.As_Package_Body.F_Decls.Last_Child.Token_End),
            Decl.Token_End,
            Decl.Unit.Get_Charset));
      Put_New_Line;
   end Process_Package_Body;

   -----------------------------
   -- Process_Subprogram_Body --
   -----------------------------

   procedure Process_Subprogram_Body (Decl : Base_Subp_Body) is
      Pad : constant Natural := Padding (Decl);

      use TGen.Strings.Ada_Identifier_Vectors;
   begin
      if Decl.Kind = Ada_Subp_Body then
         S_Put
           (Pad,
            Image
              (Decl.Token_Start,
               Previous (Decl.As_Subp_Body.F_Decls.Token_Start),
               Decl.Unit.Get_Charset));
      elsif Decl.Kind = Ada_Expr_Function then
         S_Put
           (Pad,
            Image
              (Decl.Token_Start,
               Previous (Decl.As_Expr_Function.F_Expr.Token_Start),
               Decl.Unit.Get_Charset));
      elsif Decl.Kind = Ada_Null_Subp_Decl then
         S_Put
           (Pad,
            Image
              (Decl.Token_Start,
               Decl.As_Null_Subp_Decl.F_Subp_Spec.Token_End,
               Decl.Unit.Get_Charset)
            & " is");
      elsif Decl.Kind = Ada_Subp_Renaming_Decl then
         S_Put
           (Pad,
            Image
              (Decl.Token_Start,
               Previous (Decl.As_Subp_Renaming_Decl.F_Renames.Token_Start),
               Decl.Unit.Get_Charset)
            & " is");
      else
         raise Instrumentation_Error with
           "unsupported body kind: " & Decl.Image;
      end if;
      Put_New_Line;

      S_Put (Pad + 3, "function GNATTEST_Dump_Inputs return Boolean is");
      Put_New_Line;

      S_Put (Pad + 6, "GNATTEST_F : Ada.Streams.Stream_IO.File_Type;");
      Put_New_Line;
      S_Put (Pad + 6, "GNATTEST_S : Stream_Access;");
      Put_New_Line;

      S_Put (Pad + 3, "begin");
      Put_New_Line;

      S_Put
        (Pad + 6,
         "if TGen.Instr_Support.Subp_Hash /= """
         & Mangle_Hash_Full (Decl.P_Decl_Part)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "or else TGen.Instr_Support.Nesting_Hash /= """
         & GNAT.SHA1.Digest (Get_Nesting (Decl.P_Decl_Part))
         & """");
      Put_New_Line;
      S_Put (Pad + 6, "then");
      Put_New_Line;
      S_Put (Pad + 9, "return True;");
      Put_New_Line;
      S_Put (Pad + 6, "end if;");
      Put_New_Line;
      Put_New_Line;
      S_Put
        (Pad + 6,
         "TGen.Instr_Support.Recursion_Depth := "
         & "TGen.Instr_Support.Recursion_Depth + 1;");
      Put_New_Line;
      S_Put
        (Pad + 6,
         "if TGen.Instr_Support.Recursion_Depth > 1 then");
      Put_New_Line;
      S_Put (Pad + 9, "return True;");
      Put_New_Line;
      S_Put (Pad + 6, "end if;");
      Put_New_Line;
      Put_New_Line;

      S_Put
        (Pad + 6,
         "Create (GNATTEST_F, Out_File, """
         & TGen.LAL_Utils.Default_Blob_Test_Filename (Decl.P_Decl_Part)
         & "-"" & TGen.Instr_Support.Test_Input_Number);");
      Put_New_Line;
      S_Put
        (Pad + 6,
         "TGen.Instr_Support.Test_Input_Counter := "
         & "TGen.Instr_Support.Test_Input_Counter + 1;");
      Put_New_Line;
      S_Put (Pad + 6, "GNATTEST_S := Stream (GNATTEST_F);");
      Put_New_Line;
      Put_New_Line;

      for Param of Decl.F_Subp_Spec.P_Params loop
         declare
            Param_Typ : constant TGen.Types.Typ'Class :=
              TGen.Types.Translation.Translate
                (Param.F_Type_Expr).Res.Unchecked_Get.all;
         begin

            if Param_Typ.Package_Name = To_Qualified_Name ("standard") then
               for Name of Param.F_Ids loop
                  S_Put
                    (Pad + 6,
                      "TGen.TGen_Support."
                     & TGen.Marshalling.Output_Fname_For_Typ (Param_Typ.Name)
                     & " (GNATTEST_S, "
                     & Node_Image (Name)
                     & ");");
                  Put_New_Line;
               end loop;
            else
               for Name of Param.F_Ids loop
                  S_Put
                    (Pad + 6,
                     To_Ada (Param_Typ.Package_Name)
                     & ".TGen_Support."
                     & TGen.Marshalling.Output_Fname_For_Typ (Param_Typ.Name)
                     & " (GNATTEST_S, "
                     & Node_Image (Name)
                     & ");");
                  Put_New_Line;
               end loop;
            end if;
         end;
      end loop;

      Put_New_Line;

      S_Put (Pad + 6, "Close (GNATTEST_F);");
      Put_New_Line;

      S_Put (Pad + 6, "return True;");
      Put_New_Line;
      S_Put (Pad + 3, "end GNATTEST_Dump_Inputs;");
      Put_New_Line;
      S_Put (Pad + 3, "Dummy_GNATTEST : Boolean := GNATTEST_Dump_Inputs;");
      Put_New_Line;
      Put_New_Line;

      --  Turn original subprogram into wrapper
      S_Put
        (Pad + 3,
         Image
           (Decl.Token_Start,
            Decl.F_Subp_Spec.F_Subp_Name.Token_End,
            Decl.Unit.Get_Charset)
         & "_GNATTEST "
         & Image
           (Next (Decl.F_Subp_Spec.F_Subp_Name.Token_End),
            Decl.F_Subp_Spec.Token_End,
            Decl.Unit.Get_Charset));

      if Decl.Kind = Ada_Subp_Body then
         Put_New_Line;
         S_Put (1, "is");
         Put_New_Line;
         S_Put
           (Pad + 3,
            Image
              (Decl.As_Subp_Body.F_Decls.Token_Start,
               Decl.As_Subp_Body.F_Stmts.Token_End,
               Decl.Unit.Get_Charset));
      elsif Decl.Kind = Ada_Expr_Function then
         Put_New_Line;
         S_Put (1, "is");
         Put_New_Line;
         S_Put (Pad, "begin");
         Put_New_Line;
         S_Put
           (Pad + 3,
            "return "
            & Node_Image (Decl.As_Expr_Function.F_Expr)
            & ";");
         Put_New_Line;
      elsif Decl.Kind = Ada_Subp_Renaming_Decl then
         S_Put
           (1,
            Node_Image (Decl.As_Subp_Renaming_Decl.F_Renames)
            & ";");
         Put_New_Line;
      elsif Decl.Kind = Ada_Null_Subp_Decl then
         S_Put (1, "is null;");
         Put_New_Line;
      else
         raise Instrumentation_Error with
           "unsupported body kind: " & Decl.Image;
      end if;

      if Decl.Kind not in Ada_Subp_Renaming_Decl | Ada_Null_Subp_Decl then
         Put_New_Line;
         S_Put
           (Pad + 3,
            "end "
            & Node_Image (Decl.F_Subp_Spec.F_Subp_Name)
            & "_GNATTEST;");
         Put_New_Line;
         Put_New_Line;
      end if;

      --  Call the wrapper and decrease the recursivity counter afterwards
      S_Put (Pad, "begin");
      Put_New_Line;

      if Decl.F_Subp_Spec.F_Subp_Kind = Ada_Subp_Kind_Function then
         S_Put
           (Pad + 3,
            "return GNATTEST_Result : "
            & Node_Image (Decl.F_Subp_Spec.F_Subp_Returns)
            & " := "
            & Node_Image (Decl.F_Subp_Spec.F_Subp_Name) & "_GNATTEST (");
      else
         S_Put
           (Pad + 3,
            Node_Image (Decl.F_Subp_Spec.F_Subp_Name) & "_GNATTEST  (");
      end if;

      declare
         First_Param : Boolean := True;
      begin
         for Param of Decl.F_Subp_Spec.P_Params loop
            for Name of Param.F_Ids loop
               if First_Param then
                  First_Param := False;
                  Put_New_Line;
               else
                  S_Put (0, ",");
                  Put_New_Line;
               end if;
               S_Put (Pad + 5, Node_Image (Name));
            end loop;
         end loop;
      end;

      if Decl.F_Subp_Spec.F_Subp_Kind = Ada_Subp_Kind_Function then
         S_Put (0, ") do");
      else
         S_Put (0, ");");
      end if;

      Put_New_Line;
      Put_New_Line;
      S_Put
        (Pad + 6,
         "if TGen.Instr_Support.Subp_Hash = """
         & Mangle_Hash_Full (Decl.P_Decl_Part)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "and then TGen.Instr_Support.Nesting_Hash = """
         & GNAT.SHA1.Digest (Get_Nesting (Decl.P_Decl_Part))
         & """");
      Put_New_Line;
      S_Put (Pad + 6, "then");
      Put_New_Line;
      S_Put
        (Pad + 9,
         "TGen.Instr_Support.Recursion_Depth := "
         & "TGen.Instr_Support.Recursion_Depth - 1;");
      Put_New_Line;
      S_Put (Pad + 6, "end if;");

      Put_New_Line;
      Put_New_Line;
      if Decl.F_Subp_Spec.F_Subp_Kind = Ada_Subp_Kind_Function then
         S_Put (Pad + 3, "end return;");
      end if;
      Put_New_Line;
      Put_New_Line;

      --  If there was an unhandled exception in the original subprogram,
      --  decrease recursivity counter nad re-raise it.
      Put_New_Line;
      Put_New_Line;
      S_Put (Pad, "exception");
      Put_New_Line;
      S_Put (Pad + 3, "when others =>");
      Put_New_Line;
      S_Put
        (Pad + 6,
         "if TGen.Instr_Support.Subp_Hash = """
         & Mangle_Hash_Full (Decl.P_Decl_Part)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "and then TGen.Instr_Support.Nesting_Hash = """
         & GNAT.SHA1.Digest (Get_Nesting (Decl.P_Decl_Part))
         & """");
      Put_New_Line;
      S_Put (Pad + 6, "then");
      Put_New_Line;
      S_Put
        (Pad + 9,
         "TGen.Instr_Support.Recursion_Depth := "
         & "TGen.Instr_Support.Recursion_Depth - 1;");
      Put_New_Line;
      S_Put (Pad + 6, "end if;");
      Put_New_Line;
      S_Put (Pad + 6, "raise;");
      Put_New_Line;

      S_Put
        (Pad,
         "end "
         & Node_Image (Decl.F_Subp_Spec.F_Subp_Name)
         & ";");

   end Process_Subprogram_Body;

end Test.Instrument;
