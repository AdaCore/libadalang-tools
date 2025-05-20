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

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Hashed_Sets;
with Ada.Characters.Conversions;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Fixed;

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

   function Padding (N : Ada_Node'Class) return Natural
   is (Natural (First_Column_Number (N)) - 1);
   --  Necessary padding for current element

   function Image (L, R : Token_Reference; Charset : String) return String
   is (Encode (Text (L, R), Charset));
   --  Returns Image of given slice

   function Inspect_Spec (Node : Ada_Node'Class) return Visit_Status;
   --  Collects info on subprograms that need to be instrumented

   procedure Process_Package_Body (Decl : Basic_Decl);
   --  Generates instrumented package body

   procedure Generate_Package_Body (Name : Wide_Wide_String);
   --  Generates body for package that originally did not require it,
   --  but does so after spec instrumentation.

   procedure Process_Package_Spec (Decl : Basic_Decl);
   --  Generates instrumented package spec

   procedure Process_Subprogram_Body (Decl : Base_Subp_Body);
   --  Generates instrumented subprogran body

   procedure Process_Nesting_Difference
     (N1, N2 : Langkit_Support.Text.Text_Type);
   --  Puts beginnings and ends of declarations expected between N1 and N2

   function From_Same_Unit (L, R : Ada_Node'Class) return Boolean
   is (not L.Is_Null
       and then not R.Is_Null
       and then L.P_Enclosing_Compilation_Unit
                = R.P_Enclosing_Compilation_Unit);

   Included_Subps : String_Ordered_Set;

   Remove_Expr_Func_Declarations : String_Ordered_Set;
   --  Expression functions specified in private part of package spec, which
   --  act as bodies for functions declared in the same spec. No special
   --  handling needed for corresponding specs, we just need to remove
   --  the private expression function "bodies".

   Modify_Spec : Boolean;
   --  Whether current spec needs to be overridden

   Subprograms_To_Body : Ada_Nodes_List.List;
   --  List of expression functions that need to receive corresponding
   --  body wrappers

   package Ada_Node_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Ada_Node,
        Hash                => Hash,
        Equivalent_Elements => Libadalang.Analysis."=");

   Bodyless_Specs : Ada_Node_Sets.Set;
   --  List of nested package specs that contain expression functions
   --  and do not require a body in the original source. Only "leaf" specs
   --  are stored.

   package Nesting_Cursors is new
     Ada.Containers.Indefinite_Holders (Langkit_Support.Text.Text_Type);
   subtype Nesting_Cursor is Nesting_Cursors.Holder;

   ------------------
   -- Inspect_Spec --
   ------------------

   function Inspect_Spec (Node : Ada_Node'Class) return Visit_Status is
      Diags : String_Vector;
   begin
      if Kind (Node) = Ada_Package_Decl then

         --  Test instrumentation causes side effects, making pure packages
         --  impossible to instrument.

         if Test.Common.Belongs_To_Pure_Package (Node.As_Basic_Decl) then
            Report_Err
              ("instrumentation of pure package "
               & Image (Node.As_Basic_Decl.P_Defining_Name.Text)
               & " is not supported");
            return Over;
         end if;
         return Into;
      elsif Kind (Node)
            in Ada_Single_Protected_Decl
             | Ada_Protected_Type_Decl
             | Ada_Single_Task_Decl
             | Ada_Task_Type_Decl
      then
         return Over;
      end if;

      if Kind (Node) in Ada_Subp_Decl | Ada_Expr_Function then

         if Kind (Node) = Ada_Expr_Function then
            Modify_Spec := True;

            if From_Same_Unit (Node.As_Expr_Function.P_Decl_Part, Node) then
               Remove_Expr_Func_Declarations.Include (Node.Image);
               return Over;
            end if;

         end if;

         if not TGen.Libgen.Include_Subp
                  (TGen_Libgen_Ctx, Node.As_Basic_Decl, Diags)
         then
            Report_Std (Join (Diags) & ASCII.LF);
            return Over;
         end if;

         Included_Subps.Include
           (Image (Node.As_Basic_Decl.P_Unique_Identifying_Name));

         return Over;
      end if;

      --  Do not look into generic package: we only care about generic
      --  instantiations.

      if Kind (Node) in Ada_Generic_Decl then
         return Over;
      end if;

      return Into;
   end Inspect_Spec;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Analysis_Unit) is
      F_Name    : constant String := The_Unit.Get_Filename;
      CU        : Compilation_Unit;
      Unit      : Basic_Decl;
      Spec_Unit : Basic_Decl;

      Instr_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Prev_Instr_File : Ada.Strings.Unbounded.Unbounded_String;

      procedure Error_Cleanup;
      --  Deletes usuccessfull attempt to instrument given source

      procedure Error_Cleanup is
         Success     : Boolean;
         Delete_File : constant String :=
           Ada.Strings.Unbounded.To_String (Instr_File_Name);
         use Char_Sequential_IO;
         use Ada.Strings.Unbounded;
      begin
         if Is_Open (Output_File) then
            Close (Output_File);
         end if;
         GNAT.OS_Lib.Delete_File (Delete_File, Success);
         if not Success then
            Cmd_Error_No_Help ("cannot delete " & Delete_File);
         end if;

         if Prev_Instr_File /= Null_Unbounded_String then
            declare
               Delete_Other_File : constant String :=
                 To_String (Prev_Instr_File);
            begin
               GNAT.OS_Lib.Delete_File (Delete_Other_File, Success);
               if not Success then
                  Cmd_Error_No_Help ("cannot delete " & Delete_Other_File);
               end if;
            end;
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

      Trace (Me, "inspecting " & F_Name);

      Instr_File_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Get_Source_Instr_Dir (CU.Unit.Get_Filename)
           & GNAT.OS_Lib.Directory_Separator
           & Simple_Name (CU.Unit.Get_Filename));

      Unit := CU.F_Body.As_Library_Item.F_Item;
      Included_Subps.Clear;
      Remove_Expr_Func_Declarations.Clear;
      Subprograms_To_Body.Clear;
      Bodyless_Specs.Clear;
      Modify_Spec := False;
      Traverse (Unit, Inspect_Spec'Access);

      if Modify_Spec then
         Trace (Me, "instrumenting " & F_Name);

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

         Process_Package_Spec (Unit);

         Close_File;
      end if;

      Spec_Unit := Unit;

      if not Included_Subps.Is_Empty then

         if Get_Source_Body (F_Name) /= "" then
            CU := CU.P_Other_Part;
            Trace (Me, "instrumenting " & CU.Unit.Get_Filename);
         else
            Trace
              (Me,
               "creating instrumented body for non-existing "
               & Get_Source_Instr_Body (F_Name));
         end if;

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

         --  Save spec file name in case there is an unexpected error while
         --  instrumenting the body, if that happens we need to clean up both
         --  files to keep test driver compilable.
         Prev_Instr_File := Instr_File_Name;

         if Get_Source_Body (F_Name) = "" then
            Instr_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Get_Source_Instr_Dir (CU.Unit.Get_Filename)
                 & GNAT.OS_Lib.Directory_Separator
                 & Simple_Name (Get_Source_Instr_Body (F_Name)));
         else
            Instr_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (Get_Source_Instr_Dir (CU.Unit.Get_Filename)
                 & GNAT.OS_Lib.Directory_Separator
                 & Simple_Name (CU.Unit.Get_Filename));
         end if;
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

         for U of
           TGen.Libgen.Required_Support_Packages
             (TGen_Libgen_Ctx,
              TGen.LAL_Utils.Convert_Qualified_Name
                (CU.P_Syntactic_Fully_Qualified_Name))
         loop
            S_Put (0, "with " & To_Ada (U) & ";");
            Put_New_Line;
         end loop;

         if Get_Source_Body (F_Name) = "" then
            Generate_Package_Body
              (Spec_Unit.As_Basic_Decl.P_Unique_Identifying_Name);
         else
            Process_Package_Body (Unit);
         end if;

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
      N_Cursor : Nesting_Cursor;

      Artificial_Body_Created : Boolean := False;

      function First_Parent_With_Body (BDS : Ada_Node) return Basic_Decl;
      --  Looks for first enclosing package that has a body

      ----------------------------
      -- First_Parent_With_Body --
      ----------------------------

      function First_Parent_With_Body (BDS : Ada_Node) return Basic_Decl is
         Par : Ada_Node := BDS.P_Semantic_Parent;
      begin
         while not Par.Is_Null loop
            if not Par.As_Package_Decl.P_Body_Part.Is_Null then
               return Par.As_Basic_Decl;
            end if;
            Par := Par.P_Semantic_Parent;
         end loop;

         return No_Basic_Decl;
      end First_Parent_With_Body;

   begin
      S_Put
        (Padding (Decl),
         Image
           (Decl.Token_Start,
            Previous (Decl.As_Package_Body.F_Decls.First_Child.Token_Start),
            Decl.Unit.Get_Charset));
      Put_New_Line;

      --  Insert artificial bodies for expression functions

      for D of Subprograms_To_Body loop
         if D.P_Parent_Basic_Decl.P_Unique_Identifying_Name
           = Decl.P_Unique_Identifying_Name
         then
            Process_Subprogram_Body (D.As_Base_Subp_Body);
         end if;
      end loop;

      --  Insert nested bodies for expression functions
      N_Cursor.Replace_Element (Decl.P_Unique_Identifying_Name);

      for BDS of Bodyless_Specs loop
         if First_Parent_With_Body (BDS).P_Unique_Identifying_Name
           = Decl.P_Unique_Identifying_Name
         then
            Artificial_Body_Created := True;

            Process_Nesting_Difference
              (N_Cursor.Element, BDS.As_Basic_Decl.P_Unique_Identifying_Name);

            for D of Subprograms_To_Body loop
               if D.P_Parent_Basic_Decl.P_Unique_Identifying_Name
                 = BDS.As_Basic_Decl.P_Unique_Identifying_Name
               then
                  Process_Subprogram_Body (D.As_Base_Subp_Body);
               end if;
            end loop;

            N_Cursor.Replace_Element
              (BDS.As_Basic_Decl.P_Unique_Identifying_Name);

         end if;
      end loop;

      if Artificial_Body_Created then
         Process_Nesting_Difference
           (N_Cursor.Element, Decl.P_Unique_Identifying_Name);
      end if;

      for D of Decl.As_Package_Body.F_Decls.F_Decls loop

         if D.Kind = Ada_Package_Body then
            if From_Same_Unit (D.As_Package_Body.P_Decl_Part, D) then
               --  Corresponding spec not visible to outside world,
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

            if not From_Same_Unit (D.As_Subp_Body.P_Decl_Part, D)
              and then D.As_Subp_Body.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                         (Image (D.As_Subp_Body.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         elsif D.Kind in Ada_Expr_Function then

            if not From_Same_Unit (D.As_Expr_Function.P_Decl_Part, D)
              and then D.As_Expr_Function.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                         (Image (D.As_Expr_Function.P_Unique_Identifying_Name))
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            else
               S_Put (Padding (D), Node_Image (D));
            end if;
            Put_New_Line;

         elsif D.Kind in Ada_Null_Subp_Decl then

            if not From_Same_Unit (D.As_Null_Subp_Decl.P_Decl_Part, D)
              and then D.As_Null_Subp_Decl.F_Subp_Spec.P_Params'Length > 0
              and then Included_Subps.Contains
                         (Image
                            (D.As_Null_Subp_Decl.P_Unique_Identifying_Name))
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
                         (Image
                            (D
                               .As_Subp_Renaming_Decl
                               .P_Unique_Identifying_Name))
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
           (Next (Decl.As_Package_Body.F_Decls.Last_Child.Token_End),
            Decl.Token_End,
            Decl.Unit.Get_Charset));
      Put_New_Line;
   end Process_Package_Body;

   ---------------------------
   -- Generate_Package_Body --
   ---------------------------

   procedure Generate_Package_Body (Name : Wide_Wide_String) is
      use Ada.Characters.Conversions;

      N_Cursor : Nesting_Cursor;

      Artificial_Body_Created : Boolean := False;
   begin
      S_Put (0, "package body " & To_String (Name) & " is");
      Put_New_Line;

      --  Insert bodies for expression functions
      N_Cursor.Replace_Element (Name);

      for BDS of Bodyless_Specs loop

         Artificial_Body_Created := True;

         Process_Nesting_Difference
           (N_Cursor.Element, BDS.As_Basic_Decl.P_Unique_Identifying_Name);

         for D of Subprograms_To_Body loop
            if D.P_Parent_Basic_Decl.P_Unique_Identifying_Name
              = BDS.As_Basic_Decl.P_Unique_Identifying_Name
            then
               Process_Subprogram_Body (D.As_Base_Subp_Body);
            end if;
         end loop;

         N_Cursor.Replace_Element
           (BDS.As_Basic_Decl.P_Unique_Identifying_Name);

      end loop;

      if Artificial_Body_Created then
         Process_Nesting_Difference (N_Cursor.Element, Name);
      end if;

      S_Put (0, "end " & To_String (Name) & ";");
      Put_New_Line;
   end Generate_Package_Body;

   --------------------------
   -- Process_Package_Spec --
   --------------------------

   procedure Process_Package_Spec (Decl : Basic_Decl) is

      type Declarations_Visibility is (Public_Decls, Private_Decls);
      --  Specify whether declarations are part of the `public` or `private`
      --  part of the given package spec.

      procedure Process_Declarations
        (Decls : Ada_Node_List; Visibility : Declarations_Visibility);
      --  Processes declarations of the package spec

      --------------------------
      -- Process_Declarations --
      --------------------------

      procedure Process_Declarations
        (Decls : Ada_Node_List; Visibility : Declarations_Visibility)
      is
         procedure Register_Bodyless_Spec (Decl : Ada_Node);
         --  Add the package declaration that holds the current declaration
         --  node to Bodyless_Specs.

         procedure Register_Bodyless_Spec (Decl : Ada_Node) is
            --  The semantic parent of a subprogram declaration isn't
            --  necessarily a package declaration. In the case of declarations
            --  in the private part of a package, the semantic parent is
            --  `PrivatePart`. In order to retrieve the corresponding package
            --  declaration, declarations that are part of a public and private
            --  part of a package should be handled individually.

            Semantic_Parent : constant Ada_Node := Decl.P_Semantic_Parent;
         begin
            case Visibility is
               when Public_Decls =>
                  if Semantic_Parent.As_Package_Decl.P_Body_Part.Is_Null then
                     Bodyless_Specs.Include (Semantic_Parent.As_Ada_Node);
                  end if;

               when Private_Decls =>
                  if Semantic_Parent
                       .As_Private_Part
                       .P_Semantic_Parent
                       .As_Package_Decl
                       .P_Body_Part
                       .Is_Null
                  then
                     Bodyless_Specs.Include
                       (Semantic_Parent.As_Private_Part.P_Semantic_Parent);
                  end if;
            end case;
         end Register_Bodyless_Spec;

      begin
         for D of Decls loop
            case D.Kind is
               when Ada_Package_Decl =>
                  Process_Package_Spec (D.As_Basic_Decl);

               when Ada_Expr_Function =>
                  --  If D is an expression function, add it to the list of
                  --  subprograms to write to the body, and register the spec
                  --  as bodyless to create a body if necessary.

                  Subprograms_To_Body.Append (D.As_Ada_Node);
                  Register_Bodyless_Spec (D.As_Ada_Node);

                  if not Remove_Expr_Func_Declarations.Contains (D.Image) then
                     --  In case D is not in Remove_Expr_Func_Declarations, it
                     --  means that the expression function has no prior
                     --  declaration, so we omit the "expression" part and just
                     --  write the declaration to the instrumented spec.

                     S_Put
                       (Padding (D),
                        Node_Image (D.As_Expr_Function.F_Subp_Spec)
                        & (if D.As_Expr_Function.F_Aspects.Is_Null
                           then ";"
                           else
                             " "
                             & Node_Image (D.As_Expr_Function.F_Aspects)
                             & ";"));
                     Put_New_Line;
                  end if;

               when others =>
                  --  In all other cases, just copy the declaration as is to
                  --  the instrumented spec.

                  S_Put (Padding (D), Node_Image (D));
                  Put_New_Line;
            end case;
         end loop;
      end Process_Declarations;
   begin
      S_Put
        (Padding (Decl),
         Image
           (Decl.Token_Start,
            Previous
              (Decl
                 .As_Package_Decl
                 .F_Public_Part
                 .F_Decls
                 .First_Child
                 .Token_Start),
            Decl.Unit.Get_Charset));
      Put_New_Line;

      Process_Declarations
        (Decl.As_Package_Decl.F_Public_Part.F_Decls, Public_Decls);
      Put_New_Line;

      if Decl.As_Package_Decl.F_Private_Part.Is_Null then
         S_Put
           (Padding (Decl),
            Image
              (Next (Decl.As_Package_Decl.F_Public_Part.Token_End),
               Decl.Token_End,
               Decl.Unit.Get_Charset));
         Put_New_Line;
      else
         S_Put (Padding (Decl), "private");
         Put_New_Line;
         Process_Declarations
           (Decl.As_Package_Decl.F_Private_Part.F_Decls, Private_Decls);
         S_Put
           (Padding (Decl),
            Image
              (Next (Decl.As_Package_Decl.F_Private_Part.Token_End),
               Decl.Token_End,
               Decl.Unit.Get_Charset));
      end if;

   end Process_Package_Spec;

   -----------------------------
   -- Process_Subprogram_Body --
   -----------------------------

   procedure Process_Subprogram_Body (Decl : Base_Subp_Body) is
      Pad : constant Natural := Padding (Decl);

      Decl_Decl : constant Basic_Decl :=
        (if Decl.P_Decl_Part.Is_Null
         then Decl.As_Basic_Decl
         else Decl.P_Decl_Part);

      Subp_Wrapper_Name : Ada.Strings.Unbounded.Unbounded_String;

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
         raise Instrumentation_Error
           with "unsupported body kind: " & Decl.Image;
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
         & Mangle_Hash_Full (Decl_Decl)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "or else TGen.Instr_Support.Nesting_Hash /= """
         & GNAT.SHA1.Digest (Get_Nesting (Decl_Decl))
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
      S_Put (Pad + 6, "if TGen.Instr_Support.Recursion_Depth > 1 then");
      Put_New_Line;
      S_Put (Pad + 9, "return True;");
      Put_New_Line;
      S_Put (Pad + 6, "end if;");
      Put_New_Line;
      Put_New_Line;

      S_Put
        (Pad + 6,
         "Create (GNATTEST_F, Out_File, TGen.Instr_Support.Output_Dir"
         & " & """
         & TGen.LAL_Utils.Default_Blob_Test_Filename (Decl_Decl)
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
            Param_Typ        : constant TGen.Types.Typ_Access :=
              TGen.Types.Translation.Translate (Param.F_Type_Expr).Res;
            Support_Pkg_Name : constant String :=
              (if Param_Typ.Fully_Private
               then ".TGen_Support_Private."
               else ".TGen_Support.");
         begin

            if Param_Typ.all.Package_Name = To_Qualified_Name ("standard") then
               for Name of Param.F_Ids loop
                  S_Put
                    (Pad + 6,
                     "TGen.TGen_Support."
                     & TGen.Marshalling.Output_Fname_For_Typ
                         (Param_Typ.all.Name)
                     & " (GNATTEST_S, "
                     & Node_Image (Name)
                     & ");");
                  Put_New_Line;
               end loop;
            else
               for Name of Param.F_Ids loop
                  S_Put
                    (Pad + 6,
                     To_Ada (Param_Typ.all.Package_Name)
                     & Support_Pkg_Name
                     & TGen.Marshalling.Output_Fname_For_Typ
                         (Param_Typ.all.Name)
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
      declare
         use Ada.Strings.Unbounded;
      begin
         if Decl.F_Subp_Spec.F_Subp_Name.F_Name.Kind in Ada_String_Literal then
            Subp_Wrapper_Name :=
              To_Unbounded_String
                (Map_Operator_Name
                   (Node_Image (Decl.F_Subp_Spec.F_Subp_Name)));
         else
            Subp_Wrapper_Name :=
              To_Unbounded_String (Node_Image (Decl.F_Subp_Spec.F_Subp_Name));
         end if;
         Subp_Wrapper_Name := Subp_Wrapper_Name & "_GNATTEST";
      end;

      S_Put
        (Pad + 3,
         Image
           (Decl.Token_Start,
            Previous (Decl.F_Subp_Spec.F_Subp_Name.Token_Start),
            Decl.Unit.Get_Charset)
         & Ada.Strings.Unbounded.To_String (Subp_Wrapper_Name)
         & Image
             (Next (Decl.F_Subp_Spec.F_Subp_Name.Token_End),
              Decl.F_Subp_Spec.Token_End,
              Decl.Unit.Get_Charset));

      if Decl.Kind = Ada_Subp_Body then
         Put_New_Line;
         S_Put (Pad, "is");
         Put_New_Line;
         S_Put
           (Pad + 3,
            Image
              (Decl.As_Subp_Body.F_Decls.Token_Start,
               Decl.As_Subp_Body.F_Stmts.Token_End,
               Decl.Unit.Get_Charset));
      elsif Decl.Kind = Ada_Expr_Function then
         Put_New_Line;
         S_Put (Pad, "is");
         Put_New_Line;
         S_Put (Pad, "begin");
         Put_New_Line;
         S_Put
           (Pad + 3,
            "return " & Node_Image (Decl.As_Expr_Function.F_Expr) & ";");
         Put_New_Line;
      elsif Decl.Kind = Ada_Subp_Renaming_Decl then
         S_Put (1, Node_Image (Decl.As_Subp_Renaming_Decl.F_Renames) & ";");
         Put_New_Line;
      elsif Decl.Kind = Ada_Null_Subp_Decl then
         S_Put (1, "is null;");
         Put_New_Line;
      else
         raise Instrumentation_Error
           with "unsupported body kind: " & Decl.Image;
      end if;

      if Decl.Kind not in Ada_Subp_Renaming_Decl | Ada_Null_Subp_Decl then
         Put_New_Line;
         S_Put
           (Pad + 3,
            "end "
            & Ada.Strings.Unbounded.To_String (Subp_Wrapper_Name)
            & ";");
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
            & Ada.Strings.Unbounded.To_String (Subp_Wrapper_Name));
      else
         S_Put (Pad + 3, Ada.Strings.Unbounded.To_String (Subp_Wrapper_Name));
      end if;

      declare
         First_Param : Boolean := True;
      begin
         for Param of Decl.F_Subp_Spec.P_Params loop
            for Name of Param.F_Ids loop
               if First_Param then
                  S_Put (0, " (");
                  First_Param := False;
                  Put_New_Line;
               else
                  S_Put (0, ",");
                  Put_New_Line;
               end if;
               S_Put (Pad + 5, Node_Image (Name));
            end loop;
         end loop;

         --  First_Param can only be True if there are no parameters

         if not First_Param then
            S_Put (0, ")");
         end if;
      end;
      if Decl.F_Subp_Spec.F_Subp_Kind = Ada_Subp_Kind_Function then
         S_Put (0, " do");
      else
         S_Put (0, ";");
      end if;

      Put_New_Line;
      Put_New_Line;
      S_Put
        (Pad + 6,
         "if TGen.Instr_Support.Subp_Hash = """
         & Mangle_Hash_Full (Decl_Decl)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "and then TGen.Instr_Support.Nesting_Hash = """
         & GNAT.SHA1.Digest (Get_Nesting (Decl_Decl))
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
         & Mangle_Hash_Full (Decl_Decl)
         & """");
      Put_New_Line;
      S_Put
        (Pad + 8,
         "and then TGen.Instr_Support.Nesting_Hash = """
         & GNAT.SHA1.Digest (Get_Nesting (Decl_Decl))
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

      S_Put (Pad, "end " & Node_Image (Decl.F_Subp_Spec.F_Subp_Name) & ";");
      Put_New_Line;
      Put_New_Line;

   end Process_Subprogram_Body;

   --------------------------------
   -- Process_Nesting_Difference --
   --------------------------------

   procedure Process_Nesting_Difference
     (N1, N2 : Langkit_Support.Text.Text_Type)
   is
      use Ada.Characters.Conversions;
      use Ada.Strings.Wide_Wide_Fixed;

      Idx : Natural := 0;

      Pad      : Natural;
      Pad_Step : Integer := 0;
      --  Strictly speaking padding is not needed but helps
      --  inspecting generated code and doesn't cost much.

      function Add_Pad return Natural
      is (Pad + Pad_Step * 3);
      --  Padding for nested declarations

      procedure Open_Decls (N1, N2 : Langkit_Support.Text.Text_Type);
      --  Puts beginnings of package declarations

      procedure Close_Decls (N1, N2 : Langkit_Support.Text.Text_Type);
      --  Puts ends of package declarations

      ----------------
      -- Open_Decls --
      ----------------

      procedure Open_Decls (N1, N2 : Langkit_Support.Text.Text_Type) is
      begin
         Pad := 0;
         for J in N1'Range loop
            if N1 (J) = '.' then
               Pad := Pad + 3;
            end if;
         end loop;

         declare
            Dif : constant Wide_Wide_String :=
              Tail (N2, N2'Length - N1'Length);
         begin
            Idx := Dif'First + 1;
            for J in Dif'First + 1 .. Dif'Last loop
               if Dif (J) = '.' then
                  Pad_Step := Pad_Step + 1;
                  S_Put
                    (Add_Pad,
                     "package body " & To_String (Dif (Idx .. J - 1)) & " is");
                  Put_New_Line;
                  Idx := J + 1;
               elsif J = Dif'Last then
                  Pad_Step := Pad_Step + 1;
                  S_Put
                    (Add_Pad,
                     "package body " & To_String (Dif (Idx .. J)) & " is");
                  Put_New_Line;
               end if;
            end loop;
         end;
      end Open_Decls;

      -----------------
      -- Close_Decls --
      -----------------

      procedure Close_Decls (N1, N2 : Langkit_Support.Text.Text_Type) is
      begin
         Pad := 0;
         for J in N1'Range loop
            if N1 (J) = '.' then
               Pad := Pad + 3;
            end if;
         end loop;

         declare
            Dif : constant Wide_Wide_String :=
              Tail (N1, N1'Length - N2'Length);
         begin
            Idx := Dif'Last;
            for J in reverse Dif'First + 1 .. Dif'Last loop
               if Dif (J) = '.' then
                  S_Put
                    (Add_Pad, "end " & To_String (Dif (J + 1 .. Idx)) & ";");
                  Put_New_Line;
                  Pad_Step := Pad_Step - 1;
                  Idx := J - 1;
               elsif J = Dif'First + 1 then
                  S_Put (Add_Pad, "end " & To_String (Dif (J .. Idx)) & ";");
                  Put_New_Line;
               end if;
            end loop;
         end;
      end Close_Decls;

   begin

      --  Nothing to do if N1 and N2 are the same

      if N1 = N2 then
         return;

      elsif N1'Length < N2'Length and then N1 = Head (N2, N1'Length) then
         --  Need to start package body declarations
         Open_Decls (N1, N2);

      elsif N2'Length < N1'Length and then N2 = Head (N1, N2'Length) then
         --  Need to end package body declarations.
         Close_Decls (N1, N2);

      else
         --  First need to close some declarations, then open others

         declare
            Has_Dot : Boolean := False;
         begin
            --  Getting common root package
            for J in 0 .. Integer'Min (N1'Length, N2'Length) - 1 loop
               if N1 (N1'First + J) /= N2 (N2'First + J) then
                  exit;
               elsif N1 (N1'First + J) = '.' then
                  Idx := J - 1;
                  Has_Dot := True;
               end if;
            end loop;

            declare
               N_Common : constant Wide_Wide_String :=
                 (if Has_Dot
                  then N1 (N1'First .. N1'First + Idx)
                  --  N1 and N2 are the same at this point. We reached the
                  --  end of the string without encountering a dot.
                  else N1);
            begin
               if not Has_Dot then
                  pragma Assert (N1 = N2);
               end if;

               Close_Decls (N1, N_Common);
               Put_New_Line;
               Open_Decls (N_Common, N2);
            end;
         end;
      end if;
   end Process_Nesting_Difference;

end Test.Instrument;
