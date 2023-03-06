------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Characters.Latin_9;
with Ada.Characters;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Langkit_Support.Text; use Langkit_Support.Text;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with TGen.Big_Int; use TGen.Big_Int;
with TGen.Files;                use TGen.Files;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.LAL_Utils;            use TGen.LAL_Utils;
with TGen.Marshalling;          use TGen.Marshalling;
with TGen.Strategies;
with TGen.Types.Translation;    use TGen.Types.Translation;

package body TGen.Gen_Strategies is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Context    : in out Generation_Context;
      Output_Dir : Unbounded_String) is
   begin
      Context.Output_Dir := Output_Dir;
      Prepare_Output_Dirs (Context);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Generate_Artifacts (Context : in out Generation_Context) is
   begin
      if not Context.Codegen_Required then
         --  Fully static test value generation is ON

         Dump_JSON (Context);
         return;
      end if;

      for Package_Data of Context.Packages_Data loop
         null;
      end loop;
      --  Generate_Type_Strategies (Context);
   end Generate_Artifacts;

   ----------------------------------
   -- Distinct_Type_Parent_Package --
   ----------------------------------

   function Distinct_Type_Parent_Package
     (Parameters_Data : Parameters_Data_Vector)
         return String_Ordered_Set
   is
      S : String_Ordered_Set;

   begin
      for Data of Parameters_Data loop
         if not S.Contains
           (To_UTF8 (To_Text (Data.Type_Parent_Package)))
         then
            S.Insert (To_UTF8 (To_Text (Data.Type_Parent_Package)));
         end if;
      end loop;

      return S;
   end Distinct_Type_Parent_Package;

   function Distinct_Type_Parent_Package
     (Subprogram_Data : Subprograms_Data_Vector)
      return String_Ordered_Set
   is
      S : String_Ordered_Set;
   begin
      for Subprogram of Subprogram_Data loop
         S.Union (Distinct_Type_Parent_Package (Subprogram.Parameters_Data));
      end loop;
      return S;
   end Distinct_Type_Parent_Package;

   ------------
   -- Indent --
   ------------

   function Indent (Amount : Natural; Str : String) return String is
      Res    : Unbounded_String;
      Indent : constant String (1 .. Amount) := [others => ' '];
   begin
      Append (Res, Indent);
      for C of Str loop
         Append (Res, C);
         if C = Ada.Characters.Latin_9.LF then
            Append (Res, Indent);
         end if;
      end loop;
      return +Res;
   end Indent;

   ---------------------
   -- Number_Of_Lines --
   ---------------------

   function Number_Of_Lines (Str : String) return Natural is
   begin
      return Ada.Strings.Fixed.Count
        (Str,
         Ada.Strings.Maps.To_Set
           (Ada.Characters.Latin_9.LF)) + 1;
   end Number_Of_Lines;

   procedure Collect_Type_Translations (Subp : Basic_Decl'Class);

   -------------------------------
   -- Collect_Type_Translations --
   -------------------------------

   procedure Collect_Type_Translations (Subp : Basic_Decl'Class)
   is
      Subp_Translation_Res : constant Translation_Result :=
        Translate (Subp.P_Subp_Spec_Or_Null.As_Subp_Spec);
   begin
      if not Subp_Translation_Res.Success then
         raise Program_Error with To_String (Subp_Translation_Res.Diagnostics);
      end if;
   end Collect_Type_Translations;

   ---------------
   -- Dump_JSON --
   ---------------

   procedure Dump_JSON
     (Context : Generation_Context) is
      use Unit_To_JSON_Maps;
   begin
      for Unit_JSON_Cursor in Context.Test_Vectors.Iterate loop
         declare
            File : constant Virtual_File :=
              Get_JSON_Name (Context, +Key (Unit_JSON_Cursor));
            JSON_Unit_Writable_File : Writable_File :=
              Write_File (File);
         begin
            Write
              (JSON_Unit_Writable_File,
               Write (Create (Element (Unit_JSON_Cursor))));
            Close (JSON_Unit_Writable_File);
         end;
      end loop;
   end Dump_JSON;

   ---------------------------
   -- Generate_Test_Vectors --
   ---------------------------

   procedure Generate_Test_Vectors
     (Context  : in out Generation_Context;
      Nb_Tests : Positive;
      Subp     : Basic_Decl'Class;
      Subp_UID : Unbounded_String := Null_Unbounded_String)
   is
      Subp_Data : Subprogram_Data :=
        Extract_Subprogram_Data (Subp);
      Function_JSON     : constant JSON_Value := Create_Object;
      Test_Vectors_JSON : JSON_Array := Empty_Array;
      Test_Vector_JSON  : JSON_Array;

      function Escape (Input_String : String) return String;
      --  Escape every double quote inside Input_String

      ------------
      -- Escape --
      ------------

      function Escape (Input_String : String) return String is
         Result : Unbounded_String;
      begin
         for C of Input_String loop
            if C = '"' then
               Result.Append ("""");
            end if;
            Result.Append (C);
         end loop;
         return +Result;
      end Escape;

   begin
      Collect_Type_Translations (Subp);
      Subp_Data.All_Params_Static := True;
      for Param of Subp_Data.Parameters_Data loop
         if Param.Mode in In_Mode | In_Out_Mode then
            declare
               Ignore_P_Qualified_Name : constant Ada_Qualified_Name :=
                 To_Qualified_Name (+Param.Type_Fully_Qualified_Name);
               Param_Type : constant SP.Ref :=
                 TGen.Types.Translation.Translation_Cache.Element
                   (To_Qualified_Name (+Param.Type_Fully_Qualified_Name));
            begin
               Subp_Data.All_Params_Static :=
                 Subp_Data.All_Params_Static
                 and then Param_Type.Get.Supports_Static_Gen;
               Subp_Data.Some_Param_Static :=
                 Subp_Data.Some_Param_Static
                 or else Param_Type.Get.Supports_Static_Gen;
               if not Context.Type_And_Param_Strategies.Contains
                 (Param.Type_Fully_Qualified_Name)
               then
                  Context.Type_And_Param_Strategies.Insert
                  (Param.Type_Fully_Qualified_Name,
                     Try_Generate_Static (Param_Type));
               end if;
            end;
         end if;
      end loop;

      --  Do not generate any JSON if none of the types are supported.

      if not Subp_Data.Some_Param_Static then
         return;
      end if;

      for J in 1 .. Nb_Tests loop
         Test_Vector_JSON := Empty_Array;
         for Param of Subp_Data.Parameters_Data loop
            declare
               Param_JSON : constant JSON_Value := Create_Object;
               Param_Type : constant SP.Ref :=
                 TGen.Types.Translation.Translation_Cache.Element
                   (To_Qualified_Name (+Param.Type_Fully_Qualified_Name));
            begin
               Param_JSON.Set_Field ("name", Create (+Param.Name));
               Param_JSON.Set_Field ("type_name", Create (+Param.Type_Name));
               if Param.Mode in In_Mode | In_Out_Mode then
                  declare
                     Strat : Strategies.Strategy_Type'Class :=
                        Strategies.Strategy_Type
                          (Context.Type_And_Param_Strategies.Element
                             (Param.Type_Fully_Qualified_Name));

                     JSON_String_Var  : constant String :=
                       +Param.Name &  "_JSON";
                     JSON_String_Decl : constant String :=
                       JSON_String_Var & " : constant TGen.JSON.JSON_Value :="
                       & " TGen.JSON.Read ("""
                       & Escape
                         (Param_Type.Get.Encode (Strat.Generate
                            (Strategies.Disc_Value_Maps.Empty_Map)).Write)
                       & """);";

                     Decls : JSON_Array;
                  begin
                     Append (Decls, Create (JSON_String_Decl));
                     Param_JSON.Set_Field ("decls", Decls);

                     Param_JSON.Set_Field
                       ("value",
                        Input_Fname_For_Typ (Param_Type.Get)
                        & "(" & JSON_String_Var & ")");
                  end;
               end if;
               Param_JSON.Set_Field
                 ("mode",
                  Create
                    (Big_Int.To_Big_Integer
                       (Parameter_Mode'Pos (Param.Mode))));
               Append (Test_Vector_JSON, Param_JSON);
            end;
         end loop;
         if not Is_Empty (Test_Vector_JSON) then
            Append (Test_Vectors_JSON, Create (Test_Vector_JSON));
         end if;
      end loop;

      Function_JSON.Set_Field
        ("fully_qualified_name", +Subp_Data.Fully_Qualified_Name);
      Function_JSON.Set_Field ("package_name", +Subp_Data.Parent_Package);
      Function_JSON.Set_Field ("UID", +Subp_UID);
      Function_JSON.Set_Field
        ("generation_complete", Create (Subp_Data.All_Params_Static));
      if not Is_Null (Subp.P_Subp_Spec_Or_Null.P_Returns) then
         Function_JSON.Set_Field
           ("return_type",
            Create (+Subp_Data.Return_Type_Fully_Qualified_Name));
      end if;
      Function_JSON.Set_Field ("values", Test_Vectors_JSON);

      if not Context.Test_Vectors.Contains (Subp_Data.Parent_Package) then
         Context.Test_Vectors.Insert (Subp_Data.Parent_Package, Empty_Array);
      end if;

      declare

         procedure Add_Function_Testing
           (Unit_Name  : Unbounded_Text_Type;
            Unit_Tests : in out JSON_Array);

         procedure Add_Function_Testing
           (Unit_Name  : Unbounded_Text_Type;
            Unit_Tests : in out JSON_Array) is
         begin
            pragma Unreferenced (Unit_Name);
            Append (Unit_Tests, Function_JSON);
         end Add_Function_Testing;

      begin
         Context.Test_Vectors.Update_Element
           (Context.Test_Vectors.Find (Subp_Data.Parent_Package),
            Add_Function_Testing'Access);
      end;
   end Generate_Test_Vectors;

end TGen.Gen_Strategies;
