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
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Libadalang.Common; use Libadalang.Common;

with Templates_Parser; use Templates_Parser;

with TGen.Gen_Types_Value;   use TGen.Gen_Types_Value;
with TGen.Files;             use TGen.Files;
with TGen.Strategies;        use TGen.Strategies;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Types.Translation; use TGen.Types.Translation;

package body TGen.Gen_Strategies is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Context    : in out Generation_Context;
      Project    : Project_Type;
      Output_Dir : Unbounded_String) is
   begin
      Context.Output_Dir := Output_Dir;
      Context.Project := Project;
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

   ------------------------------
   -- Generate_Type_Strategies --
   ------------------------------

   procedure Generate_Type_Strategies
     (Context : Generation_Context) is
      package Strategies_Per_Package_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_Text_Type,
         Element_Type => Strategy_Set,
         "="          => Strategy_Sets."=");
      subtype Strategies_Per_Package_Map is Strategies_Per_Package_Maps.Map;

      Strategies_Per_Package : Strategies_Per_Package_Map;

   begin

      --  Let's order strategies per package

      for Strat of Context.Strategies loop
         declare
            procedure Update_Element
              (Ignored    : Unbounded_Text_Type;
               Strategies : in out Strategy_Set);

            procedure Update_Element
              (Ignored    : Unbounded_Text_Type;
               Strategies : in out Strategy_Set) is
            begin
               Strategies.Insert (Strat);
            end Update_Element;
            Pkg_Name : constant Unbounded_Text_Type :=
              +Package_Name (Dynamic_Strategy_Type (Strat));
         begin
            if not Strategies_Per_Package.Contains (Pkg_Name) then
               Strategies_Per_Package.Insert
                 (Pkg_Name, Strategy_Sets.Empty_Set);
            end if;
            Strategies_Per_Package.Update_Element
              (Strategies_Per_Package.Find (Pkg_Name),
               Update_Element'Access);
         end;
      end loop;

      --  Then generate the packages

      for Strats of Strategies_Per_Package loop
         declare
            TSG : constant Type_Value_Generator := Create (Context, Strats);
         begin
            TSG.Generate_Source_Code (Context);
         end;
      end loop;
   end Generate_Type_Strategies;

   package body Test_Generator is

      function Generate_Source_Code
        (Self : Test_Generator;
         Ctx  : TGen.Templates.Context'Class) return Wide_Wide_String
      is
         pragma Unreferenced (Ctx);
         use type Ada.Containers.Count_Type;

         TS : aliased constant Test_Translator :=
           Create_Test_Translator (Self.Subp);

         Proc_Name_Tag : constant String := "PROC_NAME";

         Proc_Qualified_Name_Tag : constant String := "PROC_QUALIFIED_NAME";

         Has_Params_Tag : constant String := "HAS_PARAMS";

         Precondition_Tag : constant String := "PRECONDITION_EXPRESSION";

         Table : Templates_Parser.Translate_Set;

      begin
         TS.Translate (Table);

         --  All the tables are translated. Now translate the individual
         --  tags.

         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Proc_Name_Tag,
               +Self.Subp.Name));

         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Proc_Qualified_Name_Tag,
               +Self.Subp.Fully_Qualified_Name));

         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Has_Params_Tag,
               Self.Subp.Parameters_Data.Length > 0));

         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Precondition_Tag,
               +Self.Subp.Precondition));

         return
           To_Text
             (Templates_Parser.Parse
                (Filename          => +Self.Test_Template_File.Full_Name,
                 Translations      => Table,
                 Cached            => True,
                 Keep_Unknown_Tags => True));
      end Generate_Source_Code;

      function Create
        (Test_Template_File  : GNATCOLL.VFS.Virtual_File;
         Subp                : Subprogram_Data)
         return Test_Generator
      is
      begin
         return Test_Generator'
           (Test_Template_File, Subp);
      end Create;

      function Create_Test_Translator
        (Subp : Subprogram_Data;
         Next  : access constant Translator'Class := null)
         return Test_Translator is
           (Next, Subp);

      procedure Translate_Helper
        (Self  : Test_Translator;
         Table : in out Templates_Parser.Translate_Set)
      is
         Strategy_Name_Tag : constant String := "STRATEGY_NAME";
         Strategy_Name_Vector_Tag : Templates_Parser.Vector_Tag;

         Type_Name_Tag : constant String := "TYPE_NAME";
         Type_Name_Vector_Tag : Templates_Parser.Vector_Tag;

         Gen_Subp_Name_Tag : constant String := "GEN_SUBP_NAME";
         Gen_Subp_Name_Vector_Tag : Templates_Parser.Vector_Tag;

         Param_Name_Tag : constant String := "PARAM_NAME";
         Param_Name_Vector_Tag : Templates_Parser.Vector_Tag;

      begin
         for Param of Self.Subp.Parameters_Data loop
            Templates_Parser.Append
              (Strategy_Name_Vector_Tag,
               Strat_Param_Name (Param));
            Templates_Parser.Append
              (Type_Name_Vector_Tag,
               (+Param.Type_Fully_Qualified_Name));
            Templates_Parser.Append
              (Gen_Subp_Name_Vector_Tag,
               Gen_Param_Full_Function_Name (Self.Subp, Param));
            Templates_Parser.Append
              (Param_Name_Vector_Tag,
               (+Param.Name));
         end loop;

         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Strategy_Name_Tag, Strategy_Name_Vector_Tag));
         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Type_Name_Tag, Type_Name_Vector_Tag));
         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Gen_Subp_Name_Tag, Gen_Subp_Name_Vector_Tag));
         Templates_Parser.Insert
           (Table,
            Templates_Parser.Assoc
              (Param_Name_Tag, Param_Name_Vector_Tag));
      end Translate_Helper;
   end Test_Generator;

   function Get_Parent_Package (Node : Ada_Node) return Package_Decl;

   function Get_Parent_Package (Node : Ada_Node) return Package_Decl is
      Parent_Node : Ada_Node := Node.P_Semantic_Parent;
   begin
      while not Parent_Node.Is_Null
        and then Kind (Parent_Node) /= Ada_Package_Decl
      loop
         Parent_Node := Parent_Node.P_Semantic_Parent;
      end loop;

      return Parent_Node.As_Package_Decl;
   end Get_Parent_Package;

   function Get_With_Clauses (Subp : Subprogram_Data) return String_Sets.Set;

   function Get_With_Clauses (Subp : Subprogram_Data) return String_Sets.Set
   is
      Res : String_Sets.Set;
   begin
      Res.Insert ("TGen.Stream");
      Res.Insert ("TGen.Engine");
      Res.Insert ("TGen.Strategies");
      Res.Insert (+Subp.Parent_Package);
      Res.Insert (Param_Strat_Package_Name (+Subp.Parent_Package));
      return Res;
   end Get_With_Clauses;

   function Indent (Amount : Natural; Str : String) return String is
      Res    : Unbounded_String;
      Indent : constant String (1 .. Amount) := (others => ' ');
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

   procedure Collect_Type_Translations
     (Context : in out Generation_Context;
      Subp    : Subp_Decl);

   -------------------------------
   -- Collect_Type_Translations --
   -------------------------------

   procedure Collect_Type_Translations
     (Context : in out Generation_Context;
      Subp    : Subp_Decl)
   is
      Subp_Params : constant Params := Get_Subp_Params (Subp);
   begin
      if Subp_Params = No_Params then
         return;
      end if;
      for Subp_Param_Spec of Subp_Params.F_Params loop
         declare
            Parameters_Type : constant Defining_Name :=
              Subp_Param_Spec.F_Type_Expr.
                P_Designated_Type_Decl.P_Defining_Name;

            Type_Fully_Qualified_Name : constant Unbounded_Text_Type :=
              To_Unbounded_Text
                (if not Is_Null (Parameters_Type)
                 then Parameters_Type.P_Basic_Decl.P_Fully_Qualified_Name
                 else "");

            Typ_Translation_Res : constant Translation_Result :=
              Translate (Subp_Param_Spec.F_Type_Expr);
            Typ_Translation : SP.Ref;
         begin

            if Typ_Translation_Res.Success then
               Typ_Translation := Typ_Translation_Res.Res;
            else
               raise Program_Error with
                 To_String (Typ_Translation_Res.Diagnostics);
            end if;
            Context.Type_Translations.Include
              (Type_Fully_Qualified_Name, Typ_Translation);
         end;
      end loop;
   end Collect_Type_Translations;

   ------------------------
   -- Generate_Tests_For --
   ------------------------

   function Generate_Tests_For
     (Context  : in out Generation_Context;
      Nb_Tests : Positive;
      Subp     : Subp_Decl) return Generated_Body is

      Pkg      : constant Package_Decl :=
        Get_Parent_Package (Subp.As_Ada_Node);
      Pkg_Data : Package_Data;

      Res : Generated_Body;
   begin
      pragma Unreferenced (Nb_Tests);
      if Pkg.Is_Null then
         raise Program_Error with
           "Unable to find parent package of node "
           & Image (Subp.Full_Sloc_Image);
      end if;

      Pkg_Data.Pkg_Name := Pkg;

      if not Context.Packages_Data.Contains (Pkg_Data) then
         Context.Packages_Data.Insert (Pkg_Data);
      else
         Pkg_Data := Package_Data_Sets.Element
           (Context.Packages_Data.Find (Pkg_Data));
      end if;

      declare
         Subp_Data : constant Subprogram_Data :=
           Extract_Subprogram_Data (Subp);

      begin

         --  Collect the user-defined strategies here. If no strategies are
         --  defined by the user (which is the only supported case right now),
         --  generate default (random) strategies.

         --  Collect the type translations

         Collect_Type_Translations (Context, Subp);

         --  Generate the actual body of the test. Consists of the test body +
         --  the needed with clauses.

         declare
            Test_Gen : constant Test_Generator.Test_Generator :=
              Test_Generator.Create
                (Test_Template_File => Get_Template_Test_ADB,
                 Subp               => Subp_Data);
         begin
            Res.Generated_Body :=
              Unbounded_Text_Type'
                (To_Unbounded_Wide_Wide_String
                   (Test_Gen.Generate_Source_Code (Context)));

            Res.With_Clauses := Get_With_Clauses (Subp_Data);
         end;

         Pkg_Data.Subprograms.Append (Subp_Data);
         Context.Packages_Data.Replace (Pkg_Data);

         --  Register parameters' type in the context

         for Param of Subp_Data.Parameters_Data loop
            declare

               Param_Type : constant SP.Ref :=
                 Context.Type_Translations.Element
                   (Param.Type_Fully_Qualified_Name);

               Typ_Parent_Package : constant Unbounded_Text_Type :=
                 To_Unbounded_Text
                   (+Param_Type.Get.Parent_Package_Name);

               procedure Add_To_Set
                 (Parent_Pkg_Name : Unbounded_Text_Type;
                  TS : in out Typ_Set);

               procedure Add_To_Set
                 (Parent_Pkg_Name : Unbounded_Text_Type;
                  TS : in out Typ_Set)
               is
                  pragma Unreferenced (Parent_Pkg_Name);
               begin
                  if not TS.Contains (Param_Type) then
                     TS.Insert (Param_Type);
                  end if;
               end Add_To_Set;

            begin
               if not Context.Required_Type_Strategies.
                 Contains (Typ_Parent_Package)
               then
                  Context.Required_Type_Strategies.Insert
                    (Typ_Parent_Package, Typ_Sets.Empty);
               end if;
               Context.Required_Type_Strategies.Update_Element
                 (Context.Required_Type_Strategies.Find
                    (Typ_Parent_Package),
                  Add_To_Set'Access);
            end;
         end loop;

         --  Then, register the strategies used for each type. We will consider
         --  the default random generation strategies here. Actual generation
         --  (of source code) is deferred to the finalization of the generation
         --  context.

         for Param of Subp_Data.Parameters_Data loop
            declare

               Param_Type : constant SP.Ref :=
                 Context.Type_Translations.Element
                   (Param.Type_Fully_Qualified_Name);

            begin
               if not Context.Type_And_Param_Strategies.Contains
                 (Param.Type_Fully_Qualified_Name)
               then
                  Context.Type_And_Param_Strategies.Insert
                    (Param.Type_Fully_Qualified_Name,
                     Param_Type.Get.Generate_Random_Strategy (Context));
               end if;
               declare
                  Type_Strategy : constant Dynamic_Strategy_Type :=
                    Dynamic_Strategy_Type
                      (Context.Type_And_Param_Strategies.Element
                         (Param.Type_Fully_Qualified_Name));
               begin

                  --  Insert in the context all the needed strategies for that
                  --  strategy. TODO: consider strategy nesting (for
                  --  dispatching strategies for example).

                  Context.Strategies.Include (Type_Strategy);

               --  Then, also generate a strategy for the parameter. This is
               --  by default a wrapping strategy around the default (random)
               --  type strategy. We could imagine using constant in the
               --  function body and generate sampling strategies. TODO: think
               --  about this.

                  declare
                     Param_Strategy : Dynamic_Strategy_Type
                       (Kind => Wrapping_Kind, Constrained => False);

                     --  The param strategy function takes the same parameters
                     --  as the default type strategy, but has a different
                     --  name, and is defined in a different package.

                     Param_Strategy_Function : Subprogram_Data :=
                       Type_Strategy.Strategy_Function;

                  begin
                     Param_Strategy_Function.Parent_Package :=
                       +Param_Strat_Package_Name
                       (Strip (+Param_Strategy_Function.Parent_Package));
                     Param_Strategy_Function.Name :=
                       +Gen_Param_Function_Name (Subp_Data, Param);
                     Param_Strategy_Function.Fully_Qualified_Name :=
                       +Param_Strat_Package_Name
                       (Strip (+Param_Strategy_Function.Name));
                     Param_Strategy.Strategy_Function :=
                       Param_Strategy_Function;

                     Context.Type_And_Param_Strategies.Insert
                       (Subp_Data.Fully_Qualified_Name & Param.Name,
                        Param_Strategy);

                     Context.Strategies.Insert (Param_Strategy);
                  end;
               end;
            end;
         end loop;
      end;
      return Res;
   end Generate_Tests_For;

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
      Subp     : Subp_Decl;
      Subp_UID : Unbounded_String := Null_Unbounded_String)
   is
      Subp_Data : Subprogram_Data :=
        Extract_Subprogram_Data (Subp);
      Function_JSON     : constant JSON_Value := Create_Object;
      Test_Vectors_JSON : JSON_Array := Empty_Array;
      Test_Vector_JSON : JSON_Array;
   begin
      Collect_Type_Translations (Context, Subp);
      Subp_Data.All_Params_Static := True;
      for Param of Subp_Data.Parameters_Data loop
         if Param.Mode in In_Mode | In_Out_Mode then
            declare
               Param_Type : constant SP.Ref :=
               Context.Type_Translations.Element
                  (Param.Type_Fully_Qualified_Name);
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
                     Try_Generate_Static (Param_Type, Context));
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
            begin
               Param_JSON.Set_Field ("name", Create (+Param.Name));
               Param_JSON.Set_Field ("type_name", Create (+Param.Type_Name));
               if Param.Mode in In_Mode | In_Out_Mode then
                  declare
                     Strat : Static_Strategy_Type'Class :=
                        Static_Strategy_Type
                          (Context.Type_And_Param_Strategies.Element
                            (Param.Type_Fully_Qualified_Name));
                  begin
                     Param_JSON.Set_Field
                     ("value",
                        Strat.Generate_Static_Value
                        (Disc_Value_Maps.Empty_Map).To_String);
                  end;
               end if;
               Param_JSON.Set_Field
               ("mode", Create (Integer'(Parameter_Mode'Pos (Param.Mode))));
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
      if not Is_Null (Subp.F_Subp_Spec.P_Returns) then
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
