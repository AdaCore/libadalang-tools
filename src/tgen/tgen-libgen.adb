------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2022, AdaCore                    --
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

with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Unparsing; use Libadalang.Unparsing;

with Templates_Parser;

with TGen.Dependency_Graph;   use TGen.Dependency_Graph;
with TGen.LAL_Utils;
with TGen.Marshalling;        use TGen.Marshalling;
with TGen.Marshalling.Binary_Marshallers;
with TGen.Marshalling.JSON_Marshallers;
with TGen.Type_Representation;
with TGen.Types.Constraints;  use TGen.Types.Constraints;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;
with TGen.Types.Translation;  use TGen.Types.Translation;
with TGen.Types;              use TGen.Types;

package body TGen.Libgen is

   procedure Generate_Support_Library
     (Ctx        : Libgen_Context;
      Pack_Name  : Ada_Qualified_Name) with
     Pre => Ctx.Types_Per_Package.Contains (Pack_Name);
   --  Generate the support library files (spec and body) for the types that
   --  are declared in Pack_Name.

   procedure Generate_Value_Gen_Library
     (Ctx       : Libgen_Context;
      Pack_Name : Ada_Qualified_Name) with
     Pre => Ctx.Strat_Types_Per_Package.Contains (Pack_Name);
   --  Generate the type representation library files (spec and body) for the
   --  types that are declared in Pack_Name.

   procedure Generate_Wrappers_Library
     (Ctx       : Libgen_Context;
      Pack_Name : Ada_Qualified_Name);
   --  Generate the function wrappers

   procedure Generate_Harness_Unit
     (Ctx              : Libgen_Context;
      Pack_Name        : Ada_Qualified_Name;
      Harness_Dir      : String;
      Test_Output_Dir  : String;
      Default_Strat    : Default_Strat_Kind;
      Default_Test_Num : Natural;
      Bin_Tests        : Boolean) with
     Pre => Ctx.Generation_Map.Contains (Pack_Name);
   --  Generate one harness unit (spec and body) for the subprograms registered
   --  in Pack_Name.

   procedure Replace_Standard (FQN : in out Ada_Qualified_Name);
   --  Replace occurrences of reserved namespaces (such as standard) with our
   --  owns (tgen).

   function Library_Package
     (Pack_Name         : Ada_Qualified_Name;
      TGen_Library_Name : String) return Ada_Qualified_Name;
   function Support_Library_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is (Library_Package (Pack_Name, "TGen_Support"));
   function Value_Library_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is (Library_Package (Pack_Name, "TGen_Values"));
   function Wrapper_Library_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is (Library_Package (Pack_Name, "TGen_Wrappers"));
   function Generation_Harness_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is (Library_Package (Pack_Name, "TGen_Generation"));
   --  Name of the support library package

   procedure Append_Types
     (Source           : Typ_Set;
      Dest             : in out Types_Per_Package_Map;
      Pack_Name_Fct    : access function (A : Ada_Qualified_Name)
                            return Ada_Qualified_Name);
   --  Include all the types in Source in the correct package key in Dest. All
   --  anonymous types are ignored.

   ----------------------
   -- Replace_Standard --
   ----------------------

   procedure Replace_Standard (FQN : in out Ada_Qualified_Name) is
      use type Ada.Containers.Count_Type;
   begin
      --  TODO: add particular cases for all reserved namespaces

      if FQN.Length = 1
         and then To_String (FQN.First_Element) = "standard"
      then
         FQN.Replace_Element (1, To_Unbounded_String ("TGen"));
      end if;
   end Replace_Standard;

   ---------------------
   -- Library_Package --
   ---------------------

   function Library_Package
     (Pack_Name         : Ada_Qualified_Name;
      TGen_Library_Name : String) return Ada_Qualified_Name
   is
      Result : Ada_Qualified_Name := Pack_Name.Copy;
   begin
      Replace_Standard (Result);
      Result.Append (TGen.Strings.Ada_Identifier (+TGen_Library_Name));
      return Result;
   end Library_Package;

   ------------------------------
   -- Generate_Support_Library --
   ------------------------------

   procedure Generate_Support_Library
     (Ctx        : Libgen_Context;
      Pack_Name  : Ada_Qualified_Name)
   is
      use Ada_Identifier_Vectors;

      F_Spec           : File_Type;
      F_Body           : File_Type;
      Ada_Pack_Name    : constant String := To_Ada (Pack_Name);
      Origin_Unit      : Ada_Qualified_Name := Pack_Name.Copy;
      Typ_Dependencies : Typ_Set;
      File_Name        : constant String :=
        Ada.Directories.Compose
          (Containing_Directory => To_String (Ctx.Output_Dir),
           Name                 => To_Filename (Pack_Name));

      Types : constant Types_Per_Package_Maps.Constant_Reference_Type :=
        Ctx.Types_Per_Package.Constant_Reference (Pack_Name);

      TRD : constant String := To_String (Ctx.Root_Templates_Dir);

      Sorted_Types : Typ_List;

   begin
      Create (F_Spec, Out_File, File_Name & ".ads");
      Put (F_Spec, "with TGen.Marshalling_Lib; ");
      Put_Line (F_Spec, "use TGen.Marshalling_Lib;");
      Put_Line (F_Spec, "with TGen.JSON;");
      Put (F_Spec, "with Interfaces; ");
      Put_Line (F_Spec, "use Interfaces;");
      Put_Line (F_Spec, "with Ada.Streams; use Ada.Streams;");
      Put_Line (F_Spec, "with TGen.Strings;");
      Put_Line (F_Spec, "with TGen.Big_Int;");
      Put_Line (F_Spec, "with TGen.Types;");
      Put_Line (F_Spec, "with TGen.Types.Discrete_Types;");
      Put_Line (F_Spec, "with TGen.Types.Int_Types;");
      New_Line (F_Spec);

      --  Add the import of the original unit, to be able to declare
      --  subprograms with the same profile as in the original unit.

      Origin_Unit.Delete_Last;

      if Ctx.Imports_Per_Unit.Contains (Origin_Unit) then
         for Dep of Ctx.Imports_Per_Unit.Constant_Reference (Origin_Unit) loop
            Put_Line (F_Spec, "with " & To_Ada (Dep) & ";");
         end loop;
      end if;

      --  Add the imports to the support packages for all the types of the
      --  subprograms declared in this package

      if Ctx.Support_Packs_Per_Unit.Contains (Origin_Unit) then
         for Dep of Ctx.Support_Packs_Per_Unit
                      .Constant_Reference (Origin_Unit)
         loop
            if Dep /= Pack_Name then
               Put_Line (F_Spec, "with " & To_Ada (Dep) & "; use "
                                 & To_Ada (Dep) & ";");
            end if;
         end loop;
      end if;

      Put_Line (F_Spec, "package " & Ada_Pack_Name & " is");
      New_Line (F_Spec);

      --  Create a dummy null procedure in each support package, in case we end
      --  up not generating anything, to ensure that the body is still legal
      --  and the support library still builds.

      Put_Line (F_Spec, "   procedure Dummy;");
      New_Line (F_Spec);

      Create (F_Body, Out_File, File_Name & ".adb");

      --  Also include the needed library support package dependencies, as
      --  the types marshalling / unmarshalling functions may depend on
      --  m / u functions defined in other library support packages.

      for T of Types loop
         Typ_Dependencies.Union (Type_Dependencies (T));
      end loop;

      --  Add the with clauses resulting from type dependencies. We have to be
      --  careful to remove the self dependency.

      declare
         Package_Dependencies : Ada_Qualified_Name_Set;
         Package_Dependency   : Ada_Qualified_Name;
      begin
         for T of Typ_Dependencies loop
            Package_Dependency :=
              Support_Library_Package
                (if T.Get.Kind in Anonymous_Kind
                 then TGen.Types.Constraints.As_Anonymous_Typ (T)
                        .Named_Ancestor.Get.Compilation_Unit_Name
                 else T.Get.Compilation_Unit_Name);
            if Package_Dependency /= Pack_Name then
               Package_Dependencies.Include (Package_Dependency);
            end if;
         end loop;

         for Pack_Name of Package_Dependencies loop
            Put_Line
              (F_Body,
               "with " & To_Ada (Pack_Name) & "; use " & To_Ada (Pack_Name)
               & ";");
         end loop;
      end;

      Put_Line
        (F_Body, "package body " & Ada_Pack_Name & " is");
      New_Line (F_Body);

      --  Complete the dummy null procedure

      Put_Line (F_Body, "procedure Dummy is null;");
      New_Line (F_Body);

      --  Disable predicate checks in the marshalling and unmarshalling
      --  functions.

      Put_Line
        (F_Body, "   pragma Assertion_Policy (Predicate => Ignore);");
      New_Line (F_Body);

      --  Generate the marshalling support lib. Make sure to sort the types
      --  in dependency order otherwise we will get access before elaboration
      --  issues when computing the Size_Max constants for each type:
      --  we need the Size_Max of all the dependencies of the components to be
      --  available before being able to compute the Size_Max for a composite
      --  type.

      Sorted_Types := Sort (Types);

      --  Output the marshalling subprograms for the types with a public part
      --  first, then create the private part for the package and output the
      --  marshalling subprograms for the fully private types. Hopefully this
      --  won't break too much the topological order on the types, otherwise
      --  we'll need to make marshaller generation much finer grain in order to
      --  both be able to have the marshaller subprograms declarations respect
      --  the visibility of the type, and have the implementation details
      --  generated in the correct order.

      declare
         Spec_Part, Private_Part, Body_Part : aliased Unbounded_String;

         Spec_Part_Acc : US_Access;
         --  This indicates where we should write the specification
         --  declarations for the current type (private or public spec). It
         --  points to Private_Part, if the type is fully private (i.e. the
         --  parent package of its first part is a private package), or to
         --  Spec_Part otherwise.

      begin
         for T of Sorted_Types loop

            if Is_Supported_Type (T.Get)

              --  We ignore instance types when generating marshallers as they
              --  are not types per-se, but a convenient way of binding a type
              --  to its strategy context.

              and then T.Get not in Instance_Typ'Class
            then
               Spec_Part_Acc :=
                 (if T.Get.Fully_Private
                  then Private_Part'Unrestricted_Access
                  else Spec_Part'Unrestricted_Access);
               if T.Get.Kind in Function_Kind then
                  TGen.Marshalling.JSON_Marshallers
                    .Generate_TC_Serializers_For_Subp
                      (Spec_Part_Acc,
                       Private_Part'Unrestricted_Access,
                       Body_Part'Unrestricted_Access,
                       T.Get,
                       TRD);
               else
                  TGen.Marshalling.Binary_Marshallers
                    .Generate_Marshalling_Functions_For_Typ
                      (Spec_Part_Acc,
                       Private_Part'Unrestricted_Access,
                       Body_Part'Unrestricted_Access,
                       T.Get,
                       TRD);
                  TGen.Marshalling.JSON_Marshallers
                    .Generate_Marshalling_Functions_For_Typ
                      (Spec_Part_Acc,
                       Private_Part'Unrestricted_Access,
                       Body_Part'Unrestricted_Access,
                       T.Get,
                       TRD);
               end if;
            end if;
         end loop;
         Put_Line (F_Body, +Body_Part);
         Put_Line (F_Spec, +Spec_Part);
         Put_Line (F_Spec, "private");
         Put_Line (F_Spec, +Private_Part);
      end;

      Put_Line (F_Body, "end " & Ada_Pack_Name & ";");
      Close (F_Body);
      Put_Line (F_Spec, "end " & Ada_Pack_Name & ";");
      Close (F_Spec);
   end Generate_Support_Library;

   --------------------------------
   -- Generate_Value_Gen_Library --
   --------------------------------

   procedure Generate_Value_Gen_Library
     (Ctx       : Libgen_Context;
      Pack_Name : Ada_Qualified_Name)
   is
      use Templates_Parser;
      F_Spec           : File_Type;
      F_Body           : File_Type;
      Ada_Pack_Name    : constant String := To_Ada (Pack_Name);
      Typ_Dependencies : Typ_Set;
      File_Name        : constant String :=
        Ada.Directories.Compose
          (Containing_Directory => To_String (Ctx.Output_Dir),
           Name                 => To_Filename (Pack_Name));

      Types : constant Types_Per_Package_Maps.Constant_Reference_Type :=
        Ctx.Strat_Types_Per_Package.Constant_Reference (Pack_Name);

      Initialization_Code : Tag;
      --  Code that should be put in the initialization section of the
      --  package body.

   begin
      Create (F_Spec, Out_File, File_Name & ".ads");
      Create (F_Body, Out_File, File_Name & ".adb");
      Put (F_Spec, "with Interfaces; ");
      Put_Line (F_Spec, "use Interfaces;");
      Put_Line (F_Spec, "with Ada.Streams; use Ada.Streams;");
      Put_Line (F_Spec, "with TGen.JSON;");
      Put_Line (F_Spec, "with TGen.Strategies;");
      Put_Line (F_Spec, "with TGen.Strings;");
      Put_Line (F_Spec, "with TGen.Big_Int;");
      Put_Line (F_Spec, "with TGen.Big_Reals;");
      Put_Line (F_Spec, "with TGen.Types;");
      Put_Line (F_Spec, "with TGen.Types.Array_Types;");
      Put_Line (F_Spec, "with TGen.Types.Constraints;");
      Put_Line (F_Spec, "with TGen.Types.Discrete_Types;");
      Put_Line (F_Spec, "with TGen.Types.Enum_Types;");
      Put_Line (F_Spec, "with TGen.Types.Int_Types;");
      Put_Line (F_Spec, "with TGen.Types.Real_Types;");
      Put_Line (F_Spec, "with TGen.Types.Record_Types;");

      --  Also include the needed library support package dependencies, as
      --  the type representation may depend on other types' representation.

      for T of Types loop
         Typ_Dependencies.Union (Type_Dependencies (T));
      end loop;

      --  Add the with clauses resulting from type dependencies. We have to be
      --  careful to remove the self dependency.

      declare
         use Ada_Identifier_Vectors;
         Lib_Marshalling_Dependencies : Ada_Qualified_Name_Set;
         --  All of the marshalling packages we should have access to, to
         --  implement custom strategies. TODO: add dependencies only for
         --  custom strategies types here.

         Lib_Type_Dependencies : Ada_Qualified_Name_Set;
         --  All of the type representation packages we should have access to,
         --  to instantiate our type definitions.

         Package_Dependency : Ada_Qualified_Name;
      begin
         for T of Typ_Dependencies loop
            --  Ignore function type and anonymous named types

            if T.Get.Kind /= Function_Kind then
               Package_Dependency :=
                 Support_Library_Package
                   (if T.Get.Kind in Anonymous_Kind
                    then TGen.Types.Constraints.As_Anonymous_Typ (T)
                    .Named_Ancestor.Get.Compilation_Unit_Name
                    else T.Get.Compilation_Unit_Name);
               if Package_Dependency /= Pack_Name then
                  Lib_Marshalling_Dependencies.Include (Package_Dependency);
               end if;
            end if;

            --  Also include the value library package dependency

            Package_Dependency :=
              Value_Library_Package
                (if T.Get.Kind in Anonymous_Kind
                 then TGen.Types.Constraints.As_Anonymous_Typ (T)
                        .Named_Ancestor.Get.Compilation_Unit_Name
                 else T.Get.Compilation_Unit_Name);
            if Package_Dependency /= Pack_Name then
               Lib_Type_Dependencies.Include (Package_Dependency);
            end if;
         end loop;

         for Pack_Name of Lib_Type_Dependencies loop
            Put_Line
              (F_Spec,
               "with " & To_Ada (Pack_Name) & "; use " & To_Ada (Pack_Name)
               & ";");
         end loop;

         for Pack_Name of Lib_Marshalling_Dependencies loop
            Put_Line
              (F_Body,
               "with " & To_Ada (Pack_Name) & "; use " & To_Ada (Pack_Name)
               & ";");
         end loop;
      end;

      Put_Line (F_Spec, "package " & Ada_Pack_Name & " is");
      New_Line (F_Spec);
      Put_Line (F_Spec, "   pragma Elaborate_Body;");
      New_Line (F_Spec);

      Put_Line (F_Body, "package body " & Ada_Pack_Name & " is");
      New_Line (F_Body);

      --  We have to make sure to generate the initialization code in the
      --  right order, as the type's component types representation
      --  initialization code must be generated before the type. Sort the
      --  types here.

      for T of Sort (Types) loop
         TGen.Type_Representation.Generate_Type_Representation_For_Typ
           (F_Spec, F_Body, T.Get,
            To_String (Ctx.Root_Templates_Dir),
            Ctx.Strategy_Map,
            Initialization_Code);
      end loop;

      --  Print the initialization code, used for the type representation

      if Size (Initialization_Code) /= 0 then
         Put_Line (F_Body, "begin");
         for I in 1 .. Size (Initialization_Code) loop
            Put_Line (F_Body, Item (Initialization_Code, I));
         end loop;
      end if;

      Put_Line (F_Body, "end " & Ada_Pack_Name & ";");
      Close (F_Body);
      Put_Line (F_Spec, "end " & Ada_Pack_Name & ";");
      Close (F_Spec);
   end Generate_Value_Gen_Library;

   -------------------------------
   -- Generate_Wrappers_Library --
   -------------------------------

   procedure Generate_Wrappers_Library
     (Ctx       : Libgen_Context;
      Pack_Name : Ada_Qualified_Name)
   is
      F_Spec           : File_Type;
      F_Body           : File_Type;
      Ada_Pack_Name    : constant String := To_Ada (Pack_Name);
      File_Name        : constant String :=
        Ada.Directories.Compose
          (Containing_Directory => To_String (Ctx.Output_Dir),
           Name                 => To_Filename (Pack_Name));
   begin
      Create (F_Spec, Out_File, File_Name & ".ads");
      Create (F_Body, Out_File, File_Name & ".adb");

      Put_Line (F_Spec, "package " & Ada_Pack_Name & " is");

      Put_Line (F_Body, "with TGen;");
      New_Line (F_Body);
      Put_Line (F_Body, "package body " & Ada_Pack_Name & " is");
      New_Line (F_Body);

      for Subp of Ctx.Included_Subps.Element (Pack_Name) loop
         declare
            LAL_Context : constant Libadalang.Analysis.Analysis_Context :=
              Create_Context;
            Unit        : Libadalang.Analysis.Analysis_Unit;
         begin
            Unit := Libadalang.Analysis.Get_From_Buffer
              (LAL_Context,
               Filename => "Dummy",
               Buffer   => +Subp.Pre,
               Rule     => Libadalang.Common.Expr_Rule);
            Generate_Wrapper_For_Subprogram
              (F_Spec             => F_Spec,
               F_Body             => F_Body,
               Subprogram         => Function_Typ (As_Function_Typ (Subp.T)),
               Precond            => Unit.Root,
               Templates_Root_Dir => To_String (Ctx.Root_Templates_Dir));
         end;
      end loop;

      Put_Line (F_Body, "end " & Ada_Pack_Name & ";");
      Close (F_Body);
      Put_Line (F_Spec, "end " & Ada_Pack_Name & ";");
      Close (F_Spec);
   end Generate_Wrappers_Library;

   ------------------
   -- Append_Types --
   ------------------

   procedure Append_Types
     (Source           : Typ_Set;
      Dest             : in out Types_Per_Package_Map;
      Pack_Name_Fct    : access function (A : Ada_Qualified_Name)
                            return Ada_Qualified_Name)
   is
   begin
      for T of Source loop
         if not (T.Get.Kind in Anonymous_Kind)
         then
            declare
               use Types_Per_Package_Maps;
               Pack_Name      : constant Ada_Qualified_Name :=
                 Pack_Name_Fct (T.Get.Compilation_Unit_Name);
               Cur            : Cursor := Dest.Find (Pack_Name);
               Dummy_Inserted : Boolean;
            begin
               if Cur = No_Element then
                  Dest.Insert
                    (Pack_Name, Typ_Sets.Empty_Set, Cur, Dummy_Inserted);
               end if;
               Dest.Reference (Cur).Include (T);
            end;
         end if;
      end loop;
   end Append_Types;

   ------------
   -- Create --
   ------------

   function Create
     (Output_Dir         : String;
      User_Project_Path  : String;
      Root_Templates_Dir : String := "") return Libgen_Context
   is
      Actual_Templates_Dir : constant String :=
        (if Root_Templates_Dir = ""
         then Ada.Directories.Containing_Directory
                (GNAT.OS_Lib.Locate_Exec_On_Path
                               (Ada.Command_Line.Command_Name).all
              & "/../share/tgen/templates")
         else Root_Templates_Dir);
      --  Try to find the templates in the canonical installation path if not
      --  supplied.

   begin
      return (Output_Dir            =>
                To_Unbounded_String (Output_Dir),
              Root_Templates_Dir    =>
                To_Unbounded_String (Actual_Templates_Dir),
              User_Project_Path     =>
                To_Unbounded_String (User_Project_Path),
              Lib_Support_Generated => False,
              others                => <>);
   end Create;

   function Create
     (Output_Dir         : GNATCOLL.VFS.Virtual_File;
      User_Project_Path  : GNATCOLL.VFS.Virtual_File;
      Root_Templates_Dir : GNATCOLL.VFS.Virtual_File) return Libgen_Context
   is
      use GNATCOLL.VFS;
   begin
      return Create (Output_Dir         => +Output_Dir.Full_Name,
                     Root_Templates_Dir => +Root_Templates_Dir.Full_Name,
                     User_Project_Path  => +User_Project_Path.Full_Name);
   end Create;

   ------------------
   -- Include_Subp --
   ------------------

   function Include_Subp
     (Ctx  : in out Libgen_Context;
      Subp : Basic_Decl'Class;
      Diag : out Unbounded_String) return Boolean
   is
      use Ada_Qualified_Name_Sets_Maps;

      Subp_Types : Typ_Set;
      --  Transitive closure of required types for the parameters of the
      --  subprogram.

      Unit_Name : constant Ada_Qualified_Name :=
        TGen.LAL_Utils.Convert_Qualified_Name
          (Subp.P_Enclosing_Compilation_Unit.P_Syntactic_Fully_Qualified_Name);
      --  Name of the compilation unit this subprogram belongs to.

      Support_Packs : Cursor := Ctx.Support_Packs_Per_Unit.Find (Unit_Name);
      --  Cursor to the set of support packages for the unit this subprogram
      --  belongs to.

      Imports : Cursor := Ctx.Imports_Per_Unit.Find (Unit_Name);
      --  Cursor to the set of withed units for Unit_Name.

      Dummy_Inserted : Boolean;

      Trans_Res : constant Translation_Result :=
        Translate (Subp.As_Basic_Decl);

   begin
      if not Trans_Res.Success then
         Diag := Trans_Res.Diagnostics;
         return False;
      end if;

      declare
         Unsupported_Diags : constant String :=
           Trans_Res.Res.Get.Get_Diagnostics;
      begin
         if Unsupported_Diags'Length > 0 then
            Diag := To_Unbounded_String (Unsupported_Diags);
            return False;
         end if;
      end;

      if Support_Packs = No_Element then
         Ctx.Support_Packs_Per_Unit.Insert
           (Unit_Name,
            Ada_Qualified_Name_Sets.Empty_Set,
            Support_Packs,
            Dummy_Inserted);
      end if;

      --  Fill the Imports_Per_Unit map only if it hasn't been done before.
      --  Do not try to do that for the Standard unit (which doesn't depend on
      --  anything either way).

      if Imports = No_Element
        and then To_String (Unit_Name.First_Element) /= "standard"
      then
         Ctx.Imports_Per_Unit.Insert
           (Unit_Name,
            Ada_Qualified_Name_Sets.Empty_Set,
            Imports,
            Dummy_Inserted);
         for Unit of Subp.P_Enclosing_Compilation_Unit.P_Withed_Units loop
            Ctx.Imports_Per_Unit.Reference (Imports).Insert
              (TGen.LAL_Utils.Convert_Qualified_Name
                 (Unit.P_Syntactic_Fully_Qualified_Name));
         end loop;
      end if;

      declare
         Fct_Typ : Function_Typ'Class :=
           Function_Typ'Class (Trans_Res.Res.Unchecked_Get.all);
         Fct_Ref : SP.Ref;
      begin
         --  Check strategies. TODO???: integrate it into the type translation
         --  when this is more than a proof of concept.

         if Subp.P_Has_Aspect (To_Unbounded_Text (To_Text ("Generation")))
         then
            Parse_Strategy.Parse_Strategy
              (Fct_Typ,
               Subp.P_Get_Aspect_Assoc
                 (To_Unbounded_Text (To_Text ("Generation"))),
               Ctx.Strategy_Map);
         end if;

         for Param of Fct_Typ.Component_Types loop

            --  Fill out the support package map

            Ctx.Support_Packs_Per_Unit.Reference (Support_Packs).Include
              (Support_Library_Package (Param.Get.Compilation_Unit_Name));
         end loop;

         --  Get the transitive closure of the types on which the parameters'
         --  types depend, that need to be included in the support library.

         Fct_Ref.Set (Fct_Typ);
         Subp_Types := Type_Dependencies (Fct_Ref, Transitive => True);

         --  Ctx.Types_Per_Package contains the types for which marshallers
         --  will be generated (with the exclusion of instance types and
         --  anonymous types), whereas Ctx.Strat_Types_Per_Package contains the
         --  types for which the representation will be generated. These are
         --  very similar in practice, the main difference being that the Strat
         --  map will contain the instantiated Function_Typ, whereas the
         --  regular map will only contain the base Function_Typ.

         Append_Types
           (Subp_Types,
            Ctx.Types_Per_Package,
            Support_Library_Package'Access);

         --  Add the "vanilla" function type to the set of types for which we
         --  want to generate a support library; this enables the generation
         --  of whole testcase serializers.

         Append_Types
           (Typ_Sets.To_Set (Trans_Res.Res),
            Ctx.Types_Per_Package,
            Support_Library_Package'Access);

         Append_Types
           (Subp_Types,
            Ctx.Strat_Types_Per_Package,
            Value_Library_Package'Access);

         Append_Types
           (Typ_Sets.To_Set (Fct_Ref),
            Ctx.Strat_Types_Per_Package,
            Value_Library_Package'Access);

         --  Only add the (instantiated) function types to the generation map.
         --  Skip unsupported subprograms.

         if not Fct_Ref.Get.Supports_Gen then
            Put_Line
               ("Warning (TGen): subprogram "
               & Image (Subp.P_Unique_Identifying_Name) & " does not support"
               & " value generation.");
         else
            Append_Types
              (Typ_Sets.To_Set (Fct_Ref),
               Ctx.Generation_Map,
               Generation_Harness_Package'Access);
         end if;

         --  Add it to the list of included subprograms in the context

         declare
            Dummy_Inserted : Boolean;
            Cur            : Subp_Info_Vectors_Maps.Cursor;
            Pre_Aspect     : constant Unbounded_Text_Type :=
              To_Unbounded_Text (To_Text ("Pre"));
            Subp_Info      : Subp_Information;
         begin
            if Subp.P_Has_Aspect (Pre_Aspect) then
               Subp_Info.Pre :=
                 +Unparse (Subp.P_Get_Aspect_Spec_Expr (Pre_Aspect));
            end if;
            Subp_Info.T := Fct_Ref;
            Ctx.Included_Subps.Insert
              (Wrapper_Library_Package (Fct_Ref.Get.Compilation_Unit_Name),
               [],
               Cur,
               Dummy_Inserted);
            Ctx.Included_Subps.Reference (Cur).Append (Subp_Info);
         end;

         return True;
      end;
   end Include_Subp;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Ctx : in out Libgen_Context; Part : Any_Library_Part := All_Parts)
   is
      use GNATCOLL.VFS;
      use Types_Per_Package_Maps;
      Output_Dir : constant String := To_String (Ctx.Output_Dir);
   begin
      if not Is_Directory (Create (+Output_Dir)) then
         Ada.Directories.Create_Path (Output_Dir);
      end if;

      --  Genenerate project file

      declare
         Prj_File : File_Type;
         Cwd : constant Virtual_File :=
           Create (+Ada.Directories.Current_Directory);
         User_Prj : constant Virtual_File :=
           Create (+To_String (Ctx.User_Project_Path));
         Support_Prj : constant Virtual_File := Create (+Output_Dir);

         Rel_Path : constant Filesystem_String := Relative_Path
           ((if User_Prj.Is_Absolute_Path then User_Prj else Cwd / User_Prj),
            (if Support_Prj.Is_Absolute_Path
             then Support_Prj
             else Cwd / Support_Prj));
      begin
         Create
           (Prj_File,
            Out_File,
            Output_Dir & GNAT.OS_Lib.Directory_Separator & "tgen_support.gpr");
         Put_Line (Prj_File, "with """ & (+Rel_Path) & """;");
         Put_Line (Prj_File, "with ""tgen_rts.gpr"";");
         New_Line (Prj_File);
         Put_Line (Prj_File, "library project TGen_Support is");
         New_Line (Prj_File);
         Put_Line (Prj_File, "   Lib_Name := ""tgen_support"";");
         New_Line (Prj_File);
         Put_Line (Prj_File, "   type Any_Library_Type is (""static"","
                             & " ""relocatable"", ""static-pic"");");
         Put_Line (Prj_File, "   Library_Type : Any_Library_Type := external"
                   & " (""LIBRARY_TYPE"", ""static"");");
         Put_Line (Prj_File, "   type Build_Mode_Type is (""dev"","
                             & " ""prod"");");
         Put_Line (Prj_File, "   Build_Mode : Build_Mode_Type := external"
                             & " (""BUILD_MODE"", ""dev"");");
         Put_Line (Prj_File, "   for Library_Name use Lib_Name;");
         Put_Line (Prj_File, "   for Library_Kind use Library_Type;");
         Put_Line (Prj_File, "   for Object_Dir use ""obj-"" & Lib_Name & "
                             & """-""" & " & Library_Type;");
         Put_Line (Prj_File, "   for Library_Dir use ""lib-"" & Lib_Name & "
                             & """-""" & " & Library_Type;");
         New_Line (Prj_File);
         Put_Line (Prj_File, "   for Source_Dirs use (""."");");
         New_Line (Prj_File);
         Put_Line (Prj_File, "   package Compiler is");
         Put_Line (Prj_File, "      case Build_Mode is");
         Put_Line (Prj_File, "         when ""dev"" =>");
         Put_Line (Prj_File, "            for Default_Switches (""Ada"") use"
                   & " (""-g"", ""-gnatg"", ""-gnatyN"", ""-gnatws"");");
         Put_Line (Prj_File, "         when ""prod"" =>");
         Put_Line (Prj_File, "            for Default_Switches (""Ada"") use"
                   & " (""-gnatg"", ""-gnatyN"", ""-gnatws"");");
         Put_Line (Prj_File, "      end case;");
         Put_Line (Prj_File, "   end Compiler;");
         Put_Line (Prj_File, "end TGen_support;");
         Close (Prj_File);
      end;

      --  Generate all support packages

      if Part (Marshalling_Part) then
         for Cur in Ctx.Types_Per_Package.Iterate loop
            --  If all types are not supported, do not generate a support
            --  library.

            if not (for all T of Element (Cur) =>
                      not Is_Supported_Type (T.Get))
            then
               Generate_Support_Library (Ctx, Key (Cur));
            end if;
         end loop;
      end if;
      if Part (Test_Generation_Part) then
         for Cur in Ctx.Strat_Types_Per_Package.Iterate loop
            --  If all types are not supported, do not generate a support
            --  library.

            if not (for all T of Element (Cur) =>
                      not Is_Supported_Type (T.Get))
            then
               Generate_Value_Gen_Library (Ctx, Key (Cur));
            end if;
         end loop;
      end if;
      if Part (Wrappers_Part) then
         for Cur in Ctx.Included_Subps.Iterate loop
            Generate_Wrappers_Library (Ctx, Subp_Info_Vectors_Maps.Key (Cur));
         end loop;
      end if;
      Ctx.Lib_Support_Generated := True;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate
     (Ctx  : in out Libgen_Context;
      Subp : LAL.Basic_Decl'Class;
      Diag : out Unbounded_String;
      Part : Any_Library_Part := All_Parts) return Boolean
   is
   begin
      if Include_Subp (Ctx, Subp, Diag) then
         Generate (Ctx, Part);
      else
         return False;
      end if;
      return True;
   end Generate;

   -------------------------------
   -- Required_Support_Packages --
   -------------------------------

   function Required_Support_Packages
     (Ctx       : Libgen_Context;
      Unit_Name : TGen.Strings.Ada_Qualified_Name)
      return TGen.Strings.Ada_Qualified_Name_Sets_Maps.Constant_Reference_Type
   is
   begin
      return Ctx.Support_Packs_Per_Unit.Constant_Reference (Unit_Name);
   end Required_Support_Packages;

   ---------------------------
   -- Generate_Harness_Unit --
   ---------------------------

   procedure Generate_Harness_Unit
     (Ctx              : Libgen_Context;
      Pack_Name        : Ada_Qualified_Name;
      Harness_Dir      : String;
      Test_Output_Dir  : String;
      Default_Strat    : Default_Strat_Kind;
      Default_Test_Num : Natural;
      Bin_Tests        : Boolean)
   is
      use GNATCOLL.VFS;
      use Templates_Parser;
      F_Spec : File_Type;
      F_Body : File_Type;

      Out_Dir : constant Virtual_File := Create (+Harness_Dir);

      Template_Path : constant Virtual_File :=
        Create (+To_String (Ctx.Root_Templates_Dir))
        / (+"generation_routine.tmplt");

      Original_Unit          : constant Ada_Qualified_Name :=
        Copy_Delete_Last (Pack_Name);
      Orig_Unit_Support_Pack : constant Ada_Qualified_Name :=
        Support_Library_Package (Original_Unit);
      Value_Lib_Pack         : constant Ada_Qualified_Name :=
        Value_Library_Package (Original_Unit);

      Subps : constant Types_Per_Package_Maps.Constant_Reference_Type :=
        Ctx.Generation_Map.Constant_Reference (Pack_Name);

      Support_Packs : constant Ada_Qualified_Name_Sets_Maps
                               .Constant_Reference_Type :=
        Ctx.Support_Packs_Per_Unit.Constant_Reference (Original_Unit);
      --  Support packages that apply to the types used in Original_Unit
   begin
      Create
        (F_Spec,
         Out_File,
         +Full_Name (Out_Dir / (+To_Filename (Pack_Name) & ".ads")));

      Put_Line (F_Spec, "package " & To_Ada (Pack_Name) & " is");
      Put_Line (F_Spec, "   procedure Generate;");
      Put_Line (F_Spec, "   --  Generate test vectors for subprograms defined"
                        & " in " & To_Ada (Original_Unit));
      Put_Line (F_Spec, "end " &  To_Ada (Pack_Name) & ";");

      Close (F_Spec);

      Create
        (F_Body,
         Out_File,
         +Full_Name (Out_Dir / (+To_Filename (Pack_Name) & ".adb")));

      if Bin_Tests then
         Put_Line (F_Body, "with Ada.Streams.Stream_IO;");
         Put_Line (F_Body, "with Ada.Strings.Fixed;");
      end if;

      Put_Line (F_Body, "with TGen.JSON;");
      Put_Line (F_Body, "with TGen.Types;");
      Put_Line (F_Body, "with TGen.Strategies;");
      New_Line (F_Body);

      for Dep of Support_Packs loop
         Put_Line
           (F_Body, "with " & To_Ada (Dep) & "; use " & To_Ada (Dep) & ";");
      end loop;
      if not Support_Packs.Contains (Orig_Unit_Support_Pack)
      then
         Put_Line
           (F_Body,
            "with " & To_Ada (Orig_Unit_Support_Pack) & "; use "
            & To_Ada (Orig_Unit_Support_Pack) & ";");
      end if;
      New_Line (F_Body);

      Put_Line
        (F_Body, "with " & To_Ada (Value_Lib_Pack) & "; use "
                 & To_Ada (Value_Lib_Pack) & ";");
      New_Line (F_Body);

      Put_Line (F_Body, "package body " & To_Ada (Pack_Name) & " is");

      --  Generate the generation routines for each subprogram

      for Subp of Subps loop
         declare
            use Component_Maps;
            Assocs : Translate_Set;
            Param_Names : Vector_Tag;
            Param_Types : Vector_Tag;
            Input_FNs   : Vector_Tag;
            Output_FNs  : Vector_Tag;
            Real_Name   : constant String :=
              As_Function_Typ (Subp).Simple_Name;
            Subp_Name   : constant String :=
              (if Is_Operator (Real_Name)
               then Map_Operator_Name (Real_Name)
               else Real_Name);

            Concrete_Typ : SP.Ref;
            --  Shortcut to hold the concrete type of a parameter

            Params : Component_Map renames
              As_Function_Typ (Subp).Component_Types;
            Param_Cur : Component_Maps.Cursor;
            Ordered_Params : String_Vectors.Vector renames
              As_Function_Typ (Subp).Param_Order;
         begin
            Assocs.Insert (Assoc ("GLOBAL_PREFIX", Global_Prefix));
            Assocs.Insert (Assoc ("NUM_TESTS", Default_Test_Num));
            Assocs.Insert (Assoc ("ENUM_STRAT", Default_Strat = Stateful));
            Assocs.Insert
              (Assoc ("SUBP_NAME", Subp_Name));
            Assocs.Insert
              (Assoc ("SUBP_UID", As_Function_Typ (Subp).Subp_UID));
            Assocs.Insert
              (Assoc
                ("FN_TYP_REF", Subp.Get.Slug & "_Typ_Ref"));
            for Param_Name of Ordered_Params loop
               Param_Cur := Params.Find (Param_Name);
               Param_Names.Append (Unbounded_String (Key (Param_Cur)));
               if Element (Param_Cur).Get.Kind in Anonymous_Kind then
                  Concrete_Typ :=
                    As_Anonymous_Typ (Element (Param_Cur)).Named_Ancestor;
               elsif Element (Param_Cur).Get.Kind in Instance_Kind then
                  Concrete_Typ :=
                    As_Instance_Typ (Element (Param_Cur)).Orig_Typ;
               else
                  Concrete_Typ := Element (Param_Cur);
               end if;
               Param_Types.Append (Concrete_Typ.Get.Fully_Qualified_Name);
               Input_FNs.Append (Input_Fname_For_Typ (Concrete_Typ.Get.Name));
               Output_FNs.Append
                 (Output_Fname_For_Typ (Concrete_Typ.Get.Name));
            end loop;
            Assocs.Insert (Assoc ("PARAM_NAME", Param_Names));
            Assocs.Insert (Assoc ("PARAM_TY", Param_Types));
            Assocs.Insert (Assoc ("INPUT_FN", Input_FNs));
            Assocs.Insert (Assoc ("OUTPUT_FN", Output_FNs));
            Assocs.Insert
              (Assoc
                 ("TC_NAME",
                  Unbounded_String'
                    (Test_Output_Dir & GNAT.OS_Lib.Directory_Separator
                     & (+Subp_Name) & "-"
                     & (+As_Function_Typ (Subp).Long_UID))));
            Assocs.Insert
              (Assoc
                 ("TC_FORMAT", String'(if Bin_Tests then "BIN" else "JSON")));
            Put_Line (F_Body, Parse (+Template_Path.Full_Name, Assocs));
            New_Line (F_Body);
         end;
      end loop;

      --  Generate the body of the global generation routine

      Put_Line (F_Body, "   procedure Generate is");
      if not Bin_Tests then
         Put_Line (F_Body, "      Dumper : constant TGen.JSON.Utils"
                           & ".JSON_Auto_IO :=");
         Put_Line (F_Body, "        TGen.JSON.Utils.Create ("""
                           & Test_Output_Dir & GNAT.OS_Lib.Directory_Separator
                           & To_Filename (Original_Unit) & ".json"");");
         Put_Line (F_Body, "      Unit_JSON : TGen.JSON.JSON_Value := Dumper"
                           & ".Get_JSON_Ref;");
      end if;
      Put_Line (F_Body, "   begin");
      for Subp of Subps loop
         declare
            Real_Name   : constant String :=
              As_Function_Typ (Subp).Simple_Name;
            Subp_Name   : constant String :=
              (if Is_Operator (Real_Name)
               then Map_Operator_Name (Real_Name)
               else Real_Name);
         begin
            Put_Line
              (F_Body,
               "      Gen_" & Subp_Name & '_'
               & To_String (As_Function_Typ (Subp).Subp_UID)
               & (if Bin_Tests then ";" else " (Unit_JSON);"));
         end;
      end loop;
      if Ada.Containers."=" (Subps.Length, 0) then
         Put_Line (F_Body, "      null;");
      end if;
      Put_Line (F_Body, "   end Generate;");

      Put_Line (F_Body, "end " & To_Ada (Pack_Name) & ";");

      Close (F_Body);
      exception
         when others =>
            if Is_Open (F_Spec) then
               Close (F_Spec);
            end if;
            if Is_Open (F_Body) then
               Close (F_Body);
            end if;
            raise;
   end Generate_Harness_Unit;

   ----------------------
   -- Generate_Harness --
   ----------------------

   procedure Generate_Harness
     (Ctx              : in out Libgen_Context;
      Harness_Dir      : String;
      Test_Output_Dir  : String;
      Default_Strat    : Default_Strat_Kind := Stateless;
      Default_Test_Num : Natural := 5;
      Bin_Tests        : Boolean := False)
   is
      use GNATCOLL.VFS;
      use Types_Per_Package_Maps;
      VHarness_Dir : constant Virtual_File := Create (+Harness_Dir);
      User_Prj     : constant Virtual_File :=
        Create (+To_String (Ctx.User_Project_Path));
      Support_Prj  : constant Virtual_File :=
        Create (+To_String (Ctx.Output_Dir)) / (+"tgen_support.gpr");
      Cwd          : constant Virtual_File := Get_Current_Dir;

      Rel_User_Path : constant Filesystem_String := Relative_Path
        ((if User_Prj.Is_Absolute_Path then User_Prj else Cwd / User_Prj),
         (if VHarness_Dir.Is_Absolute_Path
          then VHarness_Dir
          else Cwd / VHarness_Dir));
      Rel_Support_Path : constant Filesystem_String := Relative_Path
        ((if Support_Prj.Is_Absolute_Path
          then Support_Prj
          else Cwd / Support_Prj),
         (if VHarness_Dir.Is_Absolute_Path
          then VHarness_Dir
          else Cwd / VHarness_Dir));
      Prj_File  : File_Type;
      Main_File : File_Type;
   begin
      if not VHarness_Dir.Is_Directory then
         Ada.Directories.Create_Path (Harness_Dir);
      end if;

      if not Ctx.Lib_Support_Generated then
         Generate (Ctx);
      end if;
      Create
        (File => Prj_File,
         Mode => Out_File,
         Name => +Full_Name (VHarness_Dir / (+"tgen_generation_harness.gpr")));

      Put_Line (Prj_File, "with """ & (+Rel_User_Path) & """;");
      Put_Line (Prj_File, "with """ & (+Rel_Support_Path) & """;");
      Put_Line (Prj_File, "with ""tgen_rts.gpr"";");
      New_Line (Prj_File);
      Put_Line (Prj_File, "project TGen_Generation_Harness is");
      Put_Line (Prj_File, "   for Main use (""generation_main.adb"");");
      Put_Line (Prj_File, "   for Object_Dir use ""obj"";");
      Put_Line (Prj_File, "end TGen_Generation_Harness;");
      Close (Prj_File);

      Create
        (File => Main_File,
         Mode => Out_File,
         Name => +Full_Name (VHarness_Dir / (+"generation_main.adb")));

      for Unit_Cur in Ctx.Generation_Map.Iterate loop
         Put_Line (Main_File, "with " & To_Ada (Key (Unit_Cur)) & ";");
      end loop;
      New_Line (Main_File);
      Put_Line (Main_File, "procedure Generation_Main is");
      Put_Line (Main_File, "begin");
      for Unit_Cur in Ctx.Generation_Map.Iterate loop
         Put_Line (Main_File, "   " & To_Ada (Key (Unit_Cur)) & ".Generate;");
         Generate_Harness_Unit
           (Ctx,
            Key (Unit_Cur),
            Harness_Dir,
            Test_Output_Dir,
            Default_Strat,
            Default_Test_Num,
            Bin_Tests);
      end loop;
      Put_Line (Main_File, "end Generation_Main;");
      Close (Main_File);
   exception
      when others =>
         if Is_Open (Prj_File) then
            Close (Prj_File);
         end if;
         if Is_Open (Main_File) then
            Close (Main_File);
         end if;
         raise;
   end Generate_Harness;

end TGen.Libgen;
