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

with TGen.Marshalling;        use TGen.Marshalling;
with TGen.Marshalling.Binary_Marshallers;
with TGen.Marshalling.JSON_Marshallers;
with TGen.Types.Array_Types;
with TGen.Types.Constraints;
with TGen.Types.Record_Types;
with TGen.Types.Translation;  use TGen.Types.Translation;
with TGen.Types;              use TGen.Types;

package body TGen.Libgen is

   function Type_Dependencies
     (T : SP.Ref; Transitive : Boolean := False) return Typ_Set;
   --  Return all the types that T needs to have visibility on (i.e. index/
   --  component / discriminant types). If transitive is True, this returns
   --  the transitive closure of types on which self depends.

   procedure Generate_Support_Library
     (Ctx        : Libgen_Context;
      Pack_Name  : Ada_Qualified_Name;
      Part       : Any_Library_Part) with
     Pre => Ctx.Types_Per_Package.Contains (Pack_Name);
   --  Generate the support library files (spec and body) for the types that
   --  are declared in Pack_Name.

   function Support_Library_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name;
   --  Name of the support library package. Replace occurrences of reserved
   --  namespaces (such as standard) with our owns (tgen).

   procedure Append_Types
     (Source           : Typ_Set;
      Dest             : in out Types_Per_Package_Map;
      Ignore_Anonymous : Boolean := True);
   --  Include all the types in Source in the correct package key in Dest. If
   --  Ignore_Anonymous is True, all type of kind Anonymous_Typ_Kind will be
   --  filtered out.

   -----------------------
   -- Type_Dependencies --
   -----------------------

   function Type_Dependencies
     (T : SP.Ref; Transitive : Boolean := False) return Typ_Set
   is
      use TGen.Types.Array_Types;
      use TGen.Types.Constraints;
      use TGen.Types.Record_Types;

      Res : Typ_Set;

      procedure Inspect_Variant (Var : Variant_Part_Acc);
      --  Include the types of the components defined in Var in the set of type
      --  on which T depends on, and inspect them transitively if needed.

      procedure Inspect_Variant (Var : Variant_Part_Acc) is
      begin
         if Var = null then
            return;
         end if;
         for Choice of Var.Variant_Choices loop
            for Comp of Choice.Components loop
               Res.Include (Comp);
               if Transitive then
                  Res.Union (Type_Dependencies (Comp, Transitive));
               end if;
            end loop;
            Inspect_Variant (Choice.Variant);
         end loop;
      end Inspect_Variant;

   begin
      case T.Get.Kind is
         when Anonymous_Kind =>
            Res.Include (As_Anonymous_Typ (T).Named_Ancestor);
            if Transitive then
               Res.Union (Type_Dependencies
                            (As_Anonymous_Typ (T).Named_Ancestor, Transitive));
            end if;
         when Array_Typ_Range =>
            Res.Include (As_Array_Typ (T).Component_Type);
            if Transitive then
               Res.Union (Type_Dependencies
                            (As_Array_Typ (T).Component_Type, Transitive));
            end if;

            --  Index type are discrete types, and thus do not depend on
            --  any type.

            for Idx_Typ of As_Array_Typ (T).Index_Types loop
               Res.Include (Idx_Typ);
            end loop;
         when Non_Disc_Record_Kind =>
            for Comp_Typ of As_Nondiscriminated_Record_Typ (T).Component_Types
            loop
               Res.Include (Comp_Typ);
               if Transitive then
                  Res.Union (Type_Dependencies (Comp_Typ, Transitive));
               end if;
            end loop;
         when Disc_Record_Kind =>
            for Comp_Typ of As_Discriminated_Record_Typ (T).Component_Types
            loop
               Res.Include (Comp_Typ);
               if Transitive then
                  Res.Union (Type_Dependencies (Comp_Typ, Transitive));
               end if;
            end loop;

            --  Discriminant types are discrete types, and thus do not depend
            --  on any type.

            for Disc_Typ of As_Discriminated_Record_Typ (T).Discriminant_Types
            loop
               Res.Include (Disc_Typ);
            end loop;
            Inspect_Variant (As_Discriminated_Record_Typ (T).Variant);
         when others => null;
      end case;
      return Res;
   end Type_Dependencies;

   -----------------------------
   -- Support_Library_Package --
   -----------------------------

   function Support_Library_Package
     (Pack_Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      use Ada.Containers;
      Support_Pack_Name : Ada_Qualified_Name := Pack_Name.Copy;
   begin
      --  TODO: add particular cases for all reserved namespaces

      if Support_Pack_Name.Length = 1
         and then To_String (Support_Pack_Name.First_Element) = "standard"
      then
         Support_Pack_Name.Replace_Element (1, To_Unbounded_String ("TGen"));
      end if;
      Support_Pack_Name.Append (TGen.Strings.Ada_Identifier (+"TGen_Support"));
      return Support_Pack_Name;
   end Support_Library_Package;

   ------------------------------
   -- Generate_Support_Library --
   ------------------------------

   procedure Generate_Support_Library
     (Ctx        : Libgen_Context;
      Pack_Name  : Ada_Qualified_Name;
      Part       : Any_Library_Part)
   is
      F_Spec           : File_Type;
      F_Body           : File_Type;
      Ada_Pack_Name    : constant String := To_Ada (Pack_Name);
      Typ_Dependencies : Typ_Set;
      File_Name        : constant String :=
        Ada.Directories.Compose
          (Containing_Directory => To_String (Ctx.Output_Dir),
           Name                 => To_Filename (Pack_Name));

      Types : constant Types_Per_Package_Maps.Constant_Reference_Type :=
        Ctx.Types_Per_Package.Constant_Reference (Pack_Name);
   begin
      Create (F_Spec, Out_File, File_Name & ".ads");
      Put (F_Spec, "with TGen.Marshalling_Lib; ");
      Put_Line (F_Spec, "use TGen.Marshalling_Lib;");
      Put_Line (F_Spec, "with TGen.JSON;");
      Put (F_Spec, "with Interfaces; ");
      Put_Line (F_Spec, "use Interfaces;");
      Put_Line (F_Spec, "with Ada.Streams; use Ada.Streams;");

      Put_Line (F_Spec, "package " & Ada_Pack_Name & " is");
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
         use Ada_Identifier_Vectors;
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

      --  Disable predicate checks in the marshalling and unmarshalling
      --  functions.

      Put_Line
        (F_Body, "   pragma Assertion_Policy (Predicate => Ignore);");
      New_Line (F_Body);

      --  Generate the marshalling support lib

      if Part in Marshalling_Part | All_Parts then
         for T of Types loop
            if Is_Supported_Type (T.Get) then
               TGen.Marshalling.Binary_Marshallers
                 .Generate_Marshalling_Functions_For_Typ
                   (F_Spec, F_Body, T.Get, To_String (Ctx.Root_Templates_Dir));
               TGen.Marshalling.JSON_Marshallers
                 .Generate_Marshalling_Functions_For_Typ
                   (F_Spec, F_Body, T.Get, To_String (Ctx.Root_Templates_Dir));
            end if;
         end loop;
      end if;

      Put_Line (F_Body, "end " & Ada_Pack_Name & ";");
      Close (F_Body);
      Put_Line (F_Spec, "end " & Ada_Pack_Name & ";");
      Close (F_Spec);
   end Generate_Support_Library;

   ------------------
   -- Append_Types --
   ------------------

   procedure Append_Types
     (Source           : Typ_Set;
      Dest             : in out Types_Per_Package_Map;
      Ignore_Anonymous : Boolean := True)
   is
   begin
      for T of Source loop
         if not Ignore_Anonymous
           or else not (T.Get.Kind in Anonymous_Kind)
         then
            declare
               use Types_Per_Package_Maps;
               Pack_Name      : constant Ada_Qualified_Name :=
                 Support_Library_Package (T.Get.Compilation_Unit_Name);
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
      return (Output_Dir         => To_Unbounded_String (Output_Dir),
              Root_Templates_Dir => To_Unbounded_String (Actual_Templates_Dir),
              User_Project_Path  => To_Unbounded_String (User_Project_Path),
              others             => <>);
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
      Subp : LAL.Basic_Decl'Class;
      Diag : out Unbounded_String) return Boolean
   is
      use LAL;

      Spec : constant Base_Subp_Spec := Subp.P_Subp_Spec_Or_Null;
      --  Spec of the subprogram

      Subp_Types : Typ_Set;
      --  Transitive closure of required types for the parameters of the
      --  subprogram.

   begin
      for Param of Spec.P_Params loop
         declare
            Trans_Res : constant Translation_Result :=
              Translate (Param.F_Type_Expr);
            --  Translated type of the parameter

            Dummy_Cur : Typ_Sets.Cursor;
            Inserted  : Boolean;
            --  Whether the current type was already present in the set of
            --  types for which we will generate the support library.

         begin
            if not Trans_Res.Success then
               Diag := Trans_Res.Diagnostics;
               return False;
            end if;

            --  Include the param type in the set of types for which we want to
            --  generate the support library.

            Subp_Types.Insert (Trans_Res.Res, Dummy_Cur, Inserted);

            --  Get the transitive closure of the types on which this param's
            --  type depends that need to be included in the support library.
            --  Only do so if we actually inserted the type in the set to avoid
            --  recomputing transitive closures and doing set unions.

            if Inserted then
               Subp_Types.Union
                 (Type_Dependencies (Trans_Res.Res, Transitive => True));
            end if;
         end;
      end loop;

      --  Merge the set of types on which this subprogram's parameters depend
      --  on to the rest of the generation context.

      Append_Types (Subp_Types, Ctx.Types_Per_Package);
      return True;
   end Include_Subp;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (Ctx : Libgen_Context; Part : Any_Library_Part := All_Parts)
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
         Put_Line (Prj_File, "      for Default_Switches (""Ada"") use"
                             & " (""-gnatg"", ""-gnatyN"", ""-gnatws"");");
         Put_Line (Prj_File, "   end Compiler;");
         Put_Line (Prj_File, "end TGen_support;");
         Close (Prj_File);
      end;

      --  Generate all support packages

      for Cur in Ctx.Types_Per_Package.Iterate loop
         --  If all types are not supported, do not generate a support library

         if not (for all T of Element (Cur) => not Is_Supported_Type (T.Get))
         then
            Generate_Support_Library (Ctx, Key (Cur), Part);
         end if;
      end loop;
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

end TGen.Libgen;
