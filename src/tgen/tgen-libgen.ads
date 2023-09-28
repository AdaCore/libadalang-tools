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
--
--  Main entry point to generate the support library (marshalling and test
--  generation).

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Libadalang.Analysis;

with TGen.Context;
with TGen.Strings;
with TGen.Parse_Strategy; use TGen.Parse_Strategy;

package TGen.Libgen is
   package LAL renames Libadalang.Analysis;

   type Any_Library_Part is
     (Marshalling_Part, Test_Generation_Part, All_Parts);
   --  Parts of the support library that can be generated. At the moment only
   --  Marshalling is implemented, with Test_Generation coming soon.

   type Libgen_Context is private;

   function Create
     (Output_Dir         : String;
      User_Project_Path  : String;
      Root_Templates_Dir : String := "") return Libgen_Context;
   --  Initialize a context object.
   --  Output dir is the path to the directory in which the generated library
   --  will be generated, Root_Templates_Dir is the path to the root dir of
   --  the TGen templates, User_Project_Path is a path (relative or absolute)
   --  to the user project, which will be with'ed in the generated project
   --  file.
   --
   --  If Root_Templates_Dir is the empty string, try to locate the templates
   --  in their canonical installation path, i.e.
   --  <executable_location>/../share/tgen/templates.

   function Create
     (Output_Dir         : GNATCOLL.VFS.Virtual_File;
      User_Project_Path  : GNATCOLL.VFS.Virtual_File;
      Root_Templates_Dir : GNATCOLL.VFS.Virtual_File) return Libgen_Context;
   --  Same as above, but with virtual files

   function Include_Subp
     (Ctx  : in out Libgen_Context;
      Subp : LAL.Basic_Decl'Class;
      Diag : out Unbounded_String) return Boolean;
   --  Register all the types in the parameters of Subp in the set of types for
   --  which the marshalling library will be generated. This procedures does
   --  not actually generate any sources, call Generate to create the support
   --  library for all the registered types.
   --
   --  Returns False if there is an error translating some of the parameter
   --  types, or if some of the types are unsupported for marshalling,
   --  reporting a diagnostic string in Diag. In that case, the context
   --  is not modified. Otherwise, Diag should be ignored.

   procedure Generate
     (Ctx : in out Libgen_Context; Part : Any_Library_Part := All_Parts);
   --  Output all of the support library files

   function Generate
     (Ctx  : in out Libgen_Context;
      Subp : LAL.Basic_Decl'Class;
      Diag : out Unbounded_String;
      Part : Any_Library_Part := All_Parts) return Boolean;
   --  Shortcut for
   --
   --     if Include_Subp (Ctx, Subp, Diags) then
   --       Generate (Ctx);
   --     else
   --        return False;
   --     end if;
   --
   --  Beware that calling this procedure multiple times on different
   --  procedures may result in overwriting of previously generated files if
   --  the output directories defined in Ctx are the same across calls.

   function Required_Support_Packages
     (Ctx       : Libgen_Context;
      Unit_Name : TGen.Strings.Ada_Qualified_Name)
      return TGen.Strings.Ada_Qualified_Name_Sets_Maps.Constant_Reference_Type;
   --  Return a reference to the set of support packages that need to be
   --  "withed" to be used with the types used in Unit_Name.

   type Default_Strat_Kind is (Stateful, Stateless);

   procedure Generate_Harness
     (Ctx              : in out Libgen_Context;
      Harness_Dir      : String;
      Test_Output_Dir  : String;
      Default_Strat    : Default_Strat_Kind := Stateless;
      Default_Test_Num : Natural := 5);
   --  Generate a harness, in Harness_Dir, to generate values for the various
   --  subprograms to test as registered in Ctx. The JSON test files will be
   --  generated by the harness under Test_Output_Dir, and will contain
   --  Default_Test_Num tests.

private
   use TGen.Strings;
   use TGen.Context;

   package Types_Per_Package_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Ada_Qualified_Name,
         Element_Type => Typ_Set,
         "="          => Typ_Sets."=");

   subtype Types_Per_Package_Map is Types_Per_Package_Maps.Map;

   type Libgen_Context is record
      Output_Dir : Unbounded_String;
      --  Directory in which the support library files will be generated

      Root_Templates_Dir : Unbounded_String;
      --  Root directory in which templates can be found

      User_Project_Path : Unbounded_String;
      --  Path (absolute or relative) to the original user project file

      Types_Per_Package : Types_Per_Package_Map;
      --  Map of package name to set of types for which we want to generate
      --  the support library.

      Support_Packs_Per_Unit : TGen.Strings.Ada_Qualified_Name_Sets_Map;
      --  Map of the required support packages for each processed unit

      Strat_Types_Per_Package : Types_Per_Package_Map;
      --  Map of package name to set of types for which we want to generate
      --  the type introspection library.

      Strategy_Map : FQN_To_Parsed_Strat_Maps.Map;
      --  List of specified strategies. They map a fully qualified name (e.g.
      --  a parameter / a parameter component, a type / a type component) to
      --  its specified strategy.

      Imports_Per_Unit : TGen.Strings.Ada_Qualified_Name_Sets_Map;
      --  Set of "withed" unit for a given unit name.

      Lib_Support_Generated : Boolean;
      --  Whether the lib support was already generated or not.

      Generation_Map : Types_Per_Package_Map;
      --  Map of generation unit names to function types for which we should
      --  create a value generation harness.

   end record;

end TGen.Libgen;
