------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2025, AdaCore                    --
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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Preprocessing;

with TGen.Context;
with TGen.Marshalling;
with TGen.Parse_Strategy; use TGen.Parse_Strategy;
with TGen.Strings;
with TGen.Types;          use TGen.Types;
with TGen.Wrappers;       use TGen.Wrappers;

package TGen.Libgen is
   package LAL renames Libadalang.Analysis;

   type Library_Parts is
     (Marshalling_Part, Test_Generation_Part, Wrappers_Part);
   type Any_Library_Part is array (Library_Parts) of Boolean;
   All_Parts : constant Any_Library_Part :=
     [Marshalling_Part     => True,
      Test_Generation_Part => True,
      Wrappers_Part        => True];

   type Ada_Language_Version is (Unspecified, Ada_12, Ada_22);
   --  Ada language versions

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

   function Supported_Subprogram
     (Subp : LAL.Basic_Decl'Class) return Typ_Access;
   --  If the Subp is a supported subprogram profile, return a Function_Typ,
   --  otherwise return an Unsupported_Typ with the reason why it is not
   --  supported inlined in the Unsupported_Typ.Reason field.

   function Include_Subp
     (Ctx                                : in out Libgen_Context;
      Subp                               : LAL.Basic_Decl'Class;
      Diags                              :
        out TGen.Strings.String_Vectors.Vector;
      Is_Top_Level_Generic_Instantiation : Boolean := False) return Boolean;
   --  Register all the types in the parameters of Subp in the set of types for
   --  which the marshalling library will be generated. This procedures does
   --  not actually generate any sources, call Generate to create the support
   --  library for all the registered types.
   --
   --  Returns False if there is an error translating some of the parameter
   --  types, or if some of the types are unsupported for marshalling,
   --  and report diagnostics in Diags. In that case, the context is not
   --  modified. Otherwise, Diags should be ignored.
   --
   --  The `Is_Generic_Instantiation_Only` switch is used when a subp comes
   --  from a top level generic instantiation. This flag is required because
   --  it triggers the generation of a wrapper package to allow child packages.

   procedure Generate
     (Ctx : in out Libgen_Context; Part : Any_Library_Part := All_Parts);
   --  Output all of the support library files

   function Generate
     (Ctx   : in out Libgen_Context;
      Subp  : LAL.Basic_Decl'Class;
      Diags : out TGen.Strings.String_Vectors.Vector;
      Part  : Any_Library_Part := All_Parts) return Boolean;
   --  Shortcut for
   --
   --     if Include_Subp (Ctx, Subp, Diag) then
   --       Generate (Ctx);
   --     else
   --        return False;
   --     end if;
   --
   --  Beware that calling this procedure multiple times on different
   --  procedures may result in overwriting of previously generated files if
   --  the output directories defined in Ctx are the same across calls.

   function Required_Support_Packages
     (Ctx : Libgen_Context; Unit_Name : TGen.Strings.Ada_Qualified_Name)
      return TGen.Strings.Ada_Qualified_Name_Set;
   --  Return a reference to the set of support packages that need to be
   --  "withed" to be used with the types used in Unit_Name.

   type Default_Strat_Kind is (Stateful, Stateless);

   function Is_Generation_Required (Ctx : Libgen_Context) return Boolean;
   --  Return whether the given context require test case vector generation.
   --  This returns False when every included subprogram is not supported by
   --  TGen, or when no subprogram was included.

   procedure Write_Preprocessor_Config
     (Ctx          : Libgen_Context;
      Prj_File     : Ada.Text_IO.File_Type;
      Append_Flags : Boolean := True);
   --  Generate a preprocessor file from the context and enable pre-processing
   --  in the given Project_File by adding `-gnatep=<file>`.

   procedure Generate_Harness
     (Ctx              : in out Libgen_Context;
      Harness_Dir      : String;
      Test_Output_Dir  : String;
      Default_Strat    : Default_Strat_Kind := Stateless;
      Default_Test_Num : Natural := 5;
      Bin_Tests        : Boolean := False);
   --  Generate a harness, in Harness_Dir, to generate values for the various
   --  subprograms to test as registered in Ctx. The JSON test files will be
   --  generated by the harness under Test_Output_Dir, and will contain
   --  Default_Test_Num tests.

   function Get_Array_Size_Limit return Positive
   renames TGen.Marshalling.Get_Array_Size_Limit;
   --  Return the size beyond which the marshallers will give up trying to load
   --  arrays, to avoid allocating overly-large arrays on the stack.
   --
   --  The default value is 1000, but this can be overridden either through the
   --  TGEN_ARRAY_LIMIT environment variable, or the Set_Array_Size_Limit
   --  procedure. The latter takes precedence over the former.

   procedure Set_Array_Size_Limit (Limit : Positive);
   --  Set the array size limit beyond which marshallers will give up reading
   --  array values, to avoid allocating overly large arrays on the stack.
   --
   --  If used, this will override any value set through the TGEN_ARRAY_LIMIT
   --  environment variable.
   --
   --  This can only be called prior to the first call to Include_Subp or
   --  Supported_Subprogram to ensure consistency of the array limit used in
   --  all the marshallers, otherwise Constraint_Error is raised.

   procedure Set_Preprocessing_Definitions
     (Ctx  : out Libgen_Context;
      Data : Libadalang.Preprocessing.Preprocessor_Data);
   --  Set preprocessor definitions to the context.

   procedure Set_Minimum_Lang_Version
     (Ctx : in out Libgen_Context; Version : Ada_Language_Version);
   --  Set the desired language version to be used in project compilation
   --  switches. Note that if not set, no languages version switches will be
   --  added to the generated project files.
   --
   --  TGen also requires at least an Ada_2012 compiler to be available, so
   --  setting a version lower than that will result in -gnat12 being used in
   --  the compiler switches, instead of the specified language version.

   function Get_Output_Dir (Ctx : Libgen_Context) return String;
   --  Get TGgen's support library output directory

   function Generic_Package_Name
     (Pack_Name     : TGen.Strings.Ada_Qualified_Name;
      Replace_First : Boolean := False) return TGen.Strings.Ada_Qualified_Name
   with Pre => (not Pack_Name.Is_Empty);
   --  Add generic instantiation prefix to a qualified name. By default, the
   --  function will change the last identifier of a fully qualified name, the
   --  `Replace_First` switch can be used to replace the first one instead.

   function Pack_Is_Top_Level_Instantiation
     (Ctx : Libgen_Context; Pack_Name : TGen.Strings.Ada_Qualified_Name)
      return Boolean;
   --  Return if the package is a top level generic instantiation

   procedure Create_Generic_Wrapper_Package_If_Not_Exists
     (Unit_Name : String; Base_Name : String; Output_Dir : String);
   --  Create a wrapper package for top level generic instantiations. In
   --  case a of top level generic instantiation, this wrapper allows
   --  creation of child packages. The only limitation of this approach
   --  is that private generic subprograms can't be accessed which is why
   --  they're not supported.
   --
   --  `Pack_Name` being the wrapper fully qualified name and `Base_Name`
   --  the library level instantiation fully qualified name.

   function Get_Test_Case_Dump_Procedure_Name
     (Ctx              : Libgen_Context;
      Parent_Pack_Name : TGen.Strings.Ada_Qualified_Name;
      Subp_FQN         : Unbounded_String) return Unbounded_String
   with Pre => not Parent_Pack_Name.Is_Empty;
   --  Returns an Unbounded_String corresponding to the procedure name used
   --  to dump parameters for a given subprogram's fully qualified name. If no
   --  test cases correspond to the given subprogram, this function returns
   --  `Null_Unbounded_String`.
   --
   --  `Pack_Name` represents the subprogram's parent package name, and
   --  `Subp_FQN` refers to the fully qualified name of the tested subprogram.
   --
   --  This function may raise a `Program_Error` if the requested subprogram is
   --  not present in the TGen context. This typically occurs if the analysis
   --  has not been performed yet, the requested subprogram does not exist, or
   --  it is not supported for test case generation.

   function JSON_Marshalling_Enabled return Boolean;
   --  Returns whether JSON marshalling code should be emitted in generated
   --  harness. Some platform with limited Ada runtime support are not suitable
   --  for JSON serialization. This switch can be enabled by setting the
   --  `TGEN_NO_JSON_MARSHALLING` environment variable.

private
   use TGen.Strings;
   use TGen.Context;

   package Types_Per_Package_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Ada_Qualified_Name,
        Element_Type => Typ_Set,
        "="          => Typ_Sets."=");

   subtype Types_Per_Package_Map is Types_Per_Package_Maps.Map;

   package Subp_Info_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Subp_Information);
   subtype Subp_Info_Vector is Subp_Info_Vectors.Vector;

   package Subp_Info_Vectors_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Ada_Qualified_Name,
        Element_Type => Subp_Info_Vector,
        "="          => Subp_Info_Vectors."=");
   subtype Subp_Info_Vectors_Map is Subp_Info_Vectors_Maps.Map;

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

      Included_Subps : Subp_Info_Vectors_Map;
      --  Map of package name to list of subprograms included. We store some
      --  information to be able to retrieve the subprogram specification +
      --  the precondition.

      Array_Index_Types : Typ_Set;
      --  Set of types used to instantiate array index constraints

      Preprocessor_Definitions : Libadalang.Preprocessing.Preprocessor_Data;
      --  Preprocessor defintions to add in projects files

      Has_Preprocessor_Config : Boolean;
      --  Whether the context has preprocessing definitions

      Lang_Version : Ada_Language_Version := Unspecified;
      --  Language version to be used in the compilation switches of the
      --  generated projects. If Unspecified, no language version switch will
      --  be added to the projects.
   end record;

end TGen.Libgen;
