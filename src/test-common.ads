------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2011-2022, AdaCore                    --
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

--  This package contains some general-purpose entities that are used by many
--  GNATtest components

with Libadalang.Analysis; use Libadalang.Analysis;
with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with GNAT.OS_Lib;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.Projects;

with Ada.Sequential_IO;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with TGen.Context;

package Test.Common is

   package String_Set is new
     Ada.Containers.Indefinite_Ordered_Sets (String);
   use String_Set;

   Excluded_Test_Package_Bodies : String_Set.Set;
   Excluded_Test_Data_Files     : String_Set.Set;

   package List_Of_Strings is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Ada_Nodes_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Ada_Node);

   function Mangle_Hash_Full
     (Subp           : Ada_Node'Class;
      Case_Sensitive : Boolean := False;
      N_Controlling  : Boolean := False;
      For_Stubs      : Boolean := False) return String;
   --  Returns full hash for given subprogram

   function Mangle_Hash
     (Subp               : Ada_Node'Class;
      Unwind_Controlling : Boolean := True) return String;
   --  Returns the name of a given procedure or function with a hash code made
   --  of full ada names of all its parameters and result profile in case of
   --  a function.
   --  Unwind_Controlling defines wether name of controlling parameter type
   --  should be replaced with corresponding root type name or not.

   function Substring_16 (S : String) return String is
     (S (S'First .. S'First + 15));
   function Substring_6 (S : String) return String is
     (S (S'First .. S'First + 5));

   function Get_Nesting (Elem : Ada_Node'Class) return String;
   --  Returns the full package & protected prefix of the element

   function Nesting_Common_Prefix
     (Nesting_1, Nesting_2 : String) return String;
   --  Returns the common prefix of two nestings

   function Nesting_Difference
     (Nesting_1, Nesting_2 : String) return String;
   --  Returns difference in ending of two nestings without the first dot
   --  of the deeper nesting.

   function Node_Image (Node : Ada_Node'Class) return String is
     (Encode (Text (Node), Node.Unit.Get_Charset));
   --  Textual image of the node

   function Get_Subp_Name (Node : Ada_Node'Class) return String;
   --  if Subp is a subprigram declaration it will return subprogram's name;
   --  if Subp is an overloaded operator - it's text name

   function Enclosing_Unit_Name (Node : Ada_Node'Class) return String is
      (Node_Image (P_Top_Level_Decl (Node, Node.Unit).P_Defining_Name));
   --  Returns name of the compilation unit enclosing given node

   function Parent_Type_Declaration
     (Type_Dec : Base_Type_Decl) return Base_Type_Decl;
   --  Returns a corresponding parent type declaration for a given tagged type
   --  extension declaration.

   function Inheritance_Depth
     (Inheritance_Root_Type  : Base_Type_Decl;
      Inheritance_Final_Type : Base_Type_Decl)
      return Natural;
   --  Returns the number of derivations that lead from root type to final type

   function Root_Type_Declaration
     (Type_Dec : Base_Type_Decl) return Base_Type_Decl;
   --  Returns root type of the hierarchy

   function Is_Private (Node : Ada_Node'Class) return Boolean;
   --  Checks if Node is located in the private part of a package,
   --  a generic package, a task or protected  type or object declaration.
   --  If Declaration is located in the visible part of such a construct, but
   --  this enclosing construct is itself located in some private part
   --  (immediately or being nested in some other constructs), this function
   --  also returns True.

   function Abstract_Type (Decl : Base_Type_Decl) return Boolean;
   --  Returns true if the declared type is abstract

   function Is_Function (Decl : Basic_Decl) return Boolean;
   --  Returns True for function declarations, False for any unexpected
   --  arguments.

   procedure Check_Unit_For_Elaboration (CU : Compilation_Unit);
   --  Checks if given compilation unit has elaboration related pragmas or
   --  aspects and outputs corresponding warnings.

   procedure Report_Err (Message : String);
   --  Prints its argument to the standard error output
   procedure Report_Std (Message : String; Offset : Integer := 0);
   --  Prints its argument to the standard output. Silent in quiet mode
   package Char_Sequential_IO is new Ada.Sequential_IO (Character);
   Output_File : Char_Sequential_IO.File_Type;

   procedure Create_Dirs (Target_Dirs : File_Array_Access);
   --  Creates given directories

   procedure S_Put (Span : Natural; Text : String);
   --  Adds Span number spaces before the Text and prints it to Output_File

   procedure Create (Name : String);
   procedure Close_File;
   --  Wrappers for creating and closing output files

   procedure Put_New_Line;
   --  Puts a unix-style terminator to the Output_File disregard from the
   --  current actual platform.

   function Unit_To_File_Name (Old : String) return String;
   --  Replaces dots with "-" and lowers the case of the letters

   procedure Generate_Common_File;
   --  Creates a file with package gnattest_generated which denotes the default
   --  skeletons behavior and declares renamins necessary to avoid name
   --  conflicts with tested sources.

   procedure Put_Harness_Header;

   function First_Line_Number (Element : Ada_Node'Class) return Line_Number
     is (Element.Sloc_Range.Start_Line);
   function First_Column_Number (Element : Ada_Node'Class) return Column_Number
     is (Element.Sloc_Range.Start_Column);
   --  Returns the number on the first line/column of the element

   --------------------
   -- Stub exclusion --
   --------------------

   Default_Stub_Exclusion_List : String_Set.Set :=
     String_Set.Empty_Set;
   package String_To_String_Set is new
     Ada.Containers.Indefinite_Ordered_Maps (String, String_Set.Set);
   use String_To_String_Set;
   Stub_Exclusion_Lists : String_To_String_Set.Map    :=
     String_To_String_Set.Empty_Map;

   procedure Store_Default_Excluded_Stub (Excluded : String);
   --  Store data on units that should not be stubbed for all UUTs
   procedure Store_Excluded_Stub (Source : String; Excluded : String);
   --  Store data on units that should not be stubbed for given UUT

   ------------------------
   --  String constants  --
   ------------------------

   GT_Package : constant String := "gnattest";
   --  Name of tool specific package in the project file.

   Test_Routine_Prefix      : constant String := "Test_";
   --  Prefix to each test routine

   Wrapper_Prefix           : constant String := "Wrap_";

   Stub_Type_Prefix         : constant String := "Stub_Data_Type_";

   Stub_Object_Prefix       : constant String := "Stub_Data_";

   Setter_Prefix            : constant String := "Set_Stub_";

   Stub_Result_Suffix       : constant String := "_Result";

   Stub_Counter_Var         : constant String := "Stub_Counter";

   Test_Unit_Name           : constant String := "Tests";
   --  Name of test child package for non-primitive tests

   Test_Unit_Name_Suff      : constant String := "_Tests";
   --  Suffix for test packages that correspond to tagged record types

   Gen_Test_Unit_Name       : constant String := "Gen_Tests";
   --  Name of generic test child package for non-primitive tests

   Gen_Test_Unit_Name_Suff  : constant String := "_Gen_Tests";
   --  Suffix for generic test packages that correspond to tagged record types

   Inst_Test_Unit_Name      : constant String := "Inst_Tests";
   --  Name of instatiation test child package

   Test_Prj_Prefix          : constant String := "test_";
   --  Prefix of the output project file name

   Test_Data_Unit_Name      : constant String := "Test_Data";

   Test_Data_Unit_Name_Suff : constant String := "_Test_Data";

   Stub_Data_Unit_Name      : constant String := "Stub_Data";

   Stub_Project_Prefix      : constant String := "Stub_";

   TD_Prefix                : constant String := "Driver_";
   TD_Prefix_Overriden      : constant String := "VTE_Driver_";

   Hash_Version             : constant String := "2.2";

   Closure_Subdir_Name      : constant String := "tmp_gnattest_closure";

   GT_Marker_Begin   : constant String := "--  begin read only";
   GT_Marker_End     : constant String := "--  end read only";

   Stub_Dir_Name     : GNAT.OS_Lib.String_Access := new String'
     ("gnattest" & GNAT.OS_Lib.Directory_Separator & "stubs");

   Test_Subdir_Name  : String_Access;
   --  Name of subdirectory to place test files in case of --sudbir option

   Separate_Root_Dir : String_Access;
   --  The root directory to place the test file hierarchy in case of
   --  --separate-root option.

   Test_Dir_Name     : GNAT.OS_Lib.String_Access := new String'
     ("gnattest" & GNAT.OS_Lib.Directory_Separator & "tests");
   --  Name of default directory to place test files

   Source_Project_Tree : GNATCOLL.Projects.Project_Tree;
   --  Source project file name. Used for extraction of source
   --  and project files.

   Generate_Separates : Boolean := False;

   Stub_Mode_ON : Boolean := False;

   Transition : Boolean := False;

   Omit_Sloc : Boolean := False;

   Harness_Dir_Str : GNAT.OS_Lib.String_Access := new String'
     ("gnattest" & GNAT.OS_Lib.Directory_Separator & "harness");

   Skeletons_Fail : Boolean := True;

   IDE_Package_Present : Boolean := False;
   Make_Package_Present : Boolean := False;

   Tmp_Test_Prj : GNAT.OS_Lib.String_Access := null;

   Reporter_Name : GNAT.OS_Lib.String_Access := new String'("gnattest");

   No_Command_Line : Boolean := False;

   Harness_Only : Boolean := False;

   Add_Exit_Status : Boolean := False;
   --  When true, generated test driver will set exit status according to
   --  the outcome of tests.

   Driver_Per_Unit : Boolean := True;

   Show_Passed_Tests : Boolean := True;
   --  Distinguishes the default output of passed tests

   Show_Test_Duration : Boolean := False;
   --  When true, AUnit_Options.Test_Case_Timer is set to True in test runner

   RTS_Path : GNAT.OS_Lib.String_Access := new String'("");
   RTS_Attribute_Val : GNAT.OS_Lib.String_Access;

   Target_Val  : GNAT.OS_Lib.String_Access;

   Has_Test_Cases : Boolean := False;

   Separate_Drivers : Boolean := False;
   --  When true, multiple test drivers willbe generated

   Additional_Tests_Prj : GNAT.OS_Lib.String_Access := null;

   Gnattest_Generated_Present : Boolean := False;
   --  Indicates if any of the source projects already have
   --  gnattest_generated.ads so that it won't be duplicated.

   Inherited_Switches : List_Of_Strings.List := List_Of_Strings.Empty_List;

   Relocatable_Harness : Boolean := False;

   Inheritance_To_Suite : Boolean := True;
   --  Whether or not to add inherited tests that correspond to inherited
   --  primitives to the test suite for descendant type.

   Substitution_Suite   : Boolean := False;
   --  Whenever or not to genretate suites for overrden tests applying them
   --  to fixture containing object of descendant type.

   Test_Case_Only : Boolean := False;
   --  Whether test skeletons should be created only for subprograms with
   --  associated Test_Case pragmas/aspects.

   Verbose : Boolean := False;
   --  Turns on additional verbose output and more detailed traces

   Queues_Number : Positive := 1;
   --  Number of test drivers run in parallel in aggregation mode

   Environment_Dir : GNAT.OS_Lib.String_Access := null;
   --  Designates a directory whose content should be copied to the test driver
   --  spawn directories to solve potential issues like loading a file with
   --  a hardcoded relative path.

   Aggregate_Subdir_Name : GNAT.OS_Lib.String_Access := new String'("");
   --  Used to prepend the names of test driver executables in
   --  test_drivers.list.

   Quiet : Boolean := False;
   --  Supresses non-critical output

   Strict_Execution : Boolean := False;
   --  Indicates whether exit status should depend on invalid sources detected

   Source_Processing_Failed : Boolean := False;
   --  Indicates whether at least one of sources was either rejected by
   --  lal parser or an unpredicted error happened during its processing.

   Generate_Test_Vectors : Boolean := False;
   --  Indicates that we should use TGen to generate tests vectors for the
   --  supported subprograms.

   JSON_Test_Dir : String_Access;
   --  Dir in which the test vector in json format should be stored / looked up

   TGen_Ctx : TGen.Context.Generation_Context;
   --  Shared context for test vectors generation

   TGen_Num_Tests : Positive := 5;
   --  Number of tests to be generated for each procedure

end Test.Common;
