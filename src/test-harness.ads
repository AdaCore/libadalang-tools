------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2011-2021, AdaCore                    --
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

with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Ada.Containers.Indefinite_Vectors;

with Libadalang.Analysis; use Libadalang.Analysis;

with Test.Common; use Test.Common;

package Test.Harness is

   --------------------
   --  Data Storing  --
   --------------------

   type Test_Type_Info is record
      Test_Type             : Ada_Node := No_Ada_Node;
      Test_Type_Name        : String_Access := null;
      Good_For_Substitution : Boolean := False;

      Max_Inheritance_Depth : Natural := 0;
      --  Inheritance depth of a test routine is a number of packages
      --  in the hierarchy of the test type between the original
      --  declaration and the current overriding declaration plus one.
      --  So if the test routine is overrifen right in the next test
      --  package, it's inheritance depth will be 1.
      --  Max_Inheritance_Depth indicates the maximum of all inheritance
      --  depths of the test routines in the LTR_List that are primitives
      --  of the given test type..

      Nesting : String_Access;

      --  Following component used only in full mode
      Tested_Type         : Ada_Node;
   end record;

   package TT_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Type_Info);
   use TT_Info;

   type Test_Routine_Info is tagged record
      TR_Declaration : Ada_Node;
      TR_Text_Name   : String_Access;
      Test_Type_Numb : Positive;
      Nesting        : String_Access;

      Tested_Sloc    : String_Access := null;
   end record;

   package TR_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Routine_Info);

   use TR_Info;

   type Test_Routine_Info_Enhanced is new Test_Routine_Info with record
      TR_Parent_Unit_Decl : Compilation_Unit;
      TR_Rarent_Unit_Name : String_Access;

      --  Following components used only in full mode
      Inheritance_Depth   : Natural;
      Tested_Type         : Ada_Node;

   end record;

   package TR_Info_Enhanced is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Routine_Info_Enhanced);

   use TR_Info_Enhanced;

   type Test_Case_Info is record
      Name    : String_Access;
      Nesting : String_Access;
   end record;

   package TC_Info is new
     Ada.Containers.Indefinite_Vectors (Positive, Test_Case_Info);

   use TC_Info;

   type Data_Kind_Type is
     (Declaration_Data,
      Instantination_Data);

   type Data_Holder (Data_Kind : Data_Kind_Type := Declaration_Data) is record

      Test_Unit : Compilation_Unit;
      --  The CU under consideration.

      Test_Unit_Full_Name : String_Access := null;
      --  Fully expanded Ada name of the CU under consideration.

      Test_Unit_File_Name : String_Access := null;
      --  Name of file containing the CU under consideration.

      case Data_Kind is
         --  Indicates which data storing structures are used, determines the
         --  way of suite generation.

         when Declaration_Data =>

            Test_Types : TT_Info.Vector;
            --  List of test types

            TR_List  : TR_Info.Vector;
            --  List of test routines declared in the test package

            ITR_List : TR_Info_Enhanced.Vector;
            --  List of test routines inherited from packages declaring
            --  predecessing test types.

            LTR_List : TR_Info_Enhanced.Vector;
            --  List of test routines overriden in current package

            TC_List : TC_Info.Vector;
            --  List of test_case types in current package

            --  Flags:
            Generic_Kind         : Boolean := False;
            --  On, when the given package is generic

            Good_For_Suite       : Boolean := False;
            --  The suite should be generated

            Good_For_Substitution       : Boolean := False;
            --  Substitution suite should be generated

         when Instantination_Data =>

            Gen_Unit : Compilation_Unit := No_Compilation_Unit;
            --  Generic CU that is instatinated into the given one.

            Gen_Unit_Full_Name : String_Access := null;
            --  Fully expanded Ada name of the generic CU.

            Gen_Unit_File_Name : String_Access := null;
            --  Name of file containing the generic CU.

      end case;

   end record;

   procedure Generate_Suite (Data : Data_Holder; Path : String := "");
   --  Creates a test suites for both the directly declared
   --  tests and inherited ones.

   procedure Generate_Substitution_Suite_From_Tested
     (Data : Data_Holder; Path : String := "");
   --  Generates substitution suite from data gathered during skeleton
   --  generation.

   procedure Generate_Test_Drivers
     (Data      : Data_Holder;
      UUT       : String;
      Stub_List : Ada_Nodes_List.List);
   --  For the given UUT generates a set of independent test driver mains,
   --  one per subprogram under test, inherited or declared.

   procedure Generate_Substitution_Test_Drivers (Data : Data_Holder);
   --  For given UUT generates a set of independent test driver mains, one per
   --  overriden subprogram. Each test driver may contain one to several
   --  tests: one for overriden operation, and if it is an inherited one,
   --  one for each of the inherited ones for ancestor types right up to the
   --  original declaration.
   --  Can only be called for separate drivers mode, not for stub mode.

   procedure Generate_Stub_Test_Driver_Projects (Source_Prj : String);
   --  Generates all project files necessary for separate test drivers in stub
   --  mode.

   procedure Generate_Test_Driver_Projects (Source_Prj : String);
   --  Generates all project files necessary for separate test drivers.

   procedure Generate_Makefile (Source_Prj : String);
   --  Generate Makefile with relevant coverage targets

   procedure Test_Runner_Generator (Source_Prj : String);
   --  Generates Main_Sute and Test_Runner

   procedure Project_Creator (Source_Prj : String);
   --  Generates a simple project file for the test driver

   procedure Process_Source (The_Unit : Analysis_Unit);
   --  Processes source for harness-only and additional-tests modes, generates
   --  suite packages.

end Test.Harness;
