------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . H A R N E S S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;          use GNATCOLL.VFS_Utils;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with Utils.Command_Lines;         use Utils.Command_Lines;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;

with Test.Skeleton.Source_Table;
with Test.Mapping;                use Test.Mapping;
with Test.Harness.Source_Table;   use Test.Harness.Source_Table;

with Libadalang.Common;           use Libadalang.Common;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

package body Test.Harness is

   Me : constant Trace_Handle := Create ("Harness", Default => Off);

   Harness_Dir : String_Access renames Harness_Dir_Str;

   use List_Of_Strings;
   Suit_List : List_Of_Strings.List;
   --  Storing the names of all suits

   type Separate_Project_Info is record
      Name_TD          : String_Access := null;
      Name_Extending   : String_Access := null;
      Path_TD          : String_Access := null;
      Path_Extending   : String_Access := null;
      Name_Of_Extended : String_Access := null;
      Path_Of_Extended : String_Access := null;
      Main_File_Name   : String_Access := null;
      Stub_Source_Dir  : String_Access := null;
      Test_Package     : String_Access := null;
      Test_Data        : String_Access := null;
      UUT_File_Name    : String_Access := null;
      Sources_List     : List_Of_Strings.List := List_Of_Strings.Empty_List;
   end record;

   package Separate_Project_Info_Vectors is new
     Ada.Containers.Vectors (Positive, Separate_Project_Info);
   use Separate_Project_Info_Vectors;

   Separate_Projects : Separate_Project_Info_Vectors.Vector;

   ------------------------
   --  String constants  --
   ------------------------

   --  Unit names:

   Common_Suite_Name          : constant String := "Suite";
   --  Suffixless name of the unit containing a common suite

   Generic_Suite_Name         : constant String := "Gen_Suite";
   --  Suffixless name of the unit containing a generic suite

   Substitution_Suite_Name    : constant String := "Substitution_Suite";
   --  Suffixless name of the unit containing substitution suite

   Generic_Substitution_Suite_Name  : constant String
     := "Gen_Substitution_Suite";
   pragma Unreferenced (Generic_Substitution_Suite_Name);
   --  Suffixless name of the unit containing a generic substitution suite

   Instant_Suite_Name         : constant String := "Suite_Inst";
   pragma Unreferenced (Instant_Suite_Name);
   --  Suffixless name of the unit containing instantination suite

   Substitution_Instant_Suite_Name  : constant String
     := "Substitution_Suite_Inst";
   pragma Unreferenced (Substitution_Instant_Suite_Name);
   --  Suffixless name of the unit containing instantination suite

   --  Infrastructure elements:

   Test_Case_Prefix           : constant String := "Case_";
   --  Prefix to Test_Case variables' names

   Main_Suite_Name            : constant String := "Gnattest_Main_Suite";
   --  Suffixless name of the unit containing the main suite

   Test_Runner_Name           : constant String := "Test_Runner";
   --  Suffixless name of the unit containing the test runner

   procedure Generate_Common_Harness_Files (Source_Prj : String);
   --  Generates aggregate project and a makefile for separate drivers
   --  and test_drivers.list.

   procedure Generate_Global_Config_Pragmas_File;
   --  Generates files containing pragmas suppressing pre and postconditions
   --  and possibly manipulating ghost policy.

   procedure Generate_Gnattest_Common_Prj;
   --  Generates abstract project file gnattest_common that contains different
   --  attributes relevant to the harness.

   function Gnattest_Common_Prj_Name return String is
     (Harness_Dir.all & Directory_Separator & "gnattest_common.gpr");

   function Positive_Image (P : Positive) return String is
      (Trim (Positive'Image (P), Both));
   --  Returns a trimmed image of the argument

   Infix : Natural := 0;
   function Get_Next_Infix return String;
   --  Returns a numbered infix ("1_", "2_",..), increasing the number for
   --  each call.

   procedure Gather_Data
     (The_Unit          :     Compilation_Unit;
      Data              : out Data_Holder;
      Appropriate_Source : out Boolean);
   --  Iterates through the given unit and sets the values of Main_Type and
   --  Subp_List. All the iterations are done here.
   --  Checks if given unit is of the right kind and if it is appropriate.
   --  Marks unappropriate sources in the source table.

   function Is_AUnit_Part (Unit : Analysis_Unit) return Boolean;
   --  Checks if the unit under consideration is a part of AUnit library itself

   function Is_Test_Routine (Subp : Basic_Subp_Decl) return Boolean;
   --  Indicates if the given Subprogram is a test routine, which means it
   --  has only one parameter whose type is a descendant of AUnit test type.
   --  Also returns False for Set_Up, Set_Up_Case, Tear_Down, Tear_Down_Case.

   function Is_Test_Fixture_Routine (Subp : Basic_Subp_Decl) return Boolean;
   --  Same as above, but only returns true if the ancestor test type is
   --  Test_Fixture.

   function Is_Test_Related (Subp : Basic_Subp_Decl) return Boolean;
   --  Indicates if the given Subprogram is of interest, that is a test routine
   --  or Set_Up/Tear_Down etc.

   -----------------------------------------
   -- Generate_Global_Config_Pragmas_File --
   -----------------------------------------

   procedure Generate_Global_Config_Pragmas_File is
   begin
      if not Is_Regular_File (Harness_Dir.all & "suppress.adc") then
         Create (Harness_Dir.all & "suppress.adc");
         S_Put (0, "pragma Assertion_Policy (Pre => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Post => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Ghost => Check);");
         Put_New_Line;
         Close_File;
      end if;

      if not Is_Regular_File (Harness_Dir.all & "suppress_no_ghost.adc") then
         Create (Harness_Dir.all & "suppress_no_ghost.adc");
         S_Put (0, "pragma Assertion_Policy (Pre => Ignore);");
         Put_New_Line;
         S_Put (0, "pragma Assertion_Policy (Post => Ignore);");
         Put_New_Line;
         Close_File;
      end if;
   end Generate_Global_Config_Pragmas_File;

   ----------------------------------
   -- Generate_Gnattest_Common_Prj --
   ----------------------------------

   procedure Generate_Gnattest_Common_Prj is
      Gnattest_Common_Prj : constant String := Gnattest_Common_Prj_Name;
   begin
      if Is_Regular_File (Gnattest_Common_Prj) then
         return;
      end if;
      Create (Gnattest_Common_Prj);

      S_Put (0, "abstract project Gnattest_Common is");
      Put_New_Line;
      S_Put (3, "for Languages use (""Ada"");");
      Put_New_Line;
      S_Put (3, "for Source_Files use ();");
      Put_New_Line;

      if RTS_Path.all /= "" then
         if RTS_Attribute_Val = null then
            S_Put (3, "for Runtime (""Ada"") use """ &
                     RTS_Path.all & """;");
         else
            S_Put
              (3,
               "for Runtime (""Ada"") use """ & RTS_Attribute_Val.all & """;");
         end if;
         Put_New_Line;
      end if;

      --  Need to treat Target the same way.

      Put_New_Line;
      S_Put
        (3,
         "type TD_Compilation_Type is (""contract-checks"","
         & """no-contract-checks"", ""no-config-file"");");
      Put_New_Line;
      if Has_Test_Cases then
         S_Put
           (3,
            "TD_Compilation : TD_Compilation_Type := external "
            & "(""TEST_DRIVER_BUILD_MODE"", ""contract-checks"");");
      else
         S_Put
           (3,
            "TD_Compilation : TD_Compilation_Type := external "
            & "(""TEST_DRIVER_BUILD_MODE"", ""no-config-file"");");
      end if;
      Put_New_Line;

      Put_New_Line;
      S_Put (3, "package Builder is");
      Put_New_Line;
      S_Put (6, "case TD_Compilation is");
      Put_New_Line;
      S_Put (9, "when ""contract-checks"" =>");
      Put_New_Line;
      S_Put (12, "for Global_Configuration_Pragmas use ""suppress.adc"";");
      Put_New_Line;
      S_Put (9, "when ""no-contract-checks"" =>");
      Put_New_Line;
      S_Put
        (12,
         "for Global_Configuration_Pragmas use ""suppress_no_ghost.adc"";");
      Put_New_Line;
      S_Put (9, "when ""no-config-file"" =>");
      Put_New_Line;
      S_Put (12, "null;");
      Put_New_Line;
      S_Put (6, "end case;");
      Put_New_Line;
      S_Put (3, "end Builder;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Linker is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use (""-g"");");
      Put_New_Line;
      S_Put (3, "end Linker;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Binder is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use (""-E"", ""-static"");");
      Put_New_Line;
      S_Put (3, "end Binder;");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "Contract_Switches := ();");
      Put_New_Line;
      S_Put (3, "case TD_Compilation is");
      Put_New_Line;
      S_Put (6, "when ""contract-checks"" =>");
      Put_New_Line;
      S_Put (9, "Contract_Switches := (""-gnata"");");
      Put_New_Line;
      S_Put (6, "when others =>");
      Put_New_Line;
      S_Put (9, "null;");
      Put_New_Line;
      S_Put (3, "end case;");
      Put_New_Line;

      S_Put (3, "package Compiler is");
      Put_New_Line;
      S_Put (6, "for Default_Switches (""ada"") use");
      Put_New_Line;
      S_Put (8, "(""-g"", ""-gnatyM0""");
      declare
         Cur : List_Of_Strings.Cursor := Inherited_Switches.First;
      begin
         loop
            exit when Cur = List_Of_Strings.No_Element;
            S_Put (0, ", """ & List_Of_Strings.Element (Cur) & """");
            List_Of_Strings.Next (Cur);
         end loop;
         Inherited_Switches.Clear;
      end;
      S_Put (0, ") & Contract_Switches;");
      Put_New_Line;
      S_Put (3, "end Compiler;");
      Put_New_Line;
      Put_New_Line;

      if Stub_Mode_ON or else Separate_Drivers then
         S_Put (3, "package Ide is");
         Put_New_Line;
         S_Put (3, "end Ide;");
         Put_New_Line;
         S_Put (3, "package Make is");
         Put_New_Line;
         S_Put (3, "end Make;");
         Put_New_Line;
      end if;

      S_Put (0, "end Gnattest_Common;");
      Put_New_Line;
      Close_File;
   end Generate_Gnattest_Common_Prj;

   -----------------------------
   --  Test_Runner_Generator  --
   -----------------------------

   procedure Test_Runner_Generator (Source_Prj : String) is
      Iterator : List_Of_Strings.Cursor;
   begin
      if List_Of_Strings.Is_Empty (Suit_List) then
         Report_Std
           ("gnattest: no test skeletons generated because "
            & "no subprogram to test");
         Report_Std
           ("found in project " & Source_Prj, 10);
         Cmd_Error_No_Help ("cannot create main suite and test runner");
      end if;

      --  creating main suite spec
      Create (Harness_Dir.all & Unit_To_File_Name (Main_Suite_Name) & ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "package " & Main_Suite_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & Main_Suite_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  creating main suite body
      Create
        (Harness_Dir.all & Unit_To_File_Name (Main_Suite_Name) & ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      Iterator := List_Of_Strings.First (Suit_List);
      loop
         exit when Iterator = List_Of_Strings.No_Element;

         S_Put
           (0,
            "with " & List_Of_Strings.Element (Iterator) & ";");
         Put_New_Line;

         List_Of_Strings.Next (Iterator);
      end loop;

      Put_New_Line;
      S_Put (0, "package body " & Main_Suite_Name & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put
        (3,
         "function Suite return AUnit.Test_Suites." & "Access_Test_Suite is");
      Put_New_Line;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      Iterator := List_Of_Strings.First (Suit_List);
      loop
         exit when Iterator = List_Of_Strings.No_Element;

         S_Put
           (6,
            "Add_Test (Result'Access, " &
            List_Of_Strings.Element (Iterator) &
            ".Suite);");
         Put_New_Line;

         List_Of_Strings.Next (Iterator);
      end loop;

      Put_New_Line;
      S_Put (6, "return Result'Access;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & Main_Suite_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  creating test runner body
      Create
        (Harness_Dir.all & Unit_To_File_Name (Test_Runner_Name) & ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Reporter." & Reporter_Name.all & ";");
      Put_New_Line;
      S_Put (0, "with AUnit.Run;");
      Put_New_Line;
      S_Put (0, "with AUnit.Options; use AUnit.Options;");
      Put_New_Line;
      S_Put (0, "with " & Main_Suite_Name & "; use " & Main_Suite_Name & ";");
      Put_New_Line;
      Put_New_Line;
      if not No_Command_Line then
         S_Put (0, "with AUnit; use AUnit;");
         Put_New_Line;
         S_Put (0, "with Ada.Command_Line;");
         Put_New_Line;
      end if;
      if not No_Command_Line then
         S_Put (0, "with GNAT.Command_Line; use GNAT.Command_Line;");
         Put_New_Line;
         Put_New_Line;
         if not Harness_Only then
            S_Put (0, "with Gnattest_Generated;");
            Put_New_Line;
            Put_New_Line;
         end if;
      end if;
      S_Put (0, "procedure " & Test_Runner_Name & " is");
      Put_New_Line;
      if No_Command_Line then
         S_Put (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
      else
         S_Put
           (3,
            "function Runner is new "
            & "AUnit.Run.Test_Runner_With_Status (Suite);");
         Put_New_Line;
         S_Put (3, "Exit_Status : AUnit.Status;");
         Put_New_Line;
         if Add_Exit_Status then
            S_Put (3, "Use_Exit_Status : Boolean := True;");
         else
            S_Put (3, "Use_Exit_Status : Boolean := False;");
         end if;
      end if;
      Put_New_Line;
      S_Put
        (3,
         "Reporter : AUnit.Reporter."
         & Reporter_Name.all
         & "."
         & Reporter_Name.all
         & "_Reporter;");
      Put_New_Line;
      S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
      Put_New_Line;
      S_Put (0, "begin");
      Put_New_Line;
      Put_New_Line;
      if Show_Passed_Tests then
         S_Put (3, "GT_Options.Report_Successes := True;");
      else
         S_Put (3, "GT_Options.Report_Successes := False;");
      end if;
      Put_New_Line;
      if Show_Test_Duration then
         S_Put (3, "GT_Options.Test_Case_Timer := True;");
      end if;
      Put_New_Line;
      if not No_Command_Line then
         S_Put (3, "begin");
         Put_New_Line;
         S_Put (6, "Initialize_Option_Scan;");
         Put_New_Line;
         S_Put (6, "loop");
         Put_New_Line;
         S_Put (9, "case GNAT.Command_Line.Getopt");
         Put_New_Line;
         if Harness_Only then
            --  No point in --skeleton-default in --harness-only mode.
            S_Put
              (11, "(""-passed-tests= -exit-status="")");
         else
            S_Put
              (11, "(""-skeleton-default= -passed-tests= -exit-status="")");
         end if;
         Put_New_Line;
         S_Put (9, "is");
         Put_New_Line;
         S_Put (12, "when ASCII.NUL =>");
         Put_New_Line;
         S_Put (15, "exit;");
         Put_New_Line;
         S_Put (12, "when '-' =>");
         Put_New_Line;
         --  --skeleton-default
         if not Harness_Only then
            S_Put (15, "if Full_Switch = ""-skeleton-default"" then");
            Put_New_Line;
            S_Put (18, "if Parameter = ""pass"" then");
            Put_New_Line;
            S_Put (21, "Gnattest_Generated.Default_Assert_Value := True;");
            Put_New_Line;
            S_Put (18, "elsif Parameter = ""fail"" then");
            Put_New_Line;
            S_Put (21, "Gnattest_Generated.Default_Assert_Value := False;");
            Put_New_Line;
            S_Put (18, "end if;");
            Put_New_Line;
            S_Put (15, "end if;");
            Put_New_Line;
         end if;
         --  --passed-tests
         S_Put (15, "if Full_Switch = ""-passed-tests"" then");
         Put_New_Line;
         S_Put (18, "if Parameter = ""show"" then");
         Put_New_Line;
         S_Put (21, "GT_Options.Report_Successes := True;");
         Put_New_Line;
         S_Put (18, "elsif Parameter = ""hide"" then");
         Put_New_Line;
         S_Put (21, "GT_Options.Report_Successes := False;");
         Put_New_Line;
         S_Put (18, "end if;");
         Put_New_Line;
         S_Put (15, "end if;");
         Put_New_Line;
         --  --exit-status
         S_Put (15, "if Full_Switch = ""-exit-status"" then");
         Put_New_Line;
         S_Put (18, "if Parameter = ""on"" then");
         Put_New_Line;
         S_Put (21, "Use_Exit_Status := True;");
         Put_New_Line;
         S_Put (18, "elsif Parameter = ""off"" then");
         Put_New_Line;
         S_Put (21, "Use_Exit_Status := False;");
         Put_New_Line;
         S_Put (18, "end if;");
         Put_New_Line;
         S_Put (15, "end if;");
         Put_New_Line;
         S_Put (12, "when others => null;");
         Put_New_Line;
         S_Put (9, "end case;");
         Put_New_Line;
         S_Put (6, "end loop;");
         Put_New_Line;
         S_Put (3, "exception");
         Put_New_Line;
         S_Put (6, "when GNAT.Command_Line.Invalid_Switch => null;");
         Put_New_Line;
         S_Put (3, "end;");
         Put_New_Line;
         Put_New_Line;
      end if;
      if No_Command_Line then
         S_Put (3, "Runner (Reporter, GT_Options);");
      else
         S_Put (3, "Exit_Status := Runner (Reporter, GT_Options);");
         Put_New_Line;
         S_Put
           (3,
            "if Use_Exit_Status and then Exit_Status = AUnit.Failure then");
         Put_New_Line;
         S_Put
           (6,
            "Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);");
         Put_New_Line;
         S_Put (3, "end if;");
      end if;
      Put_New_Line;
      S_Put (0, "end " & Test_Runner_Name & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

   end Test_Runner_Generator;

   -----------------------
   --  Project_Creator  --
   -----------------------

   procedure Project_Creator (Source_Prj : String) is
   begin
      Generate_Gnattest_Common_Prj;

      Create (Harness_Dir.all & "test_driver.gpr");

      if Tmp_Test_Prj /= null then
         S_Put (0, "with """                 &
                Base_Name (Tmp_Test_Prj.all) &
                """;");
         Put_New_Line;
      end if;

      if Harness_Only then
         S_Put (0, "with """     &
                  Source_Prj &
                  """;");
         Put_New_Line;
      end if;

      if Additional_Tests_Prj /= null then
         S_Put (0, "with """     &
                  Additional_Tests_Prj.all &
                  """;");
         Put_New_Line;
      end if;

      S_Put (0, "with ""gnattest_common.gpr"";");
      Put_New_Line;

      Put_New_Line;
      S_Put (0, "project Test_Driver is");
      Put_New_Line;
      Put_New_Line;
      S_Put
           (3,
            "for Origin_Project use """
            & (+Relative_Path
                 (Create (+Source_Prj),
                   Create (+Normalize_Pathname (Harness_Dir.all))))
            & """;");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "for Target use Gnattest_Common'Target;");
      Put_New_Line;
      Put_New_Line;
      S_Put
        (3,
         "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "for Languages use (""Ada"");");
      Put_New_Line;
      S_Put (3, "for Main use (""test_runner.adb"");");
      Put_New_Line;

      if Harness_Only and then not Gnattest_Generated_Present then
         S_Put (3, "for Source_Dirs use (""."", ""common"");");
         Put_New_Line;
      end if;

      S_Put (3, "for Exec_Dir use ""."";");
      Put_New_Line;
      Put_New_Line;

      S_Put (3, "package Builder renames Gnattest_Common.Builder;");
      Put_New_Line;
      S_Put (3, "package Linker renames Gnattest_Common.Linker;");
      Put_New_Line;
      S_Put (3, "package Binder renames Gnattest_Common.Binder;");
      Put_New_Line;
      S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
      Put_New_Line;
      Put_New_Line;

      if IDE_Package_Present then
         S_Put
           (3,
            "package Ide renames " &
            Test_Prj_Prefix &
            Base_Name (Source_Prj, File_Extension (Source_Prj)) &
            ".Ide;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if Make_Package_Present then
         S_Put
           (3,
            "package Make renames " &
            Test_Prj_Prefix &
            Base_Name (Source_Prj, File_Extension (Source_Prj)) &
            ".Make;");
         Put_New_Line;
         Put_New_Line;
      end if;

      if not Harness_Only then
         S_Put (3, "package GNATtest is");
         Put_New_Line;
         S_Put (6, "for GNATTest_Mapping_File use ""gnattest.xml"";");
         Put_New_Line;
         S_Put (3, "end GNATtest;");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (0, "end Test_Driver;");

      Close_File;

      Generate_Global_Config_Pragmas_File;
   end Project_Creator;

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Data : Data_Holder; Path : String := "") is
      New_Unit_Name : String_Access;

      Current_Type : Test_Type_Info;

      File_Destination : constant String :=
        (if Path = "" then Harness_Dir.all else Path);
   begin

      if Data.Generic_Kind then

         New_Unit_Name := new String'(Data.Test_Unit_Full_Name.all &
                                      "."                          &
                                      Generic_Suite_Name);
      else

         New_Unit_Name := new String'(Data.Test_Unit_Full_Name.all &
                                      "."                          &
                                      Common_Suite_Name);
      end if;

      --  Creating test suite spec
      Create (File_Destination
              & Unit_To_File_Name (New_Unit_Name.all)
              & ".ads");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      S_Put (0, "with AUnit.Test_Suites;");
      if Data.Generic_Kind then
         S_Put (1, "use AUnit.Test_Suites;");
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Caller;");
      end if;
      Put_New_Line;
      Put_New_Line;

      if Data.Generic_Kind then
         S_Put (0, "generic");
         Put_New_Line;
         S_Put (3, "Instance_Name : String;");
         Put_New_Line;
      end if;

      S_Put (0, "package " & New_Unit_Name.all & " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
      Put_New_Line;
      Put_New_Line;

      if Data.Generic_Kind then

         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            S_Put (3, "package Runner_" & Positive_Image (I));
            S_Put (0, " is new AUnit.Test_Caller");
            Put_New_Line;

            S_Put (5,
                   "("                           &
                   Data.Test_Unit_Full_Name.all  &
                   "."                           &
                   Data.Test_Types.Element (I).Test_Type_Name.all &
                   ");");
            Put_New_Line;
            Put_New_Line;
         end loop;

         for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

            S_Put (3,
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   "_" &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_Access : constant Runner_" &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Method :=");
            Put_New_Line;
            S_Put (5,
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   "'Access;");
            Put_New_Line;

         end loop;

         for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

            S_Put
              (3,
               Data.ITR_List.Element (K).TR_Text_Name.all &
               "_" &
               Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
               "_Access : constant Runner_" &
               Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
               ".Test_Method :=");
            Put_New_Line;
            S_Put (5,
                   Data.ITR_List.Element (K).TR_Text_Name.all &
                   "'Access;");
            Put_New_Line;

         end loop;

         Put_New_Line;

      end if;

      S_Put (0, "end " & New_Unit_Name.all & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      --  Creating test suite body
      Create (File_Destination                      &
              Unit_To_File_Name (New_Unit_Name.all) &
              ".adb");

      Put_Harness_Header;
      S_Put (0, GT_Marker_Begin);
      Put_New_Line;

      if not Data.Generic_Kind then
         S_Put (0, "with AUnit.Test_Caller;");
      end if;
      Put_New_Line;
      S_Put (0, "with Gnattest_Generated;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0,
             "package body "     &
             New_Unit_Name.all   &
             " is");
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "use AUnit.Test_Suites;");
      Put_New_Line;
      Put_New_Line;

      if not Data.Generic_Kind then

         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            Current_Type := Data.Test_Types.Element (I);

            S_Put (3, "package Runner_" & Positive_Image (I));
            S_Put (0, " is new AUnit.Test_Caller");
            Put_New_Line;

            if
              Nesting_Difference
                (Current_Type.Nesting.all,
                 Data.Test_Unit_Full_Name.all) = ""
            then
               S_Put (5,
                      "(GNATtest_Generated.GNATtest_Standard." &
                      Data.Test_Unit_Full_Name.all    &
                      "."                             &
                      Current_Type.Test_Type_Name.all &
                      ");");
            else
               S_Put
                 (5,
                  "(GNATtest_Generated.GNATtest_Standard."     &
                  Data.Test_Unit_Full_Name.all     &
                  "."                              &
                  Nesting_Difference
                    (Current_Type.Nesting.all,
                     Data.Test_Unit_Full_Name.all) &
                  "."                              &
                  Current_Type.Test_Type_Name.all  &
                  ");");
            end if;

            Put_New_Line;
            Put_New_Line;
         end loop;

         S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");

         Put_New_Line;
         Put_New_Line;

      end if;

      --  Declaring test cases for test routines

      --  Test case variables recieve unique numbers in order to
      --  escape name collisions for cases when test routines with
      --  same name and same test type are declared in different
      --  nested packages.

      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (3,
                   Test_Case_Prefix                                         &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " : Runner_"                                             &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case_Access;");
         else

            S_Put (3,
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " : aliased Runner_"                                     &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case;");
         end if;

         Put_New_Line;
      end loop;

      --  Declaring test cases for inherited test routines
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (3,
                   Test_Case_Prefix                                          &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "i_"                                                      &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " : Runner_"                                              &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case_Access;");
         else

            S_Put (3,
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " : aliased Runner_"                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Test_Case;");
         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;
      S_Put (3,
             "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
      Put_New_Line;
      if Data.Generic_Kind then
         S_Put (6, "Result : constant Access_Test_Suite := new Test_Suite;");
         Put_New_Line;
      end if;
      S_Put (3, "begin");
      Put_New_Line;
      Put_New_Line;

      --  Creating test cases for test routines
      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then

            S_Put (6,
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   " :=");
            Put_New_Line;
            S_Put (8,
                   "Runner_"                                                &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (10,
                   " (Instance_Name &");
            Put_New_Line;
            S_Put (11,
                   " "" : "                                  &
                   Data.TR_List.Element (K).TR_Text_Name.all &
                   """,");
            Put_New_Line;
            S_Put (11,
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_Access);");

         else

            S_Put (6,
                   "Runner_"                                                &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (8,
                   "("                                                      &
                   Test_Case_Prefix                                         &
                   Positive_Image (K)                                       &
                   "_"                                                      &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb) &
                   "_"                                                      &
                   Data.TR_List.Element (K).TR_Text_Name.all                &
                   ",");
            Put_New_Line;
            S_Put (9,
                   """"
                   & Data.TR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            if
              Nesting_Difference
                (Data.TR_List.Element (K).Nesting.all,
                 Data.Test_Unit_Full_Name.all) /= ""
            then
               S_Put
                 (9,
                  Nesting_Difference
                    (Data.TR_List.Element (K).Nesting.all,
                     Data.Test_Unit_Full_Name.all)          &
                  "."                                       &
                  Data.TR_List.Element (K).TR_Text_Name.all &
                  "'Access);");
            else
               S_Put (9,
                      Data.TR_List.Element (K).TR_Text_Name.all &
                      "'Access);");
            end if;

         end if;

         Put_New_Line;

      end loop;

      --  Creating test cases for inherited test routines
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         Current_Type := Data.Test_Types.Element
           (Data.ITR_List.Element (K).Test_Type_Numb);

         if Data.Generic_Kind then

            S_Put (6,
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   " :=");
            Put_New_Line;
            S_Put (8,
                   "Runner_"                                                 &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (10, "(Instance_Name &");
            Put_New_Line;
            S_Put (11, """ (inherited from " &
                   Data.ITR_List.Element (K).TR_Rarent_Unit_Name.all    &
                   ") : "                                               &
                   Data.ITR_List.Element (K).TR_Text_Name.all           &
                   """,");
            Put_New_Line;
            S_Put (11,
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   "_"                                                       &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_Access);");
         else

            S_Put (6,
                   "Runner_"                                                 &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   ".Create");
            Put_New_Line;
            S_Put (8,
                   "("                                                       &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   ",");
            Put_New_Line;

            S_Put (9,
                   """"
                   & Data.ITR_List.Element (K).Tested_Sloc.all
                   & """,");
            Put_New_Line;
            if
              Nesting_Difference
                (Current_Type.Nesting.all, Data.Test_Unit_Full_Name.all) = ""
            then
               S_Put (9,
                      Data.ITR_List.Element (K).TR_Text_Name.all &
                      "'Access);");
            else
               S_Put
                 (9,
                  Nesting_Difference
                    (Current_Type.Nesting.all,
                     Data.Test_Unit_Full_Name.all)            &
                  "."                                         &
                   Data.ITR_List.Element (K).TR_Text_Name.all &
                  "'Access);");
            end if;

         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;

      --  Adding test cases to the suite
      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop

         if Data.Generic_Kind then
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "_"                                                       &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb)  &
                   "_"                                                       &
                   Data.TR_List.Element (K).TR_Text_Name.all                 &
                   ");");
         else
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "_"                                                       &
                   Positive_Image (Data.TR_List.Element (K).Test_Type_Numb)  &
                   "_"                                                       &
                   Data.TR_List.Element (K).TR_Text_Name.all                 &
                   "'Access);");
         end if;

         Put_New_Line;

      end loop;

      --  Adding inherited test cases to the suite
      for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop

         if Data.Generic_Kind then
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   ");");
         else
            S_Put (6,
                   "Add_Test (Result'Access, "                               &
                   Test_Case_Prefix                                          &
                   Positive_Image (K)                                        &
                   "i_"                                                      &
                   Positive_Image (Data.ITR_List.Element (K).Test_Type_Numb) &
                   "_"                                                       &
                   Data.ITR_List.Element (K).TR_Text_Name.all                &
                   "'Access);");
         end if;

         Put_New_Line;

      end loop;

      Put_New_Line;

      for K in Data.TC_List.First_Index .. Data.TC_List.Last_Index loop
         S_Put
           (6,
            "Add_Test (Result'Access, new " &
            Data.TC_List.Element (K).Nesting.all &
            "." &
            Data.TC_List.Element (K).Name.all &
            ");");
         Put_New_Line;
      end loop;

      if Data.Generic_Kind then
         S_Put (6, "return Result;");
      else
         S_Put (6, "return Result'Access;");
      end if;
      Put_New_Line;
      Put_New_Line;
      S_Put (3, "end Suite;");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "end " & New_Unit_Name.all & ";");
      Put_New_Line;
      S_Put (0, GT_Marker_End);
      Put_New_Line;
      Close_File;

      if not Data.Generic_Kind then
         List_Of_Strings.Append (Suit_List, New_Unit_Name.all);
      end if;

   end Generate_Suite;

   ---------------------------
   -- Generate_Test_Drivers --
   ---------------------------

   procedure Generate_Test_Drivers
     (Data      : Data_Holder;
      UUT       : String;
      Stub_List : Ada_Nodes_List.List)
   is

      procedure Process_Test_Routine (Current_TR : Test_Routine_Info'Class);
      procedure Process_Test_Package;

      function Recover_Test_Data_Unit_Name (S : String) return String;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info;

      Local_SPI         : Separate_Project_Info;
      Local_Data_Holder : Data_Holder := Data;

      function Get_SPI
        (Current_TR    : Test_Routine_Info'Class;
         New_Unit_Dir  : String_Access;
         New_Unit_Name : String_Access)
         return Separate_Project_Info
      is
         SPI : Separate_Project_Info;
      begin
         Trace (Me, "getting separate project info...");
         SPI.Name_TD         := new String'
           (TD_Prefix
            & Current_TR.TR_Text_Name.all);
         SPI.Path_TD         := new String'
           (New_Unit_Dir.all
            & Unit_To_File_Name
              (TD_Prefix
               & Current_TR.TR_Text_Name.all)
            & ".gpr");
         SPI.Main_File_Name   := new String'
           (Unit_To_File_Name (New_Unit_Name.all)
            & ".adb");

         if not Stub_Mode_ON then
            Trace (Me, "done");
            return SPI;
         end if;

         SPI.UUT_File_Name := new String'
           (Test.Skeleton.Source_Table.Get_Current_Source_Spec);

         declare
            Corresponding_Body : constant String :=
              Test.Skeleton.Source_Table.Get_Source_Body (UUT);
            Project_Name : constant String :=
              (if Corresponding_Body = "" then
                  Test.Skeleton.Source_Table.Get_Source_Project_Name (UUT)
               else
                  Test.Skeleton.Source_Table.Get_Source_Project_Name
                    (Corresponding_Body));
            Project_Path : constant String :=
              Test.Skeleton.Source_Table.Get_Project_Path (Project_Name);

            Cur : Ada_Nodes_List.Cursor;
            use Ada_Nodes_List;
         begin
            SPI.Name_Extending := new String'
              (Current_TR.TR_Text_Name.all);
            SPI.Path_Extending := new String'
              (New_Unit_Dir.all
               & Unit_To_File_Name
                 (Current_TR.TR_Text_Name.all)
               & ".gpr");

            SPI.Name_Of_Extended := new String'
              (Project_Name);
            SPI.Path_Of_Extended := new String'
              (+Relative_Path
                 (Create (+Project_Path),
                  Create (+New_Unit_Dir.all)));

            SPI.Test_Package := new String'
              (Data.Test_Unit_Full_Name.all);
            SPI.Test_Data    := new String'
              (Recover_Test_Data_Unit_Name
                 (Data.Test_Unit_Full_Name.all));

            Cur := Stub_List.First;
            while Cur /= Ada_Nodes_List.No_Element loop
               declare
                  S : constant String :=
                    Ada_Nodes_List.Element (Cur).Unit.Get_Filename;
               begin
                  --  If any source meant to be stubbed is from same
                  --  project and is actually stubbed, then stub dir should
                  --  be set.
                  if
                    Project_Path =
                      Test.Skeleton.Source_Table.Get_Project_Path
                        (Test.Skeleton.Source_Table.
                           Get_Source_Project_Name (S)) and then
                        Test.Skeleton.Source_Table.Source_Stubbed (S)
                  then

                     if S /= UUT then
                        declare
                           App : constant String :=
                             Test.Skeleton.Source_Table.Get_Source_Body (S);
                        begin
                           if App /= "" then
                              SPI.Sources_List.Append (App);
                              declare
                                 SD_Spec : constant String :=
                                   Test.Skeleton.Source_Table.
                                     Get_Source_Stub_Data_Spec (S);
                                 SD_Body : constant String :=
                                   Test.Skeleton.Source_Table.
                                     Get_Source_Stub_Data_Body (S);
                              begin
                                 if not
                                   Excluded_Test_Data_Files.Contains (SD_Spec)
                                 then
                                    SPI.Sources_List.Append (SD_Spec);
                                 end if;
                                 if not
                                   Excluded_Test_Data_Files.Contains (SD_Body)
                                 then
                                    SPI.Sources_List.Append (SD_Body);
                                 end if;
                              end;
                           end if;
                        end;
                     end if;

                     if SPI.Stub_Source_Dir = null then
                        SPI.Stub_Source_Dir := new String'
                          (Test.Skeleton.Source_Table.
                             Get_Project_Stub_Dir (Project_Name));
                     end if;
                  end if;
               end;

               Next (Cur);
            end loop;
         end;

         Trace (Me, "done");
         return SPI;
      end Get_SPI;

      function Recover_Test_Data_Unit_Name (S : String) return String is
      begin
         for I in reverse S'Range loop
            if S (I) = '.' then
               return S (S'First .. I - 1);
            end if;
         end loop;

         return S;
      end Recover_Test_Data_Unit_Name;

      procedure Process_Test_Package is
         New_Unit_Dir : constant String :=
           Harness_Dir.all
           & Data.Test_Unit_Full_Name.all
           & Directory_Separator;

         New_Unit_Name : constant String :=
           (Data.Test_Unit_Full_Name.all
            & ".Suite.Test_Runner");
      begin
         Trace (Me, "processing package " & Data.Test_Unit_Full_Name.all);

         declare
            Dir : File_Array_Access;
         begin
            Append (Dir, GNATCOLL.VFS.Create (+New_Unit_Dir));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Cmd_Error_No_Help
                 ("gnattest: cannot create directory " & New_Unit_Dir);
         end;

         --  Creating test driver procedure
         Create (New_Unit_Dir
                 & Unit_To_File_Name (New_Unit_Name)
                 & ".adb");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         S_Put (0, "with Gnattest_Generated;");
         Put_New_Line;
         S_Put (0, "with Gnattest_Generated.Persistent;");
         Put_New_Line;
         S_Put (0, "with AUnit.Reporter.GNATtest;");
         Put_New_Line;
         S_Put (0, "with AUnit.Run;");
         Put_New_Line;
         S_Put (0, "with AUnit.Options; use AUnit.Options;");
         Put_New_Line;
         if Add_Exit_Status and then not No_Command_Line then
            S_Put (0, "with AUnit; use AUnit;");
            Put_New_Line;
            S_Put (0, "with Ada.Command_Line;");
            Put_New_Line;
         end if;
         Put_New_Line;
         if Data.Good_For_Substitution then
            S_Put
              (0,
               "with "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & "; use "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & ";");
            Put_New_Line;
         end if;

         S_Put (0, "procedure " & New_Unit_Name & " is");
         Put_New_Line;
         Put_New_Line;

         if Data.Good_For_Substitution then
            S_Put
              (3,
               "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
            Put_New_Line;
            Put_New_Line;
            S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (3,
               "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
            Put_New_Line;
            S_Put (3, "begin");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (6,
               "Add_Test (Result'Access, "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Common_Suite_Name
               & ".Suite);");
            Put_New_Line;
            S_Put
              (6,
               "Add_Test (Result'Access, "
               & Data.Test_Unit_Full_Name.all
               & "."
               & Substitution_Suite_Name
               & ".Suite);");
            Put_New_Line;
            Put_New_Line;
            S_Put (6, "return Result'Unchecked_Access;");
            Put_New_Line;
            Put_New_Line;
            S_Put (3, "end Suite;");
            Put_New_Line;
            Put_New_Line;

         end if;

         if Add_Exit_Status and then not No_Command_Line then
            S_Put
              (3,
               "function Runner is new "
               & "AUnit.Run.Test_Runner_With_Status (Suite);");
         else
            S_Put
              (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
         end if;
         Put_New_Line;

         S_Put (3, "Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;");
         Put_New_Line;
         S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
         Put_New_Line;
         if Add_Exit_Status and then not No_Command_Line then
            Put_New_Line;
            S_Put (3, "Exit_Status : AUnit.Status;");
         end if;
         Put_New_Line;

         S_Put (0, "begin");
         Put_New_Line;
         if Show_Passed_Tests then
            S_Put (3, "GT_Options.Report_Successes := True;");
         else
            S_Put (3, "GT_Options.Report_Successes := False;");
         end if;
         Put_New_Line;
         if Show_Test_Duration then
            S_Put (3,
                   "GT_Options.Test_Case_Timer := True;");
            Put_New_Line;
         end if;
         Put_New_Line;
         S_Put (3, "Gnattest_Generated.Persistent.Global_Set_Up;");
         Put_New_Line;

         if Add_Exit_Status and then not No_Command_Line then
            S_Put (3, "Exit_Status := Runner (Reporter, GT_Options);");
            Put_New_Line;
            S_Put (3, "Gnattest_Generated.Persistent.Global_Tear_Down;");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (3,
               "if Exit_Status = AUnit.Failure then");
            Put_New_Line;
            S_Put
              (6,
               "Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);");
            Put_New_Line;
            S_Put
              (3,
               "end if;");
            Put_New_Line;
         else

            S_Put (3, "Runner (Reporter, GT_Options);");
            Put_New_Line;
            S_Put (3, "Gnattest_Generated.Persistent.Global_Tear_Down;");
            Put_New_Line;
         end if;

         S_Put (0, "end " & New_Unit_Name & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

      end Process_Test_Package;

      procedure Process_Test_Routine (Current_TR : Test_Routine_Info'Class)
      is
         Current_Type : constant Test_Type_Info :=
          Data.Test_Types.Element (Current_TR.Test_Type_Numb);

         New_Unit_Name : String_Access;
         New_Unit_Dir  : String_Access;
      begin
         Trace (Me, "processing routine " & Current_TR.TR_Text_Name.all);

         New_Unit_Name := new String'
           (Data.Test_Unit_Full_Name.all
            & "."
            & TD_Prefix
            & Current_TR.TR_Text_Name.all);

         --  Distinguish different set of test drivers by putting them in dirs
         --  with names corresponding to UUTs.
         New_Unit_Dir := new String'
           (Harness_Dir.all
            & Data.Test_Unit_Full_Name.all
            & Directory_Separator);

         declare
            Dir : File_Array_Access;
         begin
            Append (Dir, GNATCOLL.VFS.Create (+New_Unit_Dir.all));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Cmd_Error_No_Help
                 ("gnattest: cannot create directory " & New_Unit_Dir.all);
         end;

         --  Creating test driver procedure
         Create (New_Unit_Dir.all
                 & Unit_To_File_Name (New_Unit_Name.all)
                 & ".adb");

         Put_Harness_Header;
         S_Put (0, GT_Marker_Begin);
         Put_New_Line;

         S_Put (0, "pragma Ada_2005;");
         Put_New_Line;
         Put_New_Line;
         S_Put (0, "with AUnit.Test_Suites; use AUnit.Test_Suites;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put (0, "with AUnit.Test_Caller;");
            Put_New_Line;
         end if;
         S_Put (0, "with Gnattest_Generated;");
         Put_New_Line;
         S_Put (0, "with Gnattest_Generated.Persistent;");
         Put_New_Line;
         S_Put (0, "with AUnit.Reporter.GNATtest;");
         Put_New_Line;
         S_Put (0, "with AUnit.Run;");
         Put_New_Line;
         S_Put (0, "with AUnit.Options; use AUnit.Options;");
         Put_New_Line;
         if Add_Exit_Status and then not No_Command_Line then
            S_Put (0, "with AUnit; use AUnit;");
            Put_New_Line;
            S_Put (0, "with Ada.Command_Line;");
            Put_New_Line;
         end if;
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "procedure " & New_Unit_Name.all & " is");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "Result : aliased AUnit.Test_Suites.Test_Suite;");
         Put_New_Line;
         if not Stub_Mode_ON then
            S_Put
              (3,
               "package Caller is new AUnit.Test_Caller");
            Put_New_Line;
            S_Put
              (5,
               "(GNATtest_Generated.GNATtest_Standard."
               & Data.Test_Unit_Full_Name.all
               & "."
               & Current_Type.Test_Type_Name.all
               & ");");
            Put_New_Line;
         end if;
         S_Put (3, "Local_Test_Case : aliased Caller.Test_Case;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3, "function Suite return AUnit.Test_Suites.Access_Test_Suite is");
         Put_New_Line;
         S_Put (3, "begin");
         Put_New_Line;
         S_Put (6, "Caller.Create");
         Put_New_Line;
         S_Put (8, "(Local_Test_Case,");
         Put_New_Line;
         S_Put (9,
                """"
                & Current_TR.Tested_Sloc.all
                & """,");
         Put_New_Line;
         S_Put (9,
                Current_TR.TR_Text_Name.all
                & "'Access);");
         Put_New_Line;
         Put_New_Line;
         S_Put (6, "Add_Test (Result'Access, Local_Test_Case'Access);");
         Put_New_Line;
         Put_New_Line;
         S_Put (6, "return Result'Unchecked_Access;");
         Put_New_Line;
         S_Put (3, "end Suite;");
         Put_New_Line;
         Put_New_Line;

         if Add_Exit_Status and then not No_Command_Line then
            S_Put
              (3,
               "function Runner is new "
               & "AUnit.Run.Test_Runner_With_Status (Suite);");
         else
            S_Put
              (3, "procedure Runner is new AUnit.Run.Test_Runner (Suite);");
         end if;
         Put_New_Line;

         S_Put (3, "Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;");
         Put_New_Line;
         S_Put (3, "GT_Options : AUnit_Options := Default_Options;");
         Put_New_Line;
         if Add_Exit_Status and then not No_Command_Line then
            Put_New_Line;
            S_Put (3, "Exit_Status : AUnit.Status;");
         end if;
         Put_New_Line;

         S_Put (0, "begin");
         Put_New_Line;
         if Show_Passed_Tests then
            S_Put (3, "GT_Options.Report_Successes := True;");
         else
            S_Put (3, "GT_Options.Report_Successes := False;");
         end if;
         Put_New_Line;
         if Show_Test_Duration then
            S_Put (3,
                   "GT_Options.Test_Case_Timer := True;");
            Put_New_Line;
         end if;
         Put_New_Line;
         S_Put (3, "Gnattest_Generated.Persistent.Global_Set_Up;");
         Put_New_Line;
         if Add_Exit_Status and then not No_Command_Line then
            S_Put (3, "Exit_Status := Runner (Reporter, GT_Options);");
            Put_New_Line;
            S_Put (3, "Gnattest_Generated.Persistent.Global_Tear_Down;");
            Put_New_Line;
            Put_New_Line;
            S_Put
              (3,
               "if Exit_Status = AUnit.Failure then");
            Put_New_Line;
            S_Put
              (6,
               "Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);");
            Put_New_Line;
            S_Put
              (3,
               "end if;");
            Put_New_Line;
         else

            S_Put (3, "Runner (Reporter, GT_Options);");
            Put_New_Line;
            S_Put (3, "Gnattest_Generated.Persistent.Global_Tear_Down;");
            Put_New_Line;
         end if;
         Put_New_Line;

         S_Put (0, "end " & New_Unit_Name.all & ";");
         Put_New_Line;
         S_Put (0, GT_Marker_End);
         Put_New_Line;
         Close_File;

         Separate_Projects.Append
           (Get_SPI
              (Current_TR,
               New_Unit_Dir,
               New_Unit_Name));

      end Process_Test_Routine;
   begin
      Trace (Me, "Generate_Test_Drivers");
      Increase_Indent (Me);

      if Driver_Per_Unit then
         Process_Test_Package;
         if Stub_Mode_ON then
            --  No test inheritance in stub mode. We need to clear the list
            --  of inherited test routines, otherwise corresponding test cases
            --  will be declared in the suite.
            Local_Data_Holder.ITR_List.Clear;
         end if;
         Generate_Suite
           (Local_Data_Holder,
            Harness_Dir.all
            & Data.Test_Unit_Full_Name.all
            & Directory_Separator);

         declare
            S1 : constant String_Access := new String'
              (Harness_Dir.all
               & Data.Test_Unit_Full_Name.all
               & Directory_Separator);
            S2 : constant String_Access := new String'
              (Data.Test_Unit_Full_Name.all
               & ".Suite.Test_Runner");
         begin
            --  We may reuse the regular way of gathering data for separate
            --  drivers. Just need to override the names and paths for
            --  the generated projects.

            if Data.TR_List.Is_Empty then
               if Stub_Mode_ON then
                  --  No test inheritance in stub mode.
                  return;
               end if;
               Local_SPI := Get_SPI (Data.ITR_List.First_Element, S1, S2);
            else
               Local_SPI := Get_SPI (Data.TR_List.First_Element, S1, S2);
            end if;

            Free (Local_SPI.Name_TD);
            Local_SPI.Name_TD := new String'("Test_Driver");
            Free (Local_SPI.Name_Extending);
            Local_SPI.Name_Extending := new String'("Stubs");
            Free (Local_SPI.Path_TD);
            Local_SPI.Path_TD := new String'(S1.all & "test_driver.gpr");
            Free (Local_SPI.Path_Extending);
            Local_SPI.Path_Extending := new String'(S1.all & "stubs.gpr");
         end;

         Separate_Projects.Append (Local_SPI);
         Decrease_Indent (Me, "done");
         return;
      end if;

      for K in Data.TR_List.First_Index .. Data.TR_List.Last_Index loop
         Process_Test_Routine (Data.TR_List.Element (K));
      end loop;

      if Separate_Drivers and then not Stub_Mode_ON then
         for K in Data.ITR_List.First_Index .. Data.ITR_List.Last_Index loop
            Process_Test_Routine (Data.ITR_List.Element (K));
         end loop;
      end if;

      Decrease_Indent (Me, "done");
   end Generate_Test_Drivers;

   ----------------------------------------
   -- Generate_Stub_Test_Driver_Projects --
   ----------------------------------------

   procedure Generate_Stub_Test_Driver_Projects (Source_Prj : String) is
      P : Separate_Project_Info;

      Imported : List_Of_Strings.List;
      I_Cur    : List_Of_Strings.Cursor;
      S_Cur    : List_Of_Strings.Cursor;

      Tmp, Current_Infix : String_Access;

      package Srcs is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Srcs;

      Out_Dirs     : Srcs.Set;
      Out_Dirs_Cur : Srcs.Cursor;

      procedure Add_Nesting_Hierarchy_Dummies (S : String);
      --  For nested packages corresponding test packages are children to a
      --  dummy hierarchy replicating the original package nesting and
      --  descending from top-level test project. All those dummy packages
      --  should also be included as test driver source files.
      --  Analyzes test package name and acts accordingly.

      procedure Add_Nesting_Hierarchy_Dummies (S : String) is
         Idx, Idx2 : Integer;
      begin
         Idx := Index (S, Test_Data_Unit_Name);
         Idx2 := Index (S, Test_Data_Unit_Name, Idx + 1);

         if Idx2 = 0 then
            --  Not a nested test package.
            return;
         end if;

         Idx2 := Index (S, ".", Idx);
         if not
           Excluded_Test_Package_Bodies.Contains
             (Unit_To_File_Name
                (S (S'First .. Idx2 - 1)) & ".adb")
         then
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".adb"",");
            Put_New_Line;
         end if;
         S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
         Put_New_Line;

         Idx2 := Index (S, ".", Idx2 + 1);
         if not
           Excluded_Test_Package_Bodies.Contains
             (Unit_To_File_Name
                (S (S'First .. Idx2 - 1)) & ".adb")
         then
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".adb"",");
            Put_New_Line;
         end if;
         S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
         Put_New_Line;

         loop
            Idx2 := Index (S, ".", Idx2 + 1);
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (S (S'First .. Idx2 - 1))
               & ".ads"",");
            Put_New_Line;

            if
              Index (S, ".", Idx2 + 1) >
              Index (S, Test_Data_Unit_Name, Idx2 + 1)
            then
               --  Next package is the original test data one.
               exit;
            end if;
         end loop;
      end Add_Nesting_Hierarchy_Dummies;

   begin
      Trace (Me, "Generate_Stub_Test_Driver_Projects");
      Increase_Indent (Me);

      if Separate_Projects.Is_Empty then
         Report_Std
           ("gnattest: no test skeletons generated because "
            & "no subprogram to test");
         Report_Std
           ("found in project " & Source_Prj, 10);
         Cmd_Error_No_Help ("cannot create main suite and test runner");
      end if;

      Test.Skeleton.Source_Table.Mark_Projects_With_Stubbed_Sources;

      for
        K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
      loop
         P := Separate_Projects.Element (K);

         Current_Infix := new String'(Get_Next_Infix);
         if Stub_Exclusion_Lists.Contains (Base_Name (P.UUT_File_Name.all))
         then
            Test.Skeleton.Source_Table.Enforce_Custom_Project_Extention
              (P.UUT_File_Name.all, P.Path_Extending.all, Current_Infix.all);
         else
            Test.Skeleton.Source_Table.Enforce_Project_Extention
              (P.Name_Of_Extended.all,
               P.Path_Extending.all,
               Current_Infix.all);
         end if;

         --  Extending project
         Create (P.Path_Extending.all);

         Imported :=
           Test.Skeleton.Source_Table.Get_Imported_Projects
             (P.Name_Of_Extended.all);

         I_Cur := Imported.First;
         while I_Cur /= List_Of_Strings.No_Element loop
            if
              Test.Skeleton.Source_Table.Project_Extended
                (List_Of_Strings.Element (I_Cur))
            then
               Tmp := new String'(List_Of_Strings.Element (I_Cur));
               declare
                  Imported_Stubbed_Path : constant String :=
                    Test.Skeleton.Source_Table.Get_Project_Stub_Dir
                      (Tmp.all)
                    & Directory_Separator
                    & Unit_To_File_Name
                    (Stub_Project_Prefix & Current_Infix.all & Tmp.all)
                    & ".gpr";
                  Relative_P : constant String :=
                    +Relative_Path
                    (Create (+Imported_Stubbed_Path),
                     Create (+Dir_Name (P.Path_Extending.all)));
               begin
                  S_Put
                    (0,
                     "with """
                     & Relative_P
                     & """;");
                  Put_New_Line;
               end;
            end if;

            Free (Tmp);

            Next (I_Cur);
         end loop;

         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Gnattest_Common_Prj_Name),
                   Create (+Dir_Name (P.Path_Extending.all))))
            & """;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (0,
            "project "
            & P.Name_Extending.all
            & " extends """
            & P.Path_Of_Extended.all
            & """ is");
         Put_New_Line;

         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Ide renames Gnattest_Common.Ide;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Make renames Gnattest_Common.Make;");
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
         Put_New_Line;

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_Extending.all)
                  & Directory_Separator
                  & P.Name_Extending.all
                  & "_lib")));
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_Extending.all)
                  & Directory_Separator
                  & P.Name_Extending.all
                  & "_obj")));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Cmd_Error_No_Help
                 ("gnattest: cannot create obj/lib directory for "
                  & P.Path_Extending.all);
         end;

         S_Put
           (3,
            "for Library_Dir use """
            & P.Name_Extending.all
            & "_lib"";");
         Put_New_Line;
         S_Put
           (3,
            "for Object_Dir use """
            & P.Name_Extending.all
            & "_obj"";");
         Put_New_Line;
         Put_New_Line;
         if  P.Stub_Source_Dir = null then
            S_Put (3, "for Source_Dirs use ();");
         else

            S_Put
              (3,
               "for Source_Dirs use ("""
               & P.Stub_Source_Dir.all
               & """);");
         end if;
         Put_New_Line;
         Put_New_Line;

         S_Cur := P.Sources_List.First;
         if S_Cur /= List_Of_Strings.No_Element then
            S_Put (3, "for Source_Files use");
            Put_New_Line;

            while S_Cur /= List_Of_Strings.No_Element loop

               if S_Cur = P.Sources_List.First then
                  S_Put
                    (5,
                     "("""
                     & Base_Name (List_Of_Strings.Element (S_Cur))
                     & """");
               else
                  S_Put
                    (6,
                     """"
                     & Base_Name (List_Of_Strings.Element (S_Cur))
                     & """");
               end if;

               Next (S_Cur);

               if S_Cur = List_Of_Strings.No_Element then
                  S_Put (0, ");");
               else
                  S_Put (0, ",");
               end if;

               Put_New_Line;

            end loop;
         end if;

         S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");

         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "end "
            & P.Name_Extending.all
            & ";");
         Close_File;

         --  Test driver project
         Create (P.Path_TD.all);

         S_Put (0, "with ""aunit"";");
         Put_New_Line;
         S_Put
           (0,
            "with """
              & (+Relative_Path
                 (Create (+Gnattest_Common_Prj_Name),
                   Create (+Dir_Name (P.Path_TD.all))))
            & """;");
         Put_New_Line;
         S_Put
           (0,
            "with """
            & Base_Name (P.Path_Extending.all)
            & """;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "project "
            & P.Name_TD.all
            & " is");
         Put_New_Line;
         if Relocatable_Harness then
            S_Put
              (3,
               "for Origin_Project use external "
               & "(""ORIGIN_PROJECT_DIR"", """") & """
               & Base_Name (P.Path_Of_Extended.all)
               & """;");
         else
            S_Put
              (3,
               "for Origin_Project use """
               & (+Relative_Path
                 (Create (+P.Path_Of_Extended.all),
                      Create (+Normalize_Pathname (Dir_Name (P.Path_TD.all)))))
               & """;");
         end if;
         Put_New_Line;
         Put_New_Line;

         S_Put (3, "for Languages use Gnattest_Common'Languages & (""Ada"");");
         Put_New_Line;

         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Ide renames Gnattest_Common.Ide;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Make renames Gnattest_Common.Make;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (3,
            "for Main use ("""
            & P.Main_File_Name.all
            & """);");
         Put_New_Line;
         S_Put (3, "for Exec_Dir use ""."";");
         Put_New_Line;

         declare
            Dir : File_Array_Access;
         begin
            Append
              (Dir,
               GNATCOLL.VFS.Create
                 (+(Dir_Name (P.Path_TD.all)
                  & Directory_Separator
                  & P.Name_TD.all
                  & "_obj")));
            Create_Dirs (Dir);
         exception
            when Directory_Error =>
               Cmd_Error_No_Help
                 ("gnattest: cannot create obj directory for "
                  & P.Path_TD.all);
         end;

         S_Put
           (3,
            "for Object_Dir use """
            & P.Name_TD.all
            & "_obj"";");
         Put_New_Line;

         Test.Skeleton.Source_Table.Reset_Source_Iterator;
         loop
            Tmp := new String'
              (Test.Skeleton.Source_Table.Next_Source_Name);
            exit when Tmp.all = "";

            if
              Is_Directory
                (Test.Skeleton.Source_Table.Get_Source_Output_Dir
                   (Tmp.all))
            then
               Include
                 (Out_Dirs,
                  Test.Skeleton.Source_Table.Get_Source_Output_Dir
                    (Tmp.all));
            end if;
            Free (Tmp);
         end loop;

         S_Put (3, "for Source_Dirs use");
         Put_New_Line;

         if Out_Dirs.Is_Empty then
            S_Put (5, "(""../common"", ""."");");

            Put_New_Line;
            Put_New_Line;
         else
            Out_Dirs_Cur := Out_Dirs.First;
            S_Put (5, "(""");
            S_Put
              (0,
               +Relative_Path
                 (Create (+Srcs.Element (Out_Dirs_Cur)),
                  Create (+Dir_Name (P.Path_TD.all))) &
                 """");
            loop
               Srcs.Next (Out_Dirs_Cur);
               exit when Out_Dirs_Cur = Srcs.No_Element;

               S_Put (0, ",");
               Put_New_Line;
               S_Put (6, """");
               S_Put
                 (0,
                  +Relative_Path
                    (Create (+Srcs.Element (Out_Dirs_Cur)),
                     Create (+Dir_Name (P.Path_TD.all))) &
                    """");

            end loop;
            S_Put (0, ",");
            Put_New_Line;
            S_Put (6, """../common"", ""."");");

            Put_New_Line;
            Put_New_Line;
         end if;

         if Stub_Mode_ON then
            S_Put
              (3, "for Source_Files use");
            Put_New_Line;
            S_Put
              (5, "(""gnattest_generated.ads"",");
            Put_New_Line;
            S_Put
              (6, """gnattest_generated-persistent.ads"",");
            Put_New_Line;
            S_Put
              (6, """gnattest_generated-persistent.adb"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & P.Main_File_Name.all
               & """,");
            Put_New_Line;
            Add_Nesting_Hierarchy_Dummies (P.Test_Package.all);
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Package.all)
               & ".adb"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Package.all)
               & ".ads"",");
            Put_New_Line;
            if Driver_Per_Unit then
               S_Put
                 (6,
                  """"
                  & Unit_To_File_Name
                    (P.Test_Package.all & ".Suite")
                  & ".adb"",");
               Put_New_Line;
               S_Put
                 (6,
                  """"
                  & Unit_To_File_Name
                    (P.Test_Package.all & ".Suite")
                  & ".ads"",");
               Put_New_Line;
            end if;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Data.all)
               & ".adb"",");
            Put_New_Line;
            S_Put
              (6,
               """"
               & Unit_To_File_Name
                 (P.Test_Data.all)
               & ".ads"");");
            Put_New_Line;
            Put_New_Line;
         end if;

         S_Put (3, "package Builder renames Gnattest_Common.Builder;");
         Put_New_Line;
         S_Put (3, "package Linker renames Gnattest_Common.Linker;");
         Put_New_Line;
         S_Put (3, "package Binder renames Gnattest_Common.Binder;");
         Put_New_Line;
         S_Put (3, "package Compiler renames Gnattest_Common.Compiler;");
         Put_New_Line;
         Put_New_Line;

         S_Put
           (0,
            "end "
            & P.Name_TD.all
            & ";");
         Close_File;
         Free (Current_Infix);
      end loop;

      Generate_Common_Harness_Files (Source_Prj);

      Decrease_Indent (Me, "done");
   end Generate_Stub_Test_Driver_Projects;

   --------------------
   -- Get_Next_Infix --
   --------------------

   function Get_Next_Infix return String is
   begin
      Infix := Infix + 1;
      return Trim (Natural'Image (Infix), Both) & "_";
   end Get_Next_Infix;

   -----------------------------------
   -- Generate_Common_Harness_Files --
   -----------------------------------

   procedure Generate_Common_Harness_Files (Source_Prj : String) is
      P : Separate_Project_Info;

      procedure Generate_Aggregate_Project;
      --  Create aggregate project that incorparates all test driver projects.

      function Path_To_Unix (S : String) return String;
      --  Replace all "\" with "/".

      procedure Generate_Aggregate_Project is
      begin
         Create (Harness_Dir.all & "test_drivers.gpr");

         S_Put (0, "with ""gnattest_common.gpr"";");
         Put_New_Line;
         Put_New_Line;
         S_Put (0, "aggregate project Test_Drivers is");
         Put_New_Line;
         S_Put (3, "for Project_Files use");
         Put_New_Line;

         if Separate_Projects.Length = Ada.Containers.Count_Type (1) then
            P := Separate_Projects.First_Element;

            declare
               Pth : constant String :=
                 +Relative_Path
                 (Create (+P.Path_TD.all),
                  Create (+Harness_Dir.all));
            begin
               S_Put (5, "(""" & Pth & """);");
            end;
            Put_New_Line;
         else
            for
              K in Separate_Projects.First_Index ..
                Separate_Projects.Last_Index
            loop
               P := Separate_Projects.Element (K);
               declare
                  Pth : constant String :=
                    +Relative_Path
                    (Create (+P.Path_TD.all),
                     Create (+Harness_Dir.all));
               begin
                  if K = Separate_Projects.First_Index then
                     S_Put (5, "(""" & Pth & """,");
                  elsif K = Separate_Projects.Last_Index then
                     S_Put (6, """" & Pth & """);");
                  else
                     S_Put (6, """" & Pth & """,");
                  end if;
               end;
               Put_New_Line;
            end loop;
         end if;

         Put_New_Line;
         if Relocatable_Harness then
            S_Put
              (3,
               "for Origin_Project use external "
               & "(""ORIGIN_PROJECT_DIR"", """") & """
               & Base_Name (Source_Prj)
               & """;");
         else
            S_Put
              (3,
               "for Origin_Project use """
               & (+Relative_Path
                 (Create (+Source_Prj),
                      Create (+Normalize_Pathname (Harness_Dir.all))))
               & """;");
         end if;
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "for Target use Gnattest_Common'Target;");
         Put_New_Line;
         S_Put
           (3,
            "for Runtime (""Ada"") use Gnattest_Common'Runtime (""Ada"");");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package Builder renames Gnattest_Common.Builder;");
         Put_New_Line;
         Put_New_Line;
         S_Put (3, "package GNATtest is");
         Put_New_Line;
         S_Put (3, "for GNATTest_Mapping_File use ""gnattest.xml"";");
         Put_New_Line;
         S_Put (3, "end GNATtest;");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "end Test_Drivers;");

         Close_File;
      end Generate_Aggregate_Project;

      function Path_To_Unix (S : String) return String is
         S1 : String := (S);
      begin
         for I in S1'Range loop
            if S1 (I) = '\' then
               S1 (I) := '/';
            end if;
         end loop;

         return S1;
      end Path_To_Unix;

   begin
      Generate_Gnattest_Common_Prj;

      --  Makefile
      Create (Harness_Dir.all & "Makefile");
      S_Put (0, "# Check if we are running on Windows");
      Put_New_Line;
      S_Put (0, "ifeq ($(OS),Windows_NT)");
      Put_New_Line;
      S_Put (0,
             ASCII.HT
             & "EXE_EXT=.exe");
      Put_New_Line;
      S_Put (0, "else");
      Put_New_Line;
      S_Put (0,
             ASCII.HT
             & "EXE_EXT=");
      Put_New_Line;
      S_Put (0, "endif");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Executables");
      Put_New_Line;
      S_Put (0, "GPRBUILD=gprbuild");
      Put_New_Line;
      S_Put (0, "GPRCLEAN=gprclean");
      Put_New_Line;
      S_Put (0, "GNATCOV=gnatcov");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Number of processes used to build");
      Put_New_Line;
      S_Put (0, "# (default 0 is using maximum available cores)");
      Put_New_Line;
      S_Put (0, "NUMPROC=0");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Switches for gprbuild");
      Put_New_Line;
      S_Put
        (0,
         "# To be defined if there is a specific target and/or runtime, etc");
      Put_New_Line;
      S_Put (0, "BUILDERFLAGS=");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# Project-specific switches");
      Put_New_Line;
      S_Put (0, "# To be defined to customize the build");
      Put_New_Line;
      S_Put (0, "GPRFLAGS=");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, "# List of projects to build");
      Put_New_Line;
      S_Put (0, "PRJS = \");
      Put_New_Line;

      for
        K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
      loop
         P := Separate_Projects.Element (K);
         declare
            Rel_Pth : constant Filesystem_String :=
              Relative_Path
                (Create (+P.Path_TD.all),
                 Create (+Harness_Dir.all));
            Pth : constant String :=
              (if Driver_Per_Unit then +Dir_Name (Rel_Pth) else +Rel_Pth);
         begin
            S_Put
                (0,
                 ASCII.HT
                 & Path_To_Unix (Pth)
                 & (if K = Separate_Projects.Last_Index then "" else " \"));
         end;
         Put_New_Line;
      end loop;

      Put_New_Line;
      S_Put (0, "CKPTS = $(patsubst %,%-gnatcov-cov,$(PRJS))");
      Put_New_Line;
      S_Put (0, "GNATCOV_LEVEL=stmt+decision");
      Put_New_Line;
      S_Put (0, "GNATCOV_OUTPUT_FMT=dhtml --output-dir=dhtml-report");
      Put_New_Line;
      Put_New_Line;
      S_Put (0, ".PHONY: all");
      Put_New_Line;
      Put_New_Line;

      S_Put (0, "all: $(patsubst %,%-build,$(PRJS))");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-build: %/test_driver.gpr");
      else
         S_Put (0, "%-build: %");
      end if;
      Put_New_Line;
      S_Put
        (0,
         ASCII.HT
         & "$(GPRBUILD) $(BUILDERFLAGS) -P$< $(GPRFLAGS) -gargs -j$(NUMPROC)");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-build-cov: %/test_driver.gpr");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GPRBUILD) $(BUILDERFLAGS) -P$< $(GPRFLAGS) -gargs "
            & "-j$(NUMPROC) -cargs -g -fdump-scos -fpreserve-control-flow");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "%-gnatcov-run: %-build-cov");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GNATCOV) run --level=$(GNATCOV_LEVEL) -P$*/test_driver.gpr "
            & " $*/*-test_runner$(EXE_EXT) -o $*-gnattest.trace");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "%-gnatcov-cov: %-gnatcov-run");
         Put_New_Line;
         S_Put
           (0,
            ASCII.HT
            & "$(GNATCOV) coverage --save-checkpoint=$*-gnattest.ckpt "
            & "--level=$(GNATCOV_LEVEL) "
            & "-P$*/test_driver.gpr $*-gnattest.trace");
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "gnatcov-consolidate: $(CKPTS)");
         Put_New_Line;
         declare
            Pth : constant String :=
              +Relative_Path
              (Create (+Source_Prj),
               Create (+Harness_Dir.all));
         begin
            S_Put
              (0,
               ASCII.HT
               & "$(GNATCOV) coverage -P"
               & Path_To_Unix (Pth)
               & " $(patsubst %,-C "
               & "%-gnattest.ckpt,$(PRJS)) -a $(GNATCOV_OUTPUT_FMT) "
               & "--level=$(GNATCOV_LEVEL)");
         end;
         Put_New_Line;
         Put_New_Line;

         S_Put (0, "coverage: gnatcov-consolidate");
         Put_New_Line;
         Put_New_Line;
      end if;

      S_Put (0, "clean: $(patsubst %,%-clean,$(PRJS))");
      Put_New_Line;
      Put_New_Line;

      if Driver_Per_Unit then
         S_Put (0, "%-clean: %/test_driver.gpr");
      else
         S_Put (0, "%-clean: %");
      end if;
      Put_New_Line;
      S_Put
        (0,
         ASCII.HT
         & "$(GPRCLEAN) $(BUILDERFLAGS) -P$<");
      Put_New_Line;

      Close_File;

      --  suppress.adc & suppress_no_ghost.adc
      Generate_Global_Config_Pragmas_File;

      --  Executable list
      Create (Harness_Dir.all & "test_drivers.list");

      declare
         Exe_Suffix : String_Access := Get_Target_Executable_Suffix;
      begin
         for
           K in Separate_Projects.First_Index .. Separate_Projects.Last_Index
         loop
            P := Separate_Projects.Element (K);

            S_Put
              (0,
               Dir_Name (P.Path_TD.all)
               & Base_Name (P.Main_File_Name.all, ".adb")
               & Exe_Suffix.all);
            Put_New_Line;
         end loop;

         Free (Exe_Suffix);
      end;

      Close_File;

      Generate_Aggregate_Project;
   end Generate_Common_Harness_Files;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source (The_Unit : Analysis_Unit) is
      CU : constant Compilation_Unit := Root (The_Unit).As_Compilation_Unit;

      Appropriate_Source : Boolean;
      Data               : Data_Holder;
   begin
      if P_Unit_Kind (CU) = Unit_Body then
         --  Only interested in specs
         return;
      end if;
      Gather_Data (CU, Data, Appropriate_Source);

      if Appropriate_Source then
         if Data.Good_For_Suite then
            Generate_Suite (Data);
         end if;
      end if;
   end Process_Source;

   -----------------
   -- Gather_Data --
   -----------------

   procedure Gather_Data
     (The_Unit          :     Compilation_Unit;
      Data              : out Data_Holder;
      Appropriate_Source : out Boolean)
   is
      Bod : constant Library_Item := The_Unit.F_Body.As_Library_Item;

      Unit : Ada_Node;

      Number_Of_Test_Types : Natural  := 0;

      Local_TP_Mapping : User_Test_Package;

      function Get_Records (Node : Ada_Node'Class) return Visit_Status;
      --  Collects descendants of AUnit types Test_Case and Test_Fixture

      function Get_Subprograms (Node : Ada_Node'Class) return Visit_Status;
      --  Collects test routines and mapping info on Set_Up and Tear_Down

      function Is_Test_Case (Type_Decl : Base_Type_Decl) return Boolean;
      --  Indicates if given type is a test_case type of AUnit framework

      ------------------
      -- Is_Test_Case --
      ------------------

      function Is_Test_Case (Type_Decl : Base_Type_Decl) return Boolean is
         Dec_Elem : Base_Type_Decl := Type_Decl;
      begin
         while Dec_Elem /= No_Base_Type_Decl loop
            if
              Node_Image (Dec_Elem.As_Basic_Decl.P_Defining_Name) = "Test_Case"
              and then Is_AUnit_Part (Dec_Elem.Unit)
            then
               return True;
            end if;

            Dec_Elem := Parent_Type_Declaration (Dec_Elem);
         end loop;

         return False;
      end Is_Test_Case;

      -----------------
      -- Get_Records --
      -----------------

      function Get_Records (Node : Ada_Node'Class) return Visit_Status is
         Cur_Node : Base_Type_Decl;
         Type_Def : Derived_Type_Def;

         Type_Info : Test_Type_Info;
         Test_Case : Test_Case_Info;
      begin
         if Kind (Node) = Ada_Generic_Package_Decl then
            return Over;
         end if;

         if Kind (Node) /= Ada_Type_Decl then
            return Into;
         end if;

         if not Node.As_Type_Decl.P_Is_Tagged_Type then
            return Over;
         end if;

         if Node.As_Base_Type_Decl.P_Is_Private then
            Cur_Node :=
              Node.As_Base_Type_Decl.P_Private_Completion;
         else
            Cur_Node := Node.As_Base_Type_Decl;
         end if;

         if
           Cur_Node.As_Type_Decl.F_Type_Def.Kind /= Ada_Derived_Type_Def
           or else Abstract_Type (Cur_Node.As_Base_Type_Decl)
         then
            return Over;
         end if;

         Type_Def := Cur_Node.As_Type_Decl.F_Type_Def.As_Derived_Type_Def;
         if Type_Def.F_Has_Limited then
            return Over;
         end if;

         if not Is_AUnit_Part (Root_Type_Declaration (Cur_Node).Unit) then
            return Over;
         end if;

         --  Checking for duplicating types
         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop
            if
              Data.Test_Types.Element (I).Test_Type = Cur_Node.As_Ada_Node
            then
               return Over;
            end if;
         end loop;

         Number_Of_Test_Types := Number_Of_Test_Types + 1;

         declare
            Name   : constant Defining_Name :=
              Cur_Node.As_Basic_Decl.P_Defining_Name;
            N_Span : constant Source_Location_Range := Name.Sloc_Range;
         begin
            Free (Local_TP_Mapping.Type_Name);
            Local_TP_Mapping.Type_Name := new String'(Node_Image (Name));
            Local_TP_Mapping.Type_Sloc :=
              (Natural (N_Span.Start_Line), Natural (N_Span.Start_Column));

            if Is_Test_Case (Cur_Node) then

               Test_Case.Name := new String'(Node_Image (Name));
               Test_Case.Nesting := new String'(Get_Nesting (Cur_Node));

               Data.TC_List.Append (Test_Case);
            else

               Type_Info.Test_Type := Cur_Node.As_Ada_Node;
               Type_Info.Test_Type_Name := new String'(Node_Image (Name));
               Type_Info.Nesting := new String'(Get_Nesting (Cur_Node));

               Data.Test_Types.Append (Type_Info);
            end if;
         end;

         return Over;
      end Get_Records;

      ---------------------
      -- Get_Subprograms --
      ---------------------

      function Get_Subprograms (Node : Ada_Node'Class) return Visit_Status is
         Local_TR_Mapping : TR_Mapping;
         Test_Routine     : Test_Routine_Info;

         Name   : Defining_Name;
         N_Span : Source_Location_Range;

         Owner_Decl : Base_Type_Decl;
         Type_Found : Boolean;
      begin
         if Node.Kind = Ada_Generic_Package_Decl then
            return Over;
         end if;

         if Node.Kind = Ada_Subp_Decl then
            if
              Node.As_Basic_Subp_Decl.
                P_Subp_Decl_Spec.As_Subp_Spec.F_Subp_Kind.Kind /=
                Ada_Subp_Kind_Procedure
            then
               --  Test routine cannot be a function
               return Over;
            end if;
         else
            return Into;
         end if;

         Name   := Node.As_Basic_Decl.P_Defining_Name;
         N_Span := Name.Sloc_Range;

         if Is_Test_Routine (Node.As_Basic_Subp_Decl) then
            Local_TR_Mapping.TR_Name := new String'(Node_Image (Name));
            Local_TR_Mapping.Line    := Natural (N_Span.Start_Line);
            Local_TR_Mapping.Column  := Natural (N_Span.Start_Column);

            Local_TP_Mapping.TR_List.Append (Local_TR_Mapping);
         elsif Is_Test_Related (Node.As_Basic_Subp_Decl) then
            if To_Lower (Node_Image (Name)) = "set_up" then
               Local_TP_Mapping.SetUp_Sloc :=
                 (Natural (N_Span.Start_Line), Natural (N_Span.Start_Column));
            elsif To_Lower (Node_Image (Name)) = "tear_down" then
               Local_TP_Mapping.TearDown_Sloc :=
                 (Natural (N_Span.Start_Line), Natural (N_Span.Start_Column));
            end if;
         end if;

         if not Is_Test_Fixture_Routine (Node.As_Basic_Subp_Decl) then
            --  Test routines with parameters of Test_Case-descendant type
            --  are registered automatically, nothing to do for them.
            return Over;
         end if;

         Test_Routine.TR_Declaration := Node.As_Ada_Node;
         Test_Routine.TR_Text_Name   := new String'(Node_Image (Name));
         Test_Routine.Nesting        := new String'(Get_Nesting (Node));

         declare
            Subp_Span : constant Source_Location_Range := Node.Sloc_Range;
         begin
            Test_Routine.Tested_Sloc := new String'
              (Base_Name (Data.Test_Unit_File_Name.all)
               & ":"
               & Trim (Subp_Span.Start_Line'Img, Both)
               & ":"
               & Trim (Subp_Span.Start_Column'Img, Both)
               & ": "
               & Test_Routine.TR_Text_Name.all
               & ":");
         end;

         Owner_Decl := Tagged_Primitive_Owner
           (Node.As_Basic_Subp_Decl.P_Subp_Decl_Spec);

         Type_Found := False;
         for
           I in Data.Test_Types.First_Index .. Data.Test_Types.Last_Index
         loop

            if Data.Test_Types.Element (I).Test_Type = Owner_Decl then
               Test_Routine.Test_Type_Numb := I;
               Type_Found := True;
               exit;
            end if;

         end loop;

         if Type_Found then
            Data.TR_List.Append (Test_Routine);
         end if;

         return Over;
      end Get_Subprograms;

   begin
      Unit := Bod.F_Item.As_Ada_Node;

      case Unit.Kind is
         when Ada_Package_Decl =>
            Data.Generic_Kind := False;

         when others =>
            Report_Std
              ("gnattest: "
               & Base_Name (The_Unit.Unit.Get_Filename)
               & " is an unsupported kind of unit");
            Set_Source_Status
              (Base_Name (The_Unit.Unit.Get_Filename),
               Bad_Content);
            Appropriate_Source := False;

            return;
      end case;

      Increase_Indent
        (Me,
         "processing " & Node_Image (Unit.As_Basic_Decl.P_Defining_Name)
         &  " (" & Base_Name (The_Unit.Unit.Get_Filename) & ")");

      --  That's quite ulikely for AUnit itself to be included into the
      --  input files, yet still we have to check this.
      if Is_AUnit_Part (The_Unit.Unit) then
         Cmd_Error_No_Help ("trying to process aunit itself");
      end if;

      Local_TP_Mapping.Name := new String'
        (Base_Name (The_Unit.Unit.Get_Filename));

      Data.Test_Unit := The_Unit;
      Data.Test_Unit_Full_Name := new String'
        (Node_Image (Unit.As_Basic_Decl.P_Defining_Name));
      Data.Test_Unit_File_Name := new String'(The_Unit.Unit.Get_Filename);
      Data.Good_For_Suite := False;

      Trace (Me, "Gathering test types");
      Traverse (Unit, Get_Records'Access);
      Trace (Me, "Gathering test routines");
      Traverse (Unit, Get_Subprograms'Access);

      Decrease_Indent (Me, "Traversings finished");

      if
        Data.TR_List.Is_Empty
        and then Data.ITR_List.Is_Empty
        and then Data.TC_List.Is_Empty
      then
         Data.Good_For_Suite := False;
         Set_Source_Status
           (Base_Name (The_Unit.Unit.Get_Filename),
            Processed_In_Vain);
      else
         Data.Good_For_Suite := True;
         Set_Source_Status
           (Base_Name (The_Unit.Unit.Get_Filename),
            Processed);
         if Number_Of_Test_Types = 1 then
            Additional_Mapping.Append (Local_TP_Mapping);
         else
            Report_Std
              ("warning: (gnattest) cannot create mapping for "
               & Data.Test_Unit_File_Name.all);
         end if;
      end if;

      Appropriate_Source := True;
   end Gather_Data;

   -------------------
   -- Is_AUnit_Part --
   -------------------

   function Is_AUnit_Part (Unit : Analysis_Unit) return Boolean is
      File_Name : constant String := Base_Name (Unit.Get_Filename);
   begin
      return
        File_Name'Length > 5
        and then File_Name (File_Name'First .. File_Name'First + 4) = "aunit";
   end Is_AUnit_Part;

   ---------------------
   -- Is_Test_Routine --
   ---------------------

   function Is_Test_Routine (Subp : Basic_Subp_Decl) return Boolean is
      Subp_Name : constant String :=
        To_Lower (Node_Image (Subp.As_Basic_Decl.P_Defining_Name));
   begin
      if not Is_Test_Related (Subp) then
         return False;
      end if;

      --  Checking for predefined AUnit set up and tear down routines.
      if Subp_Name = "set_up" then
         return False;
      end if;

      if Subp_Name = "set_up_case" then
         return False;
      end if;

      if Subp_Name = "tear_down" then
         return False;
      end if;

      if Subp_Name = "tear_down_case" then
         return False;
      end if;

      return True;
   end Is_Test_Routine;

   -----------------------
   --  Is_Test_Related  --
   -----------------------

   function Is_Test_Related (Subp : Basic_Subp_Decl) return Boolean is

      Params : constant Param_Spec_Array :=
        Subp.P_Subp_Decl_Spec.P_Params;
      Param  : Param_Spec;

      Param_Type_Expr : Type_Expr;
      Param_Type_Name : Libadalang.Analysis.Name;
      Param_Type_Decl : Base_Type_Decl;
   begin
      if Params'Length /= 1 then
         return False;
      end if;

      if Is_AUnit_Part (Subp.Unit) then
         return False;
      end if;

      Param := (Params (Params'First));

      declare
         Param_Names : constant Defining_Name_List := F_Ids (Param);
         Param_Names_Size : Natural := 0;
      begin
         for N of Param_Names loop
            Param_Names_Size := Param_Names_Size + 1;
         end loop;

         if Param_Names_Size > 1 then
            return False;
         end if;
      end;

      Param_Type_Expr := Param.F_Type_Expr;
      if Param_Type_Expr.Kind /= Ada_Subtype_Indication then
         return False;
      end if;

      Param_Type_Name := Param_Type_Expr.As_Subtype_Indication.F_Name;

      if Kind (Param_Type_Name) = Ada_Attribute_Ref then
         return False;
      end if;

      Param_Type_Name := Param_Type_Name.P_Relative_Name.As_Name;
      Param_Type_Decl := P_Canonical_Type
        (Param_Type_Name.P_Referenced_Decl.As_Base_Type_Decl);

      Param_Type_Decl := Root_Type_Declaration (Param_Type_Decl);

      if Param_Type_Decl.Is_Null then
         return False;
      end if;

      if Is_AUnit_Part (Param_Type_Decl.Unit) then
         return True;
      else
         return False;
      end if;

   end Is_Test_Related;

   -----------------------------
   -- Is_Test_Fixture_Routine --
   -----------------------------

   function Is_Test_Fixture_Routine (Subp : Basic_Subp_Decl) return Boolean is
      Params : constant Param_Spec_Array :=
        Subp.P_Subp_Decl_Spec.P_Params;
      Param  : Param_Spec;

      Param_Type_Expr : Type_Expr;
      Param_Type_Name : Libadalang.Analysis.Name;
      Param_Type_Decl : Base_Type_Decl;
   begin
      if not Is_Test_Routine (Subp) then
         return False;
      end if;

      Param := Params (Params'First);
      Param_Type_Expr := Param.F_Type_Expr;
      Param_Type_Name := Param_Type_Expr.As_Subtype_Indication.F_Name;
      Param_Type_Name := Param_Type_Name.P_Relative_Name.As_Name;
      Param_Type_Decl := P_Canonical_Type
        (Param_Type_Name.P_Referenced_Decl.As_Base_Type_Decl);

      while not Param_Type_Decl.Is_Null loop
         if
           Node_Image (Param_Type_Decl.As_Basic_Decl.P_Defining_Name) =
           "Test_Case"
           and then Is_AUnit_Part (Param_Type_Decl.Unit)
         then
            return False;
         end if;

         Param_Type_Decl := Parent_Type_Declaration (Param_Type_Decl);
      end loop;

      return True;
   end Is_Test_Fixture_Routine;

end Test.Harness;