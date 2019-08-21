------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                     G N A T T E S T . H A R N E S S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with Utils.Command_Lines; use Utils.Command_Lines;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Test.Common; use Test.Common;

package body Test.Harness is

   Me : constant Trace_Handle := Create ("Harness", Default => Off);
   pragma Unreferenced (Me);

   Harness_Dir : String_Access renames Harness_Dir_Str;

   use List_Of_Strings;
   Suit_List : List_Of_Strings.List;
   --  Storing the names of all suits

   ------------------------
   --  String constants  --
   ------------------------

   --  Unit names:

   Common_Suite_Name          : constant String := "Suite";
   --  Suffixless name of the unit containing a common suite

   Generic_Suite_Name         : constant String := "Gen_Suite";
   --  Suffixless name of the unit containing a generic suite

   Substitution_Suite_Name    : constant String := "Substitution_Suite";
   pragma Unreferenced (Substitution_Suite_Name);
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

end Test.Harness;
