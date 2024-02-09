------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2024, AdaCore                    --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Regpat;
with GNAT.Strings;

with Test.Command_Lines;
with Test.Mapping;               use Test.Mapping;
with Test.Subprocess;            use Test.Subprocess;
with Utils_Debug;                use Utils_Debug;
with Utils.Environment;          use Utils.Environment;
with Utils.String_Utilities;     use Utils.String_Utilities;

with TGen.JSON;    use TGen.JSON;
with TGen.Strings; use TGen.Strings;

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

package body Test.Suite_Min is

   Dir_Sep : Character renames
     GNAT.OS_Lib.Directory_Separator;

   procedure Minimize_Unit
     (Unit_Mapping   : TP_Mapping;
      Cov_Cmd        : GNATCOLL.OS.Process.Argument_List;
      Covered, Total : out Natural);
   --  Minimize all the generated tests for all the procedures that have
   --  at least one such test. Use Cov_Cmd as base "gnatcov coverage" command.
   --  Total and Covered correspond to the number of obligations respectively
   --  covered by all the traces and in total for the subprograms that have
   --  generated tests in the unit.

   procedure Minimize_Subp
     (Subp_Mapping   : TR_Mapping;
      Subp_JSON      : TGen.JSON.JSON_Value;
      Cov_Cmd        : GNATCOLL.OS.Process.Argument_List;
      Covered, Total : out Natural);
   --  Minimize the generated tests for the given subprogram. These are present
   --  in the Subp_JSON object. Use Cov_Cmd as base "gnatcov coverage" command.
   --  Total and Covered correspond to the number of obligations respectively
   --  covered  by all the traces and in total for the subprogram.

   procedure Get_Cov_For_Trace
     (Trace     : String;
      Subp_Sloc : String;
      Subp_UID  : String;
      Cov_Cmd   : GNATCOLL.OS.Process.Argument_List;
      Load_Ckpt : Boolean;
      Covered   : out Natural;
      Total     : out Natural);
   --  Generate a coverage report from Trace, for the subprogram for which the
   --  declaration is at Subp_Sloc, and from a subprogram specific checkpoint
   --  if Load_Ckpt is True. Create or overwrite the checkpoint with the new
   --  coverage information in all cases.
   --
   --  Cov_Cmd contains the base arguments to be used (command name,
   --  project related switches, level) in the "gnatcov coverage" invocation.
   --
   --  Total corresponds to the total number of obligations to be
   --  covered, and Covered corresponds to the number of such obligations that
   --  were covered by the trace, and the coverage data in the checkpoint.

   --------------------
   -- Minimize_Suite --
   --------------------

   procedure Minimize_Suite (Cmd : Command_Line) is
      use GNATCOLL.OS.Process;
      use Test.Command_Lines.Test_String_Switches;

      Setup_Cmd   : Argument_List;
      Instr_Cmd   : Argument_List;
      Build_Cmd   : Argument_List;
      Run_Cmd     : Argument_List;
      Cov_Cmd     : Argument_List;
      Harness_Cmd : Argument_List;
      Trace_Dir   : constant String :=
        Tool_Temp_Dir.all & Dir_Sep & "harness_traces" & Dir_Sep;
      Trace_Arr   : aliased File_Array_Access;
      GCVRT_Dir   : constant String :=
        Tool_Temp_Dir.all & Dir_Sep & "gcvrt";
      GCVRT_Prj   : constant String :=
        GCVRT_Dir & Dir_Sep & "share" & Dir_Sep & "gpr" & Dir_Sep
        & "gnatcov_rts.gpr";
      Ext_Acc     : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Ext         : constant String := Ext_Acc.all;
      Cov_Lev_Sw  : constant String_Ref :=
        Arg (Cmd, Test.Command_Lines.Cov_Level);
      Ret_Status  : Integer;
      Env         : Environment_Dict;

      Real_Verbose : constant Boolean :=
        Test.Common.Verbose or else Debug_Flag_1 or else Debug_Flag_2;
      --  -d1 can be used as a verbose switch, but which does not print other
      --  harness processing information.
      --  -d2 also instructs gnatcov to keep temporary files around, useful
      --  to investigate crashes.

      Covered, Total : Natural := 0;
      --  Resp. number of obligations covered and in total (in the subprograms
      --  that have generated tests), for the whole testsuite.
   begin
      GNAT.OS_Lib.Free (Ext_Acc);

      --  First, setup the coverage runtime. As gnatfuzz currently setups their
      --  own coverage runtime and does not work when gnatcov_rts is already in
      --  the GPR_PROJECT_PATH, we can assume that there won't be a prebuilt
      --  coverage runtime, and instead must install our own.

      Report_Std ("Minimizing testsuite:");
      Report_Std ("Instrument test harness for coverage");

      --  TODO??? Remove this as soon as we figure out a plan to have this only
      --  done once for both gnatfuzz and gnattest.
      --  See eng/ide/libadalang-tools#144

      Setup_Cmd.Append ("gnatcov" & Ext);
      Setup_Cmd.Append ("setup");
      Setup_Cmd.Append ("--prefix=" & GCVRT_Dir);
      if not Real_Verbose then
         Setup_Cmd.Append ("-q");
      end if;
      Run
        (Setup_Cmd,
         "gnatcov setup invocation",
         Out_To_Null => not Real_Verbose);

      --  Then, instrument the harness project for coverage, instructing
      --  gnatcov to use the manual dump/reset indications present in the
      --  harness.

      Instr_Cmd.Append ("gnatcov" & Ext);
      Instr_Cmd.Append ("instrument");
      Instr_Cmd.Append ("-P" & Harness_Dir_Str.all & "test_driver.gpr");
      Populate_X_Vars (Instr_Cmd, Cmd);
      Instr_Cmd.Append ("--dump-trigger=manual");
      Instr_Cmd.Append ("--dump-filename-simple");
      Instr_Cmd.Append ("--runtime-project=" & GCVRT_Prj);
      if not Real_Verbose then
         Instr_Cmd.Append ("-q");
      end if;

      if Debug_Flag_2 then
         Instr_Cmd.Append ("--save-temps");
      end if;

      --  TODO ??? Deal with Origin_Project & coverage level shenanigans later
      --
      --  Origin_Project was introduced so that gnattest does not need to
      --  somehow propagate the contents of the Coverage package to the
      --  test_harness project. We can't use it as otherwise the manual dump
      --  indication in this project are not parsed, but then gnatcov can't
      --  pick up the coverage level from the project file...
      --
      --  We either have to inspect the package to propagate the coverage level
      --  on the command line, or modify gnatcov to disregard the
      --  Origin_Project attribute when searching for manual indications.

      if Present (Cov_Lev_Sw) then
         Instr_Cmd.Append ("-c" & Cov_Lev_Sw.all);
      end if;
      Run
        (Instr_Cmd,
         "Harness instrumentation",
         Out_To_Null => not Real_Verbose);

      --  Build the instrumented harness

      Report_Std ("Build test harness");

      Build_Cmd.Append ("gprbuild" & Ext);
      Build_Cmd.Append ("-P" & Harness_Dir_Str.all & "test_driver.gpr");
      Populate_X_Vars (Build_Cmd, Cmd);
      Build_Cmd.Append ("--src-subdirs=gnatcov-instr");
      Build_Cmd.Append ("--implicit-with=" & GCVRT_Prj);
      if not Real_Verbose then
         Build_Cmd.Append ("-q");
      end if;
      Run
        (Build_Cmd,
         What        => "Instrumented test harness build",
         Out_To_Null => not Real_Verbose);

      Append (Trace_Arr, Create (+Trace_Dir));
      Create_Dirs (Trace_Arr);
      Unchecked_Free (Trace_Arr);

      --  Run the instrumented harness. Put the traces in the Trace_Dir through
      --  the env variable. Since we need to pass down some environment
      --  variable we can't use our Run wrapper.

      Report_Std ("Execute testsuite");

      Run_Cmd.Append
        (Harness_Dir_Str.all & Dir_Sep & "test_runner" & Ext);
      Env.Insert ("GNATCOV_TRACE_FILE", Trace_Dir);
      if Real_Verbose then
         PP_Cmd (Run_Cmd, "Running");
      end if;

      Ret_Status := Run
        (Run_Cmd,
         Stdout      =>
           (if Real_Verbose
            then GNATCOLL.OS.FS.Standout
            else GNATCOLL.OS.FS.Null_FD),
         Env         => Env,
         Inherit_Env => True);
      if Ret_Status /= 0 then
         Report_Err
           ("Harness execution failed.");
         PP_Cmd (Run_Cmd, "Command was");
         Utils.Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Report_Std ("Remove tests based on coverage");

      Cov_Cmd.Append ("gnatcov" & Ext);
      Cov_Cmd.Append ("coverage");
      Cov_Cmd.Append ("-P" & Harness_Dir_Str.all & "test_driver.gpr");
      Populate_X_Vars (Cov_Cmd, Cmd);
      Cov_Cmd.Append
        ((if Present (Cov_Lev_Sw) then "-c" & Cov_Lev_Sw.all else ""));

      --  Use the report format to extract a synthetic coverage metric, as
      --  parsing the XML document seems a bit overkill for what we are trying
      --  to do.

      Cov_Cmd.Append ("-areport");
      Cov_Cmd.Append ("--all-messages");
      for Mapping of Test.Mapping.Mapping loop
         for Unit_Mapping of Mapping.Test_Info loop
            declare
               Unit_Cov, Unit_Tot : Natural;
            begin
               Minimize_Unit (Unit_Mapping, Cov_Cmd, Unit_Cov, Unit_Tot);
               Covered := Covered + Unit_Cov;
               Total   := Total + Unit_Tot;
            end;
         end loop;
      end loop;

      if Real_Verbose then
         Report_Std
           ("Covered" & Covered'Image & " out of" & Total'Image
            & " obligation"
            & (if Total /= 0
               then "s (" & Image (Integer'(Covered * 100 / Total)) & "%)"
               else ""));
      end if;

      Report_Std ("Re-generating harness from minimized suite");

      Harness_Cmd.Append ("gnattest" & Ext);

      --  Copy the command line of the current gnattest invocation, filtering
      --  out the --gen-test-vectors and --minimize arguments.

      for J in 1 .. Argument_Count loop
         declare
            Sw : constant String := Argument (J);
         begin
            if not Has_Prefix (Sw, "--gen-test-vectors")
              and then not Has_Prefix (Sw, "--minimize")
            then
               Harness_Cmd.Append (Sw);
            end if;
         end;
      end loop;
      Run
        (Harness_Cmd,
         What        => "Harness re-generation",
         Out_To_Null => not Real_Verbose);
   end Minimize_Suite;

   -------------------
   -- Minimize_Unit --
   -------------------

   procedure Minimize_Unit
     (Unit_Mapping   : TP_Mapping;
      Cov_Cmd        : GNATCOLL.OS.Process.Argument_List;
      Covered, Total : out Natural)
   is
      use GNAT.Strings;
      Unit_JSON     : JSON_Value;
      Unit_JSON_Str : GNAT.OS_Lib.String_Access;
      Unit_Name     : constant Ada_Qualified_Name :=
        To_Qualified_Name (Unit_Mapping.TP_Name.all);
      Unit_Test_VF  : constant Virtual_File :=
        Create (+(JSON_Test_Dir.all & Dir_Sep & To_JSON_filename (Unit_Name)));
      Unit_Write_F  : Writable_File;
      Unit_Cmd      : GNATCOLL.OS.Process.Argument_List := Cov_Cmd;

   begin
      Covered := 0;
      Total   := 0;

      --  No JSON tests found for this unit, skip it

      if not Unit_Mapping.Has_Gen_Tests then
         return;
      end if;

      --  Load the JSON tests for the whole unit

      Unit_JSON_Str := GNATCOLL.VFS.Read_File (Unit_Test_VF);
      if Unit_JSON_Str = null then
         return;
      end if;

      Unit_JSON := Read (Unit_JSON_Str.all, +Unit_Test_VF.Full_Name);
      Free (Unit_JSON_Str);

      --  Reduce each subprogram, if it has tests

      if Test.Common.Verbose or else Debug_Flag_1 then
         Report_Std ("Minimizing tests for " & To_Ada (Unit_Name));
      end if;

      Unit_Cmd.Append ("--units=" & To_Ada (Unit_Name));

      for TR_Mapping of Unit_Mapping.TR_List loop
         declare
            Subp_UID  : String renames TR_Mapping.TR_Hash.all;
            Subp_JSON : constant JSON_Value := Unit_JSON.Get (Subp_UID);
            Subp_Cov, Subp_Tot : Natural;
         begin
            if not Subp_JSON.Is_Empty then

               --  JSON_Value has a by-reference semantic, so the underlying
               --  JSON tree will get modified.

               Minimize_Subp
                 (TR_Mapping, Subp_JSON, Unit_Cmd, Subp_Cov, Subp_Tot);
               Covered := Covered + Subp_Cov;
               Total   := Total + Subp_Tot;
            end if;
         end;
      end loop;

      --  Write back the minimized tests

      Unit_Write_F := Write_File (Unit_Test_VF);
      if Unit_Write_F = Invalid_File then
         Report_Err ("Warning: could not write to " & (+Unit_Test_VF.Full_Name)
                     & ". Tests were not minimized.");
         return;
      end if;
      begin
         Write (Unit_Write_F, Unit_JSON.Write (Compact => True));
         Close (Unit_Write_F);
      exception
         when Ada.Text_IO.Use_Error =>
            Report_Err
              ("Error while writing the minimized tests in "
               & (+Unit_Test_VF.Full_Name) & ":" & ASCII.LF
               & (+Error_String (Unit_Write_F)));
      end;
   end Minimize_Unit;

   -------------------
   -- Minimize_Subp --
   -------------------

   procedure Minimize_Subp
     (Subp_Mapping   : TR_Mapping;
      Subp_JSON      : TGen.JSON.JSON_Value;
      Cov_Cmd        : GNATCOLL.OS.Process.Argument_List;
      Covered, Total : out Natural)
   is
      Trace_Dir         : constant String :=
        Tool_Temp_Dir.all & Dir_Sep & "harness_traces" & Dir_Sep;
      Subp_UID          : String renames Subp_Mapping.TR_Hash.all;
      Current_Trace_Idx : Natural := 0;
      First_Cov         : Boolean := True;
      Origin_Test_Vec   : constant JSON_Array :=
        Subp_JSON.Get ("test_vectors");
      Filtered_Tests    : JSON_Array;
      Subp_Sloc         : constant String :=
        Subp_Mapping.Decl_File.all & ":" & Image (Subp_Mapping.Line);
   begin
      Covered := 0;
      Total := 0;

      --  First get a baseline coverage from the single trace originating from
      --  user written test. Those will never get minimized.
      --  The trace is named <Subp_UID>.srctrace.

      declare
         User_Test_Trace : constant Virtual_File :=
           Create (+(Trace_Dir & Dir_Sep & Subp_UID & ".srctrace"));
      begin
         if GNATCOLL.VFS.Is_Regular_File (User_Test_Trace) then
            Get_Cov_For_Trace
              (+User_Test_Trace.Full_Name,
               Subp_Sloc,
               Subp_UID,
               Cov_Cmd,
               False,
               Covered,
               Total);
            First_Cov := False;
         end if;
      end;

      --  Then iterate through all the JSON tests. The strong assumption here
      --  is that the traces were created in the same order as the tests are
      --  present in the JSON file. This should be the case in practice as the
      --  generation of the test harness simply iterates on the JSON test
      --  cases, but it is a bit fragile.
      --
      --  TODO??? Add single test case identifiers so that we can better track
      --  duplicates and keep test-to-trace consistency.
      --
      --  If there are more test cases than traces, do not remove the extra
      --  test cases as they could cover more obligations.

      while Current_Trace_Idx < Length (Origin_Test_Vec) loop
         declare
            Trace_Name  : constant Virtual_File :=
              Create (+(Trace_Dir & Dir_Sep & Subp_UID & "-gen-"
                        & Image (Current_Trace_Idx) & ".srctrace"));
            Old_Covered : constant Natural := Covered;
         begin
            if not Is_Regular_File (Trace_Name) then
               Report_Err
                 (Subp_Mapping.TR_Name.all & " found"
                  & Integer'Image (Current_Trace_Idx) & " traces but there are"
                  & Integer'Image (Length (Origin_Test_Vec)) &
                  " tests. Remaining tests will not be reduced.");
               exit;
            end if;
            Get_Cov_For_Trace
              (+Trace_Name.Full_Name,
               Subp_Sloc,
               Subp_UID,
               Cov_Cmd,
               not First_Cov,
               Covered,
               Total);
            First_Cov := False;

            if Covered > Old_Covered then
               Append
                 (Filtered_Tests,
                  Get (Origin_Test_Vec, Current_Trace_Idx + 1));
            elsif Test.Common.Verbose or else Debug_Flag_1 then
               Report_Std
                 (Subp_Mapping.TR_Name.all & ": Removing test"
                  & Current_Trace_Idx'Image);
            end if;
         end;
         Current_Trace_Idx := Current_Trace_Idx + 1;
      end loop;

      --  Replace the original test vector by the filtered one

      Subp_JSON.Set_Field ("test_vectors", Filtered_Tests);
   end Minimize_Subp;

   -----------------------
   -- Get_Cov_For_Trace --
   -----------------------

   procedure Get_Cov_For_Trace
     (Trace     : String;
      Subp_Sloc : String;
      Subp_UID  : String;
      Cov_Cmd   : GNATCOLL.OS.Process.Argument_List;
      Load_Ckpt : Boolean;
      Covered   : out Natural;
      Total     : out Natural)
   is
      use GNATCOLL.OS.Process;
      use Ada.Text_IO;
      use GNAT.Regpat;
      Cmd_Line           : Argument_List := Cov_Cmd;
      Output_Dir         : constant String :=
        Utils.Environment.Tool_Temp_Dir.all & Dir_Sep & "cov_report";
      Output_Filename    : constant String :=
        Output_Dir & Dir_Sep & Subp_UID & ".txt";
      Checkpoint         : constant String :=
        Output_Dir & Dir_Sep & Subp_UID & ".ckpt";
      Output_File        : File_Type;
      Obligations_Regexp : constant Pattern_Matcher :=
        Compile
          ("((?:No)|[0-9]+) coverage obligations? covered out of ([0-9]+)\.");
      Suppress_Out       : constant Boolean :=
        not (Test.Common.Verbose or else Debug_Flag_1);
   begin
      Total := 0;
      Covered := 0;
      Cmd_Line.Append ("-T" & Trace);
      Cmd_Line.Append ("--output-dir=" & Output_Dir);
      Cmd_Line.Append ("--output=" & Output_Filename);
      Cmd_Line.Append ("--subprograms=" & Subp_Sloc);
      if Load_Ckpt then
         Cmd_Line.Append ("-C" & Checkpoint);
      end if;
      Cmd_Line.Append ("--save-checkpoint=" & Checkpoint);
      Run
        (Cmd_Line,
         What        => "Coverage report creation for " & Trace,
         Out_To_Null => Suppress_Out);
      Open (Output_File, In_File, Output_Filename);
      while not End_Of_File (Output_File) loop
         declare
            Line    : constant String := Get_Line (Output_File);
            Matches : Match_Array (0 .. 2);
         begin
            Match (Obligations_Regexp, Line, Matches);
            if Matches (0) /= No_Match then
               if Line (1 .. 2) /= "No" then
                  Covered :=
                    Covered + Natural'Value
                                (Line (Matches (1).First .. Matches (1).Last));
               end if;
               Total :=
                 Total + Natural'Value
                           (Line (Matches (2).First .. Matches (2).Last));
            end if;
         end;
      end loop;
      Close (Output_File);
      if not Suppress_Out then
         Report_Std
           ("Trace " & Trace & " covered " & Image (Covered)
            & " obligation" & (if Covered > 1 then "s" else ""));
      end if;
   end Get_Cov_For_Trace;

end Test.Suite_Min;
