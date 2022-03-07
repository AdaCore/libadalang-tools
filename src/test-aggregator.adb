------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2022, AdaCore                    --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Text_IO;                use Ada.Text_IO;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Test.Common;                use Test.Common;

with Utils.Command_Lines;        use Utils.Command_Lines;
with Utils.Environment;          use Utils.Environment;

package body Test.Aggregator is

   Me : constant Trace_Handle := Create ("Aggregator", Default => Off);

   Test_Out_File : constant String := "test.out";

   Empty_Arg_List : constant Argument_List_Access :=
     Argument_String_To_List ("");

   Target_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);

   type TD_Status is
     (Waiting, Processing, Analysis, Invalid, Done);

   package TD_Tables is new
     Ada.Containers.Indefinite_Ordered_Maps (String, TD_Status);
   use TD_Tables;

   TD_Table : TD_Tables.Map := Empty_Map;

   function Get_Next_Driver_To_Run return String;
   --  Returns the path to next test driver that has not been run yet

   function Get_Next_Driver_To_Analyze return String;
   --  Returns the path to next test driver that has been run but the output
   --  has not yet been processed.

   procedure Mark_As_Processing (S : String);
   --  Marks the test driver as being processed

   procedure Mark_As_Done (S : String);
   --  Marks the test driver as processed

   procedure Mark_As_Invalid (S : String);
   --  Marks the test driver as invalid

   procedure Mark_As_Analysis (S : String);
   --  Marks the test driver good for analysis

   function Unfinished_Processes return Boolean;
   --  Indicates if there are still any test drivers that have not terminated
   --  yet.

   No_Process    : constant Natural := 0;
   First_Process : constant Natural := 1;

   Currently_Running : Natural := 0;
   --  The number of test drivers started and in unknown state

   subtype Full_Driver_Process_Table_Idx is Natural
     range No_Process .. Natural'Last;

   subtype Driver_Process_Table_Idx is Natural
     range First_Process .. Natural'Last;

   type Driver_Process is record
      Name : String_Access := new String'("");
      PId : Process_Id     := Invalid_Pid;
   end record;

   type Driver_Process_Table_Type is array (Positive range <>)
     of Driver_Process;

   type Driver_Process_Table_Type_Access is access Driver_Process_Table_Type;

   Driver_Process_Table : Driver_Process_Table_Type_Access;

   Last_Stored   : Full_Driver_Process_Table_Idx := No_Process;
   Last_Finished : Full_Driver_Process_Table_Idx := No_Process;
   For_Analysis  : Full_Driver_Process_Table_Idx := No_Process;

   Last_Stored_Old, Last_Finished_Old : Full_Driver_Process_Table_Idx;

   function Get_Corresponding_Dir
     (Idx : Driver_Process_Table_Idx)
      return String
   is
     (Target_Dirs (Target_Dirs'First + Idx - 1).Display_Full_Name);
      --  Gets the full name of directory from Target_Dirs with correpsonding
      --  index.

   function Not_A_Comment (S : String) return Boolean;
   --  True if S doesn't start with "--  ".

   procedure Run_More_Drivers;
   --  Spawns test drivers up to the selecled Queues_Number

   procedure Set_Terminated_Process_Index;
   --  Waits for the first test driver to terminate and returns its number

   procedure Store_Process_Termination
     (Process : Process_Id;
      Success : Boolean);
   --  Locates the record corresponding to Process in Driver_Process_Table.
   --  Changes the status of the corresponding driver according to Success and
   --  removes the record about this process from Driver_Process_Table;

   function Get_Index_For_New_Process return Driver_Process_Table_Idx;
   --  Returns the index of element in test driver table to store the process.

   procedure Parse_Test_Driver_Output;
   --  Parses output file from dir number For_Analysis.

   Cumulative_Output_Passed  : List_Of_Strings.List
     := List_Of_Strings.Empty_List;
   Cumulative_Output_Failed  : List_Of_Strings.List
     := List_Of_Strings.Empty_List;
   Cumulative_Output_Crashed : List_Of_Strings.List
     := List_Of_Strings.Empty_List;
   Cumulative_Output_Unsure : List_Of_Strings.List
     := List_Of_Strings.Empty_List;

   Total_Passed  : Natural := 0;
   Total_Failed  : Natural := 0;
   Total_Crashed : Natural := 0;

   -------------------------------
   -- Get_Index_For_New_Process --
   -------------------------------

   function Get_Index_For_New_Process return Driver_Process_Table_Idx is
      Idx : Driver_Process_Table_Idx range First_Process .. Queues_Number
        := First_Process;
      Count : Natural := 0;
      Res : Driver_Process_Table_Idx;
   begin
      --  Saving in case new process will fail
      Last_Stored_Old   := Last_Stored;
      Last_Finished_Old := Last_Finished;

      if Last_Finished /= No_Process then
         Res := Last_Finished;
         Last_Stored   := Last_Finished;
         Last_Finished := No_Process;
         return Res;
      end if;

      if Last_Stored /= No_Process then
         Idx := (Last_Stored + 1) mod (Queues_Number + 1);
      end if;

      while Driver_Process_Table (Idx).Name.all /= "" loop
         Count := Count + 1;
         pragma Assert (Count <= Queues_Number);
         Idx := (Idx + 1) mod (Queues_Number + 1);
      end loop;

      Last_Stored := Idx;
      return Idx;
   end Get_Index_For_New_Process;

   --------------------------------
   -- Get_Next_Driver_To_Analyze --
   --------------------------------

   function Get_Next_Driver_To_Analyze return String is
      Cur : TD_Tables.Cursor := TD_Table.First;
   begin
      while Cur /= No_Element loop
         if Element (Cur) = Analysis then
            return Key (Cur);
         end if;
         Next (Cur);
      end loop;
      return "";
   end Get_Next_Driver_To_Analyze;

   ----------------------------
   -- Get_Next_Driver_To_Run --
   ----------------------------

   function Get_Next_Driver_To_Run return String is
      Cur : TD_Tables.Cursor := TD_Table.First;
   begin
      while Cur /= No_Element loop
         if Element (Cur) = Waiting then
            return Key (Cur);
         end if;
         Next (Cur);
      end loop;
      return "";
   end Get_Next_Driver_To_Run;

   ----------------------------------
   -- Set_Terminated_Process_Index --
   ----------------------------------

   procedure Set_Terminated_Process_Index is
      Next_Proc : Process_Id;
      Success   : Boolean;
   begin
      Trace (Me, "Set_Terminated_Process_Index");
      Increase_Indent (Me);
      --  We are waiting for the first driver to terminate.
      Wait_Process (Next_Proc, Success);
      Trace
        (Me,
        "process terminated:"
        & Integer'Image (Pid_To_Integer (Next_Proc))
        & "; success="
        & Boolean'Image (Success));

      while Next_Proc /= Invalid_Pid loop
         Currently_Running := Currently_Running - 1;

         Store_Process_Termination (Next_Proc, Success);

         if Success then
            --  We have a driver output ready for analysis
            exit;
         end if;

         Wait_Process (Next_Proc, Success);
         Trace
           (Me,
           "process terminated:"
           & Integer'Image (Pid_To_Integer (Next_Proc))
           & "; success="
           & Boolean'Image (Success));
      end loop;
      Decrease_Indent (Me);
   end Set_Terminated_Process_Index;

   ----------------------
   -- Mark_As_Analysis --
   ----------------------
   procedure Mark_As_Analysis (S : String) is
      Cur : constant TD_Tables.Cursor := TD_Table.Find (S);
   begin
      if Cur = No_Element then
         Cmd_Error_No_Help ("no test driver " & S & " in source table");
      else
         TD_Table.Replace_Element (Cur, Analysis);
      end if;
   end Mark_As_Analysis;

   ------------------
   -- Mark_As_Done --
   ------------------

   procedure Mark_As_Done (S : String) is
      Cur : constant TD_Tables.Cursor := TD_Table.Find (S);
   begin
      if Cur = No_Element then
         Cmd_Error_No_Help ("no test driver " & S & " in source table");
      else
         TD_Table.Replace_Element (Cur, Done);
      end if;
   end Mark_As_Done;

   ---------------------
   -- Mark_As_Invalid --
   ---------------------

   procedure Mark_As_Invalid (S : String) is
      Cur : constant TD_Tables.Cursor := TD_Table.Find (S);
   begin
      if Cur = No_Element then
         Cmd_Error_No_Help ("no test driver " & S & " in source table");
      else
         TD_Table.Replace_Element (Cur, Invalid);
      end if;
   end Mark_As_Invalid;

   ------------------------
   -- Mark_As_Processing --
   ------------------------

   procedure Mark_As_Processing (S : String) is
      Cur : constant TD_Tables.Cursor := TD_Table.Find (S);
   begin
      if Cur = No_Element then
         Cmd_Error_No_Help ("no test driver " & S & " in source table");
      else
         TD_Table.Replace_Element (Cur, Processing);
      end if;
   end Mark_As_Processing;

   -------------------
   -- Not_A_Comment --
   -------------------

   function Not_A_Comment (S : String) return Boolean is
   begin
      if S'Length < 2 then
         return True;
      end if;

      return S (S'First .. S'First + 1) /= "--";
   end Not_A_Comment;

   ------------------------------
   -- Parse_Test_Driver_Output --
   ------------------------------

   procedure Parse_Test_Driver_Output is
      TD_Output : File_Type;

      Idx1, Idx2 : Integer;

      Suspicious_Output_Global : Boolean := False;
      Suspicious_Output  : Boolean := False;
      Traceback_Possible : Boolean := False;

      type Test_Kinds is (Passed, Failed, Crashed, Unsure);
      Current_Kind : Test_Kinds;

      function Get_Val (S : String) return Integer;
      --  Tries to get an integer value from substring of string between Idx1
      --  and Idx2, if substring is empty; Idx1 or Idx2 is 0 or'Val raises
      --  exception, returns 0.

      procedure Classify_Output
        (S                  :        String;
         Suspicious_Output  :    out Boolean;
         Traceback_Possible : in out Boolean;
         Test_Kind          : out    Test_Kinds);
      --  Looks for patterns that gnattest test reportes puts for passed,
      --  failed or crashed tests. I neither is present, this is most likely
      --  not test driver's output.

      procedure Classify_Output
        (S                  :        String;
         Suspicious_Output  :    out Boolean;
         Traceback_Possible : in out Boolean;
         Test_Kind          :    out Test_Kinds) is
      begin
         Suspicious_Output := True;

         if Index (S, "corresponding test PASSED") /= 0 then
            Suspicious_Output  := False;
            Test_Kind          := Passed;
            Traceback_Possible := False;
         end if;
         if Index (S, "corresponding test FAILED") /= 0 then
            Suspicious_Output  := False;
            Test_Kind          := Failed;
            Traceback_Possible := False;
         end if;
         if Index (S, "corresponding test CRASHED") /= 0 then
            --  Traceback lines look "suspicious to this check, so we have to
            --  put additional flag ON.
            Traceback_Possible := True;
            Suspicious_Output  := False;
            Test_Kind          := Crashed;
         end if;

         if Suspicious_Output then
            if Traceback_Possible then
               --  Trailing traceback.
               Test_Kind := Crashed;
            else
               --  Something unexpected.
               Test_Kind := Unsure;
            end if;
         end if;

      end Classify_Output;

      function Get_Val (S : String) return Integer is
         Res : Integer;
      begin
         if Idx1 > Idx2 or else Idx1 = 0 or else Idx2 = 0 then
            return 0;
         end if;

         Res := Integer'Value (S (S'First + Idx1 .. S'First + Idx2 - 2));
         return Res;
      exception
         when Constraint_Error =>
            return 0;
      end Get_Val;
   begin
      if For_Analysis = No_Process then
         return;
      end if;

      Open
        (TD_Output,
         In_File,
         Get_Corresponding_Dir (For_Analysis)
         & Directory_Separator
         & Test_Out_File);

      while not End_Of_File (TD_Output) loop
         declare
            S : constant String := Get_Line (TD_Output);
         begin
            if End_Of_File (TD_Output) then
               --  Last line, we need to extract info for overall statistics.
               --  Example: 7 tests run: 6 passed; 1 failed; 0 crashed.
               --  If the line does not follow this format, test driver
               --  probably crashed.
               declare
                  Passed_Add, Failed_Add, Crashed_Add : Integer;
                  Unexpected : Boolean := False;
               begin
                  Idx1 := Index (S, ":");
                  Idx2 := Index (S, "passed");
                  if Idx1 = 0 or else Idx2 = 0 then
                     Unexpected := True;
                  else
                     Passed_Add := Get_Val (S);
                  end if;
                  if not Unexpected then
                     Idx1 := Index (S, ";", Idx2);
                     Idx2 := Index (S, "failed");
                     if Idx1 = 0 or else Idx2 = 0 then
                        Unexpected := True;
                     else
                        Failed_Add := Get_Val (S);
                     end if;
                  end if;
                  if not Unexpected then
                     Idx1 := Index (S, ";", Idx2);
                     Idx2 := Index (S, "crashed");
                     if Idx1 = 0 or else Idx2 = 0 then
                        Unexpected := True;
                     else
                        Crashed_Add := Get_Val (S);
                     end if;
                  end if;

                  if Unexpected then
                     Cumulative_Output_Unsure.Append (S);
                     Cumulative_Output_Unsure.Append
                       ("error: test driver did not terminate properly");
                  else
                     Total_Passed := Total_Passed + Passed_Add;
                     Total_Failed := Total_Failed + Failed_Add;
                     Total_Crashed := Total_Crashed + Crashed_Add;
                  end if;

               end;
            else
               Classify_Output
                 (S, Suspicious_Output, Traceback_Possible, Current_Kind);

               case Current_Kind is
                  when Passed =>
                     Cumulative_Output_Passed.Append (S);
                  when Failed =>
                     Cumulative_Output_Failed.Append (S);
                  when Crashed =>
                     Cumulative_Output_Crashed.Append (S);
                  when Unsure =>
                     Cumulative_Output_Unsure.Append (S);
               end case;

               if Suspicious_Output and then not Traceback_Possible then
                  Suspicious_Output_Global := True;
               end if;
            end if;
         end;
      end loop;

      if Suspicious_Output_Global then
         Report_Std
           ("warning: (gnattest) unexpected output from test driver "
            & Get_Next_Driver_To_Analyze);
      end if;
      Close (TD_Output);

      Mark_As_Done (Get_Next_Driver_To_Analyze);
   end Parse_Test_Driver_Output;

   --------------------------
   -- Process_Drivers_List --
   --------------------------

   procedure Process_Drivers_List is
      Cur : List_Of_Strings.Cursor;

      use List_Of_Strings;
   begin

      if TD_Table.Is_Empty then
         Cmd_Error_No_Help ("No test drivers found in the list(s)");
      end if;

      --  Create subdirs for queues.
      for I in 1 .. Queues_Number loop
         Append
           (Target_Dirs,
            Create
              (+(Tool_Temp_Dir.all
               & Directory_Separator
               & "dir"
               & Trim (Positive'Image (I), Both))));
      end loop;
      Create_Dirs (Target_Dirs);

      --  Copy environment
      if Environment_Dir /= null then
         declare
            Success : Boolean;
         begin
            for J in Target_Dirs'Range loop
               Copy
                 (Create (+Environment_Dir.all),
                  Target_Dirs (J).Full_Name.all,
                  Success);
               if not Success then
                  Cmd_Error_No_Help
                    ("gnattest: cannot copy contents of "
                     & Environment_Dir.all
                     & " to temp dir");
               end if;
            end loop;
         end;
      end if;

      --  Initialise Driver Process Table
      Driver_Process_Table := new
        Driver_Process_Table_Type (First_Process .. Queues_Number);

      while
        Get_Next_Driver_To_Run /= ""
        or else Get_Next_Driver_To_Analyze /= ""
        or else Unfinished_Processes
      loop

         Run_More_Drivers;
         Set_Terminated_Process_Index;
         if For_Analysis /= No_Process then
            Parse_Test_Driver_Output;
         end if;
      end loop;

      if
        Cumulative_Output_Passed.Is_Empty and then
        Cumulative_Output_Failed.Is_Empty and then
        Cumulative_Output_Crashed.Is_Empty and then
        Cumulative_Output_Unsure.Is_Empty
      then
         Cmd_Error_No_Help ("no test drivers terminated succesfully");
      end if;

      if Show_Passed_Tests then
         Cur := Cumulative_Output_Passed.First;
         while Cur /= List_Of_Strings.No_Element loop
            Ada.Text_IO.Put_Line (List_Of_Strings.Element (Cur));
            Next (Cur);
         end loop;
      end if;
      Cur := Cumulative_Output_Failed.First;
      while Cur /= List_Of_Strings.No_Element loop
         Ada.Text_IO.Put_Line (List_Of_Strings.Element (Cur));
         Next (Cur);
      end loop;
      Cur := Cumulative_Output_Crashed.First;
      while Cur /= List_Of_Strings.No_Element loop
         Ada.Text_IO.Put_Line (List_Of_Strings.Element (Cur));
         Next (Cur);
      end loop;
      Cur := Cumulative_Output_Unsure.First;
      while Cur /= List_Of_Strings.No_Element loop
         Ada.Text_IO.Put_Line (List_Of_Strings.Element (Cur));
         Next (Cur);
      end loop;

      Ada.Text_IO.Put_Line
        (Trim
           (Integer'Image
                (Total_Failed + Total_Passed + Total_Crashed),
            Both)
         & " tests run: "
         & Trim (Integer'Image (Total_Passed), Both)
         & " passed; "
         & Trim (Integer'Image (Total_Failed), Both)
         & " failed; "
         & Trim (Integer'Image (Total_Crashed), Both)
         & " crashed.");

   end Process_Drivers_List;

   ----------------------
   -- Run_More_Drivers --
   ----------------------

   procedure Run_More_Drivers is
      Tmp          : String_Access;
      Next_Process : Process_Id;
      Idx          : Driver_Process_Table_Idx;

      Current_Dir : constant String := Get_Current_Dir;
   begin
      Trace (Me, "Run_More_Drivers");
      Increase_Indent (Me);
      while Currently_Running < Queues_Number loop

         Trace (Me, "currently running:" & Natural'Image (Currently_Running));

         Tmp := new String'(Get_Next_Driver_To_Run);
         exit when Tmp.all = "";

         Trace (Me, "test driver: " & Tmp.all);

         Idx := Get_Index_For_New_Process;

         Trace
           (Me,
            "index for new process:" & Driver_Process_Table_Idx'Image (Idx));

         --  We need to spawn it at the right place.
         Change_Dir (Get_Corresponding_Dir (Idx));
         Trace
           (Me,
            "switching dir: " & Get_Current_Dir);
         Next_Process :=
           Non_Blocking_Spawn
             (Tmp.all,
              Empty_Arg_List.all,
              Get_Corresponding_Dir (Idx)
              & Directory_Separator
              & Test_Out_File,
              True);

         Trace
           (Me,
            "pid for new process:"
            & Integer'Image (Pid_To_Integer (Next_Process)));

         Change_Dir (Current_Dir);

         if Next_Process = Invalid_Pid then
            Report_Std
              ("warning: (gnattest) cannot execute test driver " & Tmp.all);
            Mark_As_Invalid (Tmp.all);

            --  Rolling back Last_Stored and Last_Finished.
            Last_Stored   := Last_Stored_Old;
            Last_Finished := Last_Finished_Old;
         else
            Mark_As_Processing (Tmp.all);
            Currently_Running := Currently_Running + 1;
            Driver_Process_Table (Idx) := (new String'(Tmp.all), Next_Process);
         end if;

         Free (Tmp);
      end loop;
      Decrease_Indent (Me);
   end Run_More_Drivers;

   -------------------------------
   -- Store_Process_Termination --
   -------------------------------

   procedure Store_Process_Termination
     (Process : Process_Id;
      Success : Boolean)
   is
      Idx : Full_Driver_Process_Table_Idx range No_Process .. Queues_Number
        := No_Process;
   begin
      Trace (Me, "Store_Process_Termination");
      Increase_Indent (Me);
      for J in First_Process .. Queues_Number loop
         if Driver_Process_Table (J).PId = Process then
            Idx := J;
            exit;
         end if;
      end loop;
      Trace (Me, "recovered index:" & Integer'Image (Idx));

      pragma Assert (Idx in First_Process .. Queues_Number);

      if Success then
         Trace (Me, " for pid:" & Integer'Image (Pid_To_Integer (Process)));
         Mark_As_Analysis (Driver_Process_Table (Idx).Name.all);
         For_Analysis := Idx;
      else
         Report_Std
           ("warning: (gnattest) test driver "
            & Driver_Process_Table (Idx).Name.all
            & " crashed");

         Mark_As_Invalid (Driver_Process_Table (Idx).Name.all);
         For_Analysis := No_Process;
      end if;

      Driver_Process_Table (Idx) :=
        (Name => new String'(""), PId => Invalid_Pid);
      Last_Finished := Idx;
      Decrease_Indent (Me);
   end Store_Process_Termination;

   --------------------------
   -- Unfinished_Processes --
   --------------------------

   function Unfinished_Processes return Boolean is
      Cur : TD_Tables.Cursor := TD_Table.First;
   begin
      while Cur /= No_Element loop
         if Element (Cur) = Processing then
            return True;
         end if;
         Next (Cur);
      end loop;
      return False;
   end Unfinished_Processes;

   -------------------------
   -- Add_Drivers_To_List --
   -------------------------

   procedure Add_Drivers_To_List (File_Name : String) is
      TD_List_File : File_Type;

      Tmp : String_Access;
      Idx : Integer;
   begin
      --  Fill up the source table.
      Open
        (TD_List_File,
         In_File,
         File_Name);

      while not End_Of_File (TD_List_File) loop
         Tmp := new String'(Get_Line (TD_List_File));
         if Not_A_Comment (Tmp.all) and then Tmp.all /= "" then
            if Aggregate_Subdir_Name.all /= "" then
               Idx :=
                 Index (Tmp.all, [1 => Directory_Separator], Backward);
               if Idx /= 0 then
                  TD_Table.Include
                    (Tmp (Tmp.all'First .. Idx)
                     & Aggregate_Subdir_Name.all
                     & Directory_Separator
                     & Tmp (Idx + 1 .. Tmp.all'Last),
                     Waiting);
               else
                  TD_Table.Include
                    (Aggregate_Subdir_Name.all
                     & Directory_Separator
                     & Tmp.all,
                     Waiting);
               end if;
            else
               TD_Table.Include (Tmp.all, Waiting);
            end if;
         end if;
         Free (Tmp);
      end loop;

      Close (TD_List_File);
   end Add_Drivers_To_List;

end Test.Aggregator;
