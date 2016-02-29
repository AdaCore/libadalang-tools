------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . D R I V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2014, AdaCore                     --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;

with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Asis.Exceptions;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;
with ASIS_UL.Output;
with ASIS_UL.Environment;
with ASIS_UL.Options;
with ASIS_UL.Source_Table.Processing;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

with LAL_UL.Common;   use LAL_UL.Common;
with LAL_UL.Common.Post;
with LAL_UL.Projects; use LAL_UL.Projects;

with LAL_UL.Check_Parameters;

procedure LAL_UL.Driver
  (Prj                   : in out ASIS_UL.Projects.Arg_Project_Type'Class;
   Cmd                   : in out Command_Line;
   Tool_Package_Name     :        String;
   Needs_Per_File_Output :        Boolean        := False;
   No_Preprocessing      :        Boolean        := False;
   Callback              :        Parse_Callback := null)
is
   use String_Ref_Vectors;

   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Exect_Time : Duration;
   use type Ada.Calendar.Time;

   pragma Warnings (Off); -- ????????????????
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   procedure Local_Callback
     (Phase : Parse_Phase;
      Swit  : Dynamically_Typed_Switch);
   --  This processes the Common switches, and then calls the tool-specific
   --  Callback passed in.

   procedure Post_Cmd_Line_1 (Cmd : Command_Line);
   --  This is called by Process_Command_Line after the first pass through the
   --  command-line arguments.

   procedure Local_Callback
     (Phase : Parse_Phase;
      Swit  : Dynamically_Typed_Switch)
   is
      use ASIS_UL.Compiler_Options;
   begin
      case Phase is
         when Cmd_Line_1 =>
            if Common_String_Switches.Valid (Swit.Switch) then
               case Common_Strings'(From_All (Swit.Switch)) is
                  when Run_Time_System =>
                     null; -- ????????????????No need for Store_RTS_Path

                  when Target =>
                     ASIS_UL.Common.Target_From_Command_Line :=
                       new String'(Swit.String_Val.all);
                  when others =>
                     null;
               end case;
            end if;

         when Project_File | Cmd_Line_2 =>
            if Common_Flag_Switches.Valid (Swit.Switch) then
               case Common_Flags'(From_All (Swit.Switch)) is
                  when Avoid_Processing_Gnat_Adc =>
                     Store_Option (Swit.Text.all);
                  when Process_RTL_Units =>
                     ASIS_UL.Options.Process_RTL_Units := True;

                  when others =>
                     null;
               end case;

            elsif Common_String_Switches.Valid (Swit.Switch) then
               case Common_Strings'(From_All (Swit.Switch)) is
                  when Configuration_Pragmas_File |
                    Mapping_File                  |
                    Object_Path_File_Name         =>
                     Store_GNAT_Option_With_Path
                       (Swit.Text.all,
                        Swit.String_Val.all);

                  when Include_Path =>
                     Store_I_Option (Swit.String_Val.all);

                  when others =>
                     null;
               end case;

            elsif Common_Nat_Switches.Valid (Swit.Switch) then
               case Common_Nats'(From_All (Swit.Switch)) is
                  when others =>
                     null;
               end case;

            elsif Common_String_Seq_Switches.Valid (Swit.Switch) then
               null;
            end if;
      end case;

      Callback (Phase, Swit);
   end Local_Callback;

   procedure Post_Cmd_Line_1 (Cmd : Command_Line) is
      use GNAT.OS_Lib, ASIS_UL.Environment, ASIS_UL.Options;
   begin
      for Dbg of Arg (Cmd, Debug) loop
         Set_Debug_Options (Dbg.all);
      end loop;

      ASIS_UL.Options.Mimic_gcc := Arg (Cmd, Outer_Dir) /= null;
      --  We use --outer-dir to detect that we were called from gprbuild.

      ASIS_UL.Options.Incremental_Mode :=
        Arg (Cmd, Incremental) and then not ASIS_UL.Options.Mimic_gcc;
      --  We need to ignore --incremental in the inner invocation, because
      --  --incremental could be specified in package Pretty_Printer of the
      --  project file, which will cause the builder to pass it to the inner
      --  invocation.

      if ASIS_UL.Options.Incremental_Mode_By_Default
        and then Arg (Cmd, Project_File) /= null
      then
         pragma Assert (not ASIS_UL.Options.Mimic_gcc);
         ASIS_UL.Options.Incremental_Mode := True;
      end if;

      if ASIS_UL.Options.Mimic_gcc then
         Tool_Current_Dir := new String'(Arg (Cmd, Outer_Dir).all);
         Tool_Inner_Dir   := new String'(Initial_Dir);
         pragma Assert
           (Full_Name (Arg (Cmd, Outer_Dir).all) = Arg (Cmd, Outer_Dir).all);
         --  ????????????????Should we be using Normalize_Pathname instead of
         --  Ada.Directories.Full_Name? Version of Arg that returns String?
         pragma Assert (Get_Current_Dir = Initial_Dir & "/");
         Change_Dir (Tool_Current_Dir.all);
      else
         Tool_Current_Dir := new String'(Initial_Dir);
         --  Leave Tool_Inner_Dir = null
      end if;

      Verbose_Mode := Arg (Cmd, Verbose);
      Quiet_Mode   := Arg (Cmd, Quiet);
   end Post_Cmd_Line_1;

   Cmd_Text, Project_Switches_Text : GNAT.OS_Lib.Argument_List_Access;
   Global_Report_Dir               : String_Ref;
   Compiler_Options                : GNAT.OS_Lib.Argument_List_Access;
   Individual_Source_Options       : String_String_List_Map;
   Result_Dirs                     : String_String_Map;

   procedure Print_Help;
   procedure Print_Help is
   begin
      Put_Line ("Help!");
   end Print_Help;

begin
   ASIS_UL.Environment.Create_Temp_Dir;

   Process_Command_Line
     (Cmd,
      Cmd_Text,
      Project_Switches_Text,
      Global_Report_Dir,
      Compiler_Options,
      Project_RTS               => ASIS_UL.Compiler_Options.Custom_RTS,
      Individual_Source_Options => Individual_Source_Options,
      Result_Dirs               => Result_Dirs,
      Needs_Per_File_Output     => Needs_Per_File_Output,
      Tool_Package_Name         => Tool_Package_Name,
      Callback                  => Local_Callback'Unrestricted_Access,
      Post_Cmd_Line_1_Action    => Post_Cmd_Line_1'Access,
      Tool_Temp_Dir             => ASIS_UL.Environment.Tool_Temp_Dir.all,
      Print_Help                => Print_Help'Access);
   LAL_UL.Common.Post.Postprocess_Common (Cmd);
   pragma Assert
     (not (ASIS_UL.Options.Incremental_Mode and ASIS_UL.Options.Mimic_gcc));

   for Opt of Compiler_Options.all loop
      ASIS_UL.Compiler_Options.Store_Option (Opt.all);
   end loop;

   declare
      --  Do Process_cargs_Section the old way for now
      use GNAT.Command_Line, ASIS_UL.Compiler_Options;
      Parser : Opt_Parser;
   begin
      Initialize_Option_Scan
        (Parser,
         Project_Switches_Text,
         Section_Delimiters => "cargs rules asis-tool-args");
      --  'rules' is only for gnatcheck, which seems harmless
      Process_cargs_Section (Parser, No_Preprocessing);
      Initialize_Option_Scan
        (Parser,
         Cmd_Text,
         Section_Delimiters => "cargs rules asis-tool-args");
      Process_cargs_Section (Parser, No_Preprocessing);
   end;

   if Global_Report_Dir /= null then
      ASIS_UL.Common.Set_Global_Report_Dir (Global_Report_Dir.all);
   end if;

   if Debug_Flag_C then
      Dump_Cmd (Cmd);
      ASIS_UL.Environment.Print_Command_Line;
   end if;

   ASIS_UL.Source_Table.Processing.Initialize;

   for F of File_Names (Cmd) loop
      ASIS_UL.Options.No_Argument_File_Specified := False;
      ASIS_UL.Source_Table.Add_Source_To_Process (F.all, Prj);
   end loop;

   LAL_UL.Check_Parameters;

   --  ????????????????Stuff from Environment:

   declare
      use GNAT.OS_Lib, ASIS_UL.Environment, ASIS_UL.Options;
   begin
      Copy_Gnat_Adc;
      pragma Assert
        (Get_Current_Dir = Tool_Current_Dir.all & Directory_Separator);

      if not ASIS_UL.Options.Incremental_Mode then
         Change_Dir (Tool_Temp_Dir.all);
         ASIS_UL.Compiler_Options.Store_I_Options;
      end if;

      --  Create output directory if necessary

      if Out_Dir /= null then
         Parallel_Make_Dir (Out_Dir.all, Give_Message => Verbose_Mode);
      end if;
   end;

   declare
      use ASIS_UL.Source_Table, String_String_List_Maps;
      procedure Process (Position : String_String_List_Maps.Cursor);
      procedure Process (Position : String_String_List_Maps.Cursor) is
         SF : constant SF_Id :=
           File_Find (Key (Position).all, Use_Short_Name => True);
      begin
         pragma Assert (Present (SF));
         Add_Compilation_Switches (SF, Element (Position));
      end Process;
   begin
      Iterate (Individual_Source_Options, Process'Access);
   end;

   declare
      use ASIS_UL.Source_Table, String_String_Maps;
      procedure Process (Position : String_String_Maps.Cursor);
      procedure Process (Position : String_String_Maps.Cursor) is
         SF : constant SF_Id :=
           File_Find (Key (Position).all, Use_Short_Name => True);
      begin
         if Present (SF) then
            Set_Result_Dir (SF, Element (Position).all);
         end if;
      end Process;
   begin
      Iterate (Result_Dirs, Process'Access);
   end;

   --  In Incremental_Mode, we invoke the builder instead of doing the normal
   --  tool processing. The inner invocations of this tool invoked by the
   --  builder will do the normal tool processing.

   if ASIS_UL.Options.Incremental_Mode then
      ASIS_UL.Environment.Call_Builder;
   else
      ASIS_UL.Source_Table.Processing.Process_Sources;
   end if;

   ASIS_UL.Source_Table.Processing.Finalize;

   ASIS_UL.Environment.Clean_Up;

   if ASIS_UL.Options.Compute_Timing then
      Exect_Time := Ada.Calendar.Clock - Time_Start;
      ASIS_UL.Output.Info ("Execution time:" & Exect_Time'Img);
   end if;

   ASIS_UL.Output.Close_Log_File;

   if not ASIS_UL.Options.Incremental_Mode then
      if not ASIS_UL.Source_Table.Processing
          .All_Files_Successfully_Processed
      then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end if;

   ASIS_UL.Main_Done := True;

exception
   when X : LAL_UL.Command_Lines.Command_Line_Error =>
      ASIS_UL.Output.Error (Ada.Exceptions.Exception_Message (X));
      --  Should we print here or at the raise????
      GNAT.Command_Line.Try_Help;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Environment.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

   when ASIS_UL.Common.Fatal_Error =>
      --  Just a trap; all the diagnostic messages should already have been
      --  generated.
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Environment.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

   when Ex : Asis.Exceptions
     .ASIS_Inappropriate_Context | Asis.Exceptions
     .ASIS_Inappropriate_Container | Asis.Exceptions
     .ASIS_Inappropriate_Compilation_Unit | Asis.Exceptions
     .ASIS_Inappropriate_Element | Asis.Exceptions
     .ASIS_Inappropriate_Line | Asis.Exceptions
     .ASIS_Inappropriate_Line_Number | Asis.Exceptions
     .ASIS_Failed =>

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_ASIS_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      ASIS_UL.Output.Report_Unhandled_Exception (Ex);
      ASIS_UL.Environment.Clean_Up;
      GNAT.OS_Lib.OS_Exit (1);

end LAL_UL.Driver;
