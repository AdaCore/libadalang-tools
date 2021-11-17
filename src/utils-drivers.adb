------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Directories; use Ada;
with Ada.Exceptions;
with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;

with Utils.Command_Lines.Common;   use Utils.Command_Lines.Common;
with Utils.Environment;
with Utils.Err_Out;
with Utils.Projects; use Utils.Projects;
with Utils.Projects.Aggregate;
with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Tool_Names;

with Libadalang.Iterators; use Libadalang.Iterators;

package body Utils.Drivers is

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches;

   --  See libadalang_env/src/libadalang/ada/testsuite/ada/nameres.adb.

   use Tools;
   use type GNAT.OS_Lib.String_Access;

   procedure Driver
     (Cmd                   : in out Command_Line;
      Tool                  : in out Tool_State'Class;
      Tool_Package_Name     :        String;
      Preprocessing_Allowed :        Boolean        := True;
      Callback              :        Parse_Callback := null)
   is
      use String_Sets;

      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch);
      --  This processes the Common switches, and then calls the tool-specific
      --  Callback passed in.

      procedure Process_Files;

      procedure Print_Help;

      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch)
      is
      begin
         if Callback /= null then
            Callback (Phase, Swit);
         end if;
      end Local_Callback;

      Global_Report_Dir   : String_Ref;

      procedure Include_One (File_Name : String);
      --  Include File_Name in the Ignored set below

      Ignored : String_Set;
      --  Set of file names mentioned in the --ignore=... switch

      procedure Include_One (File_Name : String) is
      begin
         Include (Ignored, File_Name);
      end Include_One;

      procedure Process_Files is
         N_File_Names : constant Natural :=
           Num_File_Names (Cmd) - Arg_Length (Cmd, Ignore);

         Counter        : Natural := N_File_Names;
         Has_Syntax_Err : Boolean := False;

         use Directories;
      begin
         --  First compute the Ignored set by looking at all the --ignored
         --  switches.

         for Ignored_Arg of Arg (Cmd, Ignore) loop
            Read_File_Names_From_File (Ignored_Arg.all, Include_One'Access);
         end loop;

         for F_Name of File_Names (Cmd) loop
            if not Contains (Ignored, Simple_Name (F_Name.all)) then
               if Arg (Cmd, Verbose) then
                  Err_Out.Put ("[\1] \2\n", Image (Counter), F_Name.all);
               elsif not Arg (Cmd, Quiet) and then N_File_Names > 1 then
                  Err_Out.Put ("Units remaining: \1     \r", Image (Counter));
               end if;

               Counter := Counter - 1;
               Has_Syntax_Err := False;
               Process_File (Tool, Cmd, F_Name.all, Counter, Has_Syntax_Err);
               if Has_Syntax_Err and then not Utils.Syntax_Errors then
                  Utils.Syntax_Errors := True;
               end if;
            end if;

         end loop;
         pragma Assert (Counter = 0);
      end Process_Files;

      procedure Print_Help is
      begin
         Tool_Help (Tool);
      end Print_Help;

   --  Start of processing for Driver

   begin

      Process_Command_Line
        (Cmd,
         Global_Report_Dir,
         The_Project_Tree          => Tool.Project_Tree,
         The_Project_Env           => Tool.Project_Env,
         Preprocessing_Allowed     => Preprocessing_Allowed,
         Tool_Package_Name         => Tool_Package_Name,
         Callback                  => Local_Callback'Unrestricted_Access,
         Print_Help                => Print_Help'Access);
--      Utils.Command_Lines.Common.Post.Postprocess_Common (Cmd);

      if Debug_Flag_C then
         Dump_Cmd (Cmd);
      end if;

--      Utils.Check_Parameters; -- ????Move into Init?

      --  ????????????????Stuff from Environment:

      declare
         use GNAT.OS_Lib, Environment;
      begin
         Copy_Gnat_Adc;
         pragma Assert
           (Get_Current_Dir = Tool_Current_Dir.all & Directory_Separator);

--         if not Incremental_Mode then
--            Change_Dir (Tool_Temp_Dir.all);
--            Utils.Compiler_Options.Store_I_Options;
--         end if;

         --  Create output directory if necessary

--         if Out_Dir /= null then
--            Parallel_Make_Dir (Out_Dir.all, Give_Message => Verbose_Mode);
--         end if;
      end;

      --  In Incremental_Mode, we invoke the builder instead of doing the
      --  normal tool processing. The inner invocations of this tool invoked by
      --  the builder will do the normal tool processing.

--      if Utils.Options.Incremental_Mode then
--         Environment.Call_Builder;
--      else
--         Utils.Source_Table.Processing.Process_Sources;
--      end if;

      --  Create output directory if necessary

      if Present (Arg (Cmd, Output_Directory)) then
         declare
            Dir : constant String := Arg (Cmd, Output_Directory).all;
            Cannot_Create : constant String :=
              "cannot create directory '" & Dir & "'";
            use Directories;
         begin
            if Exists (Dir) then
               if Kind (Dir) /= Directory then
                  Cmd_Error (Cannot_Create & "; file already exists");
               end if;
            else
               begin
                  Create_Path (Dir);
               exception
                  when Name_Error | Use_Error =>
                     Cmd_Error (Cannot_Create);
               end;
            end if;
         end;
      end if;

      Init (Tool, Cmd);

      if Aggregate.Use_Subprocesses_For_Aggregated_Projects then
         Aggregate.Process_Aggregated_Projects (Cmd, Tool_Package_Name);
      else
         Process_Files;
      end if;

      Final (Tool, Cmd);

      if GNATCOLL.Projects."/="
        (Tool.Project_Tree.Status, GNATCOLL.Projects.Empty)
      then
         GNATCOLL.Projects.Aux.Delete_All_Temp_Files
           (Tool.Project_Tree.Root_Project);
      end if;
      GNATCOLL.Projects.Unload (Tool.Project_Tree.all);
      GNATCOLL.Projects.Free (Tool.Project_Env);
      Environment.Clean_Up;

--      if not Utils.Options.Incremental_Mode then
--         if not Utils.Source_Table.Processing
--             .All_Files_Successfully_Processed
--         then
--            GNAT.OS_Lib.OS_Exit (1);
--         end if;
--      end if;

      Utils.Main_Done := True;

   exception
      when X : File_Not_Found =>
         declare
            use Ada.Exceptions, Utils.Tool_Names;
         begin
            Err_Out.Put ("\1: \2\n", Tool_Name, Exception_Message (X));
         end;
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      when Utils.Command_Lines.Command_Line_Error =>
         --  Error message has already been printed.
         GNAT.Command_Line.Try_Help;
         Environment.Clean_Up;
--         GNAT.OS_Lib.OS_Exit (1);
      when Utils.Command_Lines.Command_Line_Error_No_Help |
        Utils.Command_Lines.Command_Line_Error_No_Tool_Name =>
         --  Error message has already been printed.
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
   end Driver;

end Utils.Drivers;
