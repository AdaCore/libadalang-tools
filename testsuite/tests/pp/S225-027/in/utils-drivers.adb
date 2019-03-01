------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . D R I V E R                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with Ada.Directories;           use Ada;
with Ada.Exceptions;
with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GNATCOLL.Projects;

with Utils.Environment;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
--  with Utils.Command_Lines.Common.Post;
with Utils.Projects; use Utils.Projects;
--  with Utils.Check_Parameters;
with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Tool_Names;

with Libadalang;           use Libadalang;
with Libadalang.Iterators; use Libadalang.Iterators;

package body Utils.Drivers is

   pragma Warnings (Off);
   use Common_Flag_Switches, Common_Boolean_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   --  See libadalang_env/src/libadalang/ada/testsuite/ada/nameres.adb.

   use Tools;

   procedure Post_Cmd_Line_1 (Cmd : Command_Line);
   --  This is called by Process_Command_Line after the first pass through
   --  the command-line arguments.

   function ASIS_Order_File_Names
     (X : String_Ref_Array) return String_Ref_Array;
   --  ????ASIS (or maybe just gnatmetric) seems to process files in a
   --  strange order. First, all files with bodies, in alphabetical
   --  order. Then all files without bodies, in alphabetical order.
   --  We're temporarily mimicking that here.

   procedure Post_Cmd_Line_1 (Cmd : Command_Line) is
      --  ????See lal_ul-driver.adb
      use Utils.Environment;
   begin
      for Dbg of Arg (Cmd, Command_Lines.Common.Debug) loop
         Set_Debug_Options (Dbg.all);
      end loop;

      Tool_Current_Dir := new String'(Initial_Dir);
   --  Leave Tool_Inner_Dir = null
   end Post_Cmd_Line_1;

   function ASIS_Order_File_Names
     (X : String_Ref_Array) return String_Ref_Array
   is
      use String_Ref_Sets;
      Bodies : String_Ref_Set;

      function Has_Body (S : String_Ref) return Boolean;
      function Has_Body (S : String_Ref) return Boolean is
         Body_String : aliased String := S (1 .. S'Last - 1) & "b";
      begin
         return Contains (Bodies, Body_String'Unchecked_Access);
      end Has_Body;

      Result : String_Ref_Vector;
      use String_Ref_Vectors;
   begin
      for S of X loop
         if Has_Suffix (S.all, ".adb") then
            Insert (Bodies, S);
         end if;
      end loop;

      for S of X loop
         if not Has_Suffix (S.all, ".ads") then
            Append (Result, S);
         end if;
         if Has_Suffix (S.all, ".ads") and then Has_Body (S) then
            Append (Result, S);
         end if;
      end loop;

      for S of X loop
         if Has_Suffix (S.all, ".ads") and then not Has_Body (S) then
            Append (Result, S);
         end if;
      end loop;

      return To_Array (Result);
   end ASIS_Order_File_Names;

   procedure Driver
     (Cmd : in out Command_Line; Tool : in out Tool_State'Class;
      Tool_Package_Name     : String; Needs_Per_File_Output : Boolean := False;
      Preprocessing_Allowed :        Boolean        := True;
      Callback              :        Parse_Callback := null)
   is
      use String_Sets;

      procedure Local_Callback
        (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);
      --  This processes the Common switches, and then calls the tool-specific
      --  Callback passed in.

      procedure Process_Files;

      procedure Set_Ada_Version (Version : Ada_Version_Type);
      --  Set the Ada_Version, from normal switches or from -cargs

      procedure Process_Cargs;
      --  Process arguments in the -cargs sections. This is questionable, given
      --  that lalpp does not call gcc, but we support at least "-cargs -Wx"
      --  for compatibility, at least for now.

      procedure Print_Help;

      procedure Local_Callback
        (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch)
      is
      begin
         Callback (Phase, Swit);
      end Local_Callback;

      Cmd_Text, Cmd_Cargs,
      Project_Switches_Text     : GNAT.OS_Lib.Argument_List_Access;
      Global_Report_Dir         : String_Ref;
      Compiler_Options          : GNAT.OS_Lib.Argument_List_Access;
      Custom_RTS                : GNAT.OS_Lib.String_Access;
      Individual_Source_Options : String_String_List_Map;
      Result_Dirs               : String_String_Map;

      procedure Set_Ada_Version (Version : Ada_Version_Type) is
      begin
         Ada_Version := Version;
         Ada_Version_Switches.Set_Arg (Cmd, Version);
      end Set_Ada_Version;

      procedure Process_Cargs is
         use Ada_Version_Switches;
      begin
         --  We actually only support -W8 and -Wb.
            for Arg of Cmd_Cargs.all loop
            if Arg.all = "-gnatWh" then
               Set_WCEM (Cmd, "h");
            elsif Arg.all = "-gnatWu" then
               Set_WCEM (Cmd, "u");
            elsif Arg.all = "-gnatWs" then
               Set_WCEM (Cmd, "s");
            elsif Arg.all = "-gnatWE" then
               Set_WCEM (Cmd, "E");
            elsif Arg.all = "-gnatW8" then
               Set_WCEM (Cmd, "8");
            elsif Arg.all = "-gnatWb" then
               Set_WCEM (Cmd, "b");

            elsif Arg.all = "-gnat83" then
               Set_Ada_Version (Ada_83);
            elsif Arg.all = "-gnat95" then
               Set_Ada_Version (Ada_95);
            elsif Arg.all in "-gnat2005" | "-gnat05" then
               Set_Ada_Version (Ada_2005);
            elsif Arg.all in "-gnat2012" | "-gnat12" then
               Set_Ada_Version (Ada_2012);

            else
               null; -- Ignore all others
            end if;
         end loop;

         if Arg (Cmd) /= No_Ada_Version then
            Set_Ada_Version (Ada_Version_Switches.Arg (Cmd));
         end if;
      end Process_Cargs;

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
         Counter : Natural := N_File_Names;
         use Text_IO, Directories;
      begin
         --  First compute the Ignored set by looking at all the --ignored
         --  switches.

         for Ignored_Arg of Arg (Cmd, Ignore) loop
            Read_File_Names_From_File (Ignored_Arg.all, Include_One'Access);
         end loop;

         for F_Name of ASIS_Order_File_Names (File_Names (Cmd)) loop
            if not Contains (Ignored, Simple_Name (F_Name.all)) then
               if Arg (Cmd, Verbose) then
                  Put_Line
                    (Standard_Error,
                     "[" & Image (Counter) & "] " & F_Name.all);
               --  ????Use Formatted_Output?
                  elsif not Arg (Cmd, Quiet) and then N_File_Names > 1 then
                  Put
                    (Standard_Error,
                     "Units remaining: " & Image (Counter) & "     " &
                     ASCII.CR);
               end if;
               Counter := Counter - 1;

               --         Utils.Options.No_Argument_File_Specified := False;
               Process_File (Tool, Cmd, F_Name.all);
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
      Environment.Create_Temp_Dir;

      Process_Command_Line
        (Cmd, Cmd_Text, Cmd_Cargs, Project_Switches_Text, Global_Report_Dir,
         Compiler_Options, Project_RTS => Custom_RTS,
         Individual_Source_Options     => Individual_Source_Options,
         Result_Dirs => Result_Dirs, The_Project_Tree => Tool.Project_Tree,
         The_Project_Env               => Tool.Project_Env,
         Needs_Per_File_Output         => Needs_Per_File_Output,
         Preprocessing_Allowed         => Preprocessing_Allowed,
         Tool_Package_Name             => Tool_Package_Name,
         Callback                      => Local_Callback'Unrestricted_Access,
         Post_Cmd_Line_1_Action        => Post_Cmd_Line_1'Access,
         Tool_Temp_Dir                 => Environment.Tool_Temp_Dir.all,
         Print_Help                    => Print_Help'Access);
      --      Utils.Command_Lines.Common.Post.Postprocess_Common (Cmd);
      Process_Cargs;

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
            Dir           : constant String := Arg (Cmd, Output_Directory).all;
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
                  Create_Directory (Dir);
               exception
                  when Name_Error | Use_Error =>
                     Cmd_Error (Cannot_Create);
               end;
            end if;
         end;
      end if;

      Init (Tool, Cmd);
      Process_Files;
      Final (Tool, Cmd);

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
            use Text_IO, Ada.Exceptions, Utils.Tool_Names;
         begin
            Put_Line
              (Standard_Error, Tool_Name & ": " & Exception_Message (X));
         end;
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
      when Utils.Command_Lines.Command_Line_Error =>
         --  Error message has already been printed.
         GNAT.Command_Line.Try_Help;
         Environment.Clean_Up;
      --         GNAT.OS_Lib.OS_Exit (1);
         when Utils.Command_Lines.Command_Line_Error_No_Help |
        Utils.Command_Lines.Command_Line_Error_No_Tool_Name  =>
         --  Error message has already been printed.
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
   end Driver;

end Utils.Drivers;
