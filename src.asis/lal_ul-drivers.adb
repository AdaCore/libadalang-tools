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

with Ada.Exceptions;

with GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with LAL_UL.Environment;

with LAL_UL.Common;   use LAL_UL.Common;
with LAL_UL.Common.Post;
with LAL_UL.Projects; use LAL_UL.Projects;
with LAL_UL.Tool_Names;
with LAL_UL.Check_Parameters;
with LAL_UL.String_Utilities; use LAL_UL.String_Utilities;

with GNATCOLL.Iconv;

with Langkit_Support.Diagnostics;

with Libadalang;     use Libadalang;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.AST.Types; use Libadalang.AST.Types;

package body LAL_UL.Drivers is

   pragma Warnings (Off); -- ????????????????
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   use Tools;

   procedure Driver
     (Cmd                   : in out Command_Line;
      Tool                  : in out Tool_State'Class;
      Tool_Package_Name     :        String;
      Needs_Per_File_Output :        Boolean        := False;
      No_Preprocessing      :        Boolean        := False;
      Callback              :        Parse_Callback := null)
   is
      pragma Unreferenced (No_Preprocessing);

      use String_Ref_Vectors;

      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch);
      --  This processes the Common switches, and then calls the tool-specific
      --  Callback passed in.

      procedure Post_Cmd_Line_1 (Cmd : Command_Line);
      --  This is called by Process_Command_Line after the first pass through
      --  the command-line arguments.

      procedure Process_Files;

      procedure Local_Callback
        (Phase : Parse_Phase;
         Swit  : Dynamically_Typed_Switch)
      is
      begin
         Callback (Phase, Swit);
      end Local_Callback;

      procedure Post_Cmd_Line_1 (Cmd : Command_Line) is
         --  ????????????????See lal_ul-driver.adb
         use LAL_UL.Environment;
      begin
         for Dbg of Arg (Cmd, Debug) loop
            Set_Debug_Options (Dbg.all);
         end loop;

         Tool_Current_Dir := new String'(Initial_Dir);
         --  Leave Tool_Inner_Dir = null
      end Post_Cmd_Line_1;

      Cmd_Text, Project_Switches_Text : GNAT.OS_Lib.Argument_List_Access;
      Global_Report_Dir               : String_Ref;
      Compiler_Options                : GNAT.OS_Lib.Argument_List_Access;
      Custom_RTS                      : GNAT.OS_Lib.String_Access;
      Individual_Source_Options       : String_String_List_Map;
      Result_Dirs                     : String_String_Map;

      Context : constant Analysis_Context :=
        Create (Charset => GNATCOLL.Iconv.UTF8);
      --  ???Charset is hardwired to UTF8 for now

      function ASIS_Order_File_Names
        (X : String_Ref_Array) return String_Ref_Array;
      --  ????ASIS (or maybe just gnatmetric) seems to process files in a
      --  strange order. First, all files with bodies, in alphabetical
      --  order. Then all files without bodies, in alphabetical order.
      --  We're temporarily mimicking that here.

      function ASIS_Order_File_Names
        (X : String_Ref_Array) return String_Ref_Array
      is
         use String_Ref_Sets;
         Bodies : String_Ref_Set;

         function Has_Body (S : String_Ref) return Boolean;
         function Has_Body (S : String_Ref) return Boolean is
            Body_String : aliased constant String :=
              S (1 .. S'Last - 1) & "b";
         begin
            return Contains (Bodies, Body_String'Unchecked_Access);
         end Has_Body;

         Result : String_Ref_Vector;
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

      procedure Process_Files is
         Counter : Natural := File_Names (Cmd)'Length;
      begin
         for F of ASIS_Order_File_Names (File_Names (Cmd)) loop
--         ASIS_UL.Options.No_Argument_File_Specified := False;
            declare
               File_Name : String renames F.all;
               Unit : constant Analysis_Unit :=
                 Get_From_File (Context, File_Name);
--         pragma Assert (Root (Unit) /= null);
            begin
               if Root (Unit) = null then -- ????????????????
                  return;
               end if;

               if Arg (Cmd, Verbose) then
                  Put_Line ("[" & Image (Counter) & "] " & File_Name);
                  --  ????Use Formatted_Output?
                  Counter := Counter - 1;
               end if;

               if Has_Diagnostics (Unit) then
                  Put_Line ("Errors while parsing " & File_Name);
                  for D of Diagnostics (Unit) loop
                     Put_Line
                       (Langkit_Support.Diagnostics.To_Pretty_String (D));
                  end loop;
               end if;

               --  We continue even in the presence of errors

               Per_File_Action (Tool, Cmd, File_Name, Unit);
            end;
         end loop;
         pragma Assert (Counter = 0);
      end Process_Files;

      procedure Print_Help;
      procedure Print_Help is
      begin
         Tool_Help (Tool);
      end Print_Help;

   begin
      Environment.Create_Temp_Dir;

      Process_Command_Line
        (Cmd,
         Cmd_Text,
         Project_Switches_Text,
         Global_Report_Dir,
         Compiler_Options,
         Project_RTS               => Custom_RTS,
         Individual_Source_Options => Individual_Source_Options,
         Result_Dirs               => Result_Dirs,
         Needs_Per_File_Output     => Needs_Per_File_Output,
         Tool_Package_Name         => Tool_Package_Name,
         Callback                  => Local_Callback'Unrestricted_Access,
         Post_Cmd_Line_1_Action    => Post_Cmd_Line_1'Access,
         Tool_Temp_Dir             => Environment.Tool_Temp_Dir.all,
         Print_Help                => Print_Help'Access);
      LAL_UL.Common.Post.Postprocess_Common (Cmd);

      if Debug_Flag_C then
         Dump_Cmd (Cmd);
      end if;

      LAL_UL.Check_Parameters; -- ????Move into Init?

      --  ????????????????Stuff from Environment:

      declare
         use GNAT.OS_Lib, Environment;
      begin
         Copy_Gnat_Adc;
         pragma Assert
           (Get_Current_Dir = Tool_Current_Dir.all & Directory_Separator);

--         if not Incremental_Mode then
--            Change_Dir (Tool_Temp_Dir.all);
--            ASIS_UL.Compiler_Options.Store_I_Options;
--         end if;

         --  Create output directory if necessary

--         if Out_Dir /= null then
--            Parallel_Make_Dir (Out_Dir.all, Give_Message => Verbose_Mode);
--         end if;
      end;

      --  In Incremental_Mode, we invoke the builder instead of doing the
      --  normal tool processing. The inner invocations of this tool invoked by
      --  the builder will do the normal tool processing.

--      if ASIS_UL.Options.Incremental_Mode then
--         Environment.Call_Builder;
--      else
--         ASIS_UL.Source_Table.Processing.Process_Sources;
--      end if;

      Init (Tool, Cmd);
      Process_Files;
      Final (Tool, Cmd);

      Environment.Clean_Up;

--      if not ASIS_UL.Options.Incremental_Mode then
--         if not ASIS_UL.Source_Table.Processing
--             .All_Files_Successfully_Processed
--         then
--            GNAT.OS_Lib.OS_Exit (1);
--         end if;
--      end if;

      ASIS_UL.Main_Done := True;

   exception
      when X : LAL_UL.Command_Lines.Command_Line_Error =>
         Put (Standard_Error, LAL_UL.Tool_Names.Tool_Name & ": ");
         Put_Line (Standard_Error, Ada.Exceptions.Exception_Message (X));
         GNAT.Command_Line.Try_Help;
         Environment.Clean_Up;
         GNAT.OS_Lib.OS_Exit (1);
   end Driver;

end LAL_UL.Drivers;
