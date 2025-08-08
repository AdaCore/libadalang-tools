------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2023, AdaCore                      --
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

with Ada.Text_IO;
with GNAT.OS_Lib;

with Utils_Debug; use Utils_Debug;

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;
with Utils.Err_Out;

with METRICS.Actions;
with METRICS.Command_Lines;

procedure METRICS.Main is

   --  Main procedure for lalmetric

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   function Get_Fallback_Target return String;
   --  Returns `gnatsas` if codepeer-gprconfig has only one `codepeer` target

   --------------
   -- Callback --
   --------------

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      use METRICS.Command_Lines;
      use Metrics_String_Seq_Switches;
   begin
      if Phase = Cmd_Line_1 then

         if Metrics_String_Seq_Switches.Valid (Swit.Switch) then
            declare
               Switch : constant Metrics_String_Seqs := From_All (Swit.Switch);
            begin
               case Switch is
                  when Gnatmetric_Debug =>
                     for Dbg of Swit.Seq_Val loop
                        Set_Debug_Options (Dbg.all);
                     end loop;
               end case;
            end;
         end if;

         return;
      end if;
   end Callback;

   -------------------------
   -- Get_Fallback_Target --
   -------------------------

   function Get_Fallback_Target return String is
      use GNAT.OS_Lib;
      use Ada.Text_IO;

      Exec_Access : String_Access :=
        Locate_Exec_On_Path ("codepeer-gprconfig");

      Apply : Boolean := False;
      Lines : Natural := 0;
      FD    : File_Descriptor;
      Name  : String_Access;
      Code  : Integer;
      File  : Ada.Text_IO.File_Type;
      Dummy : Boolean;

   begin
      if Exec_Access /= null then
         Create_Temp_Output_File (FD, Name);

         if FD /= Invalid_FD then
            declare
               Arg1 : aliased String := "--show-targets";
               Arg2 : aliased String := "--config=Ada";
               Args : constant Argument_List :=
                 (Arg1'Unchecked_Access, Arg2'Unchecked_Access);
            begin
               Spawn (Exec_Access.all, Args, FD, Code);
               Close (FD);

               if Code = 0 then
                  Open (File, In_File, Name.all);
                  while not End_Of_File (File) loop
                     declare
                        S : constant String := Get_Line (File);
                     begin
                        Lines := Lines + 1;

                        if Lines = 2 then
                           Apply := S = "codepeer";

                        elsif Lines = 3 then
                           Apply := False;
                           exit;
                        end if;
                     end;
                  end loop;
                  Close (File);
               end if;
            end;

            Delete_File (Name.all, Dummy);
            Free (Name);
         end if;

         Free (Exec_Access);
      end if;

      if Apply then
         return "gnatsas";
      else
         return "";
      end if;
   end Get_Fallback_Target;

   Tool : Actions.Metrics_Tool;
   Cmd  : Command_Line (METRICS.Command_Lines.Descriptor'Access);

begin
   --  By default, send errors to stdout
   Utils.Err_Out.Output_Enabled := True;

   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name => "metrics",
      Callback          => Callback'Unrestricted_Access,
      Fallback_Target   => Get_Fallback_Target);

   --  If syntax errors are detected during the processing then return a
   --  non zero exit code
   if Utils.Syntax_Errors then
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end METRICS.Main;
