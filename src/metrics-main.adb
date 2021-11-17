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
with GNAT.OS_Lib;

with Utils_Debug; use Utils_Debug;

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with METRICS.Actions;
with METRICS.Command_Lines;

procedure METRICS.Main is

   --  Main procedure for lalmetric

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

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

   Tool : Actions.Metrics_Tool;
   Cmd : Command_Line (METRICS.Command_Lines.Descriptor'Access);

begin
   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => "metrics",
      Callback              => Callback'Unrestricted_Access);

   --  If syntax errors are detected during the processing then return a
   --  non zero exit code
   if Utils.Syntax_Errors then
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end METRICS.Main;
