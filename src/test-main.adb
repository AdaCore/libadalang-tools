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

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with Test.Actions;
with Test.Command_Lines;
with Test.Common;

procedure Test.Main is

   --  Main procedure for gnattest

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      pragma Unreferenced (Swit); -- ????
   begin
      if Phase = Cmd_Line_1 then
         null; -- ????

         return;
      end if;
   end Callback;

   Tool : Actions.Test_Tool;
   Cmd : Command_Line (Test.Command_Lines.Descriptor'Access);

begin
   Test.Actions.Register_Specific_Attributes;

   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => Test.Common.GT_Package,
      Callback              => Callback'Unrestricted_Access);
end Test.Main;
