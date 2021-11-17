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

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Drivers;

with Stub.Actions;
with Stub.Command_Lines;

procedure Stub.Main is

   --  Main procedure for lalstub

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
     null;

   Tool : Actions.Stub_Tool;
   Cmd : Command_Line (Stub.Command_Lines.Descriptor'Access);

begin
   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => "gnatstub",
      Callback              => Callback'Unrestricted_Access);

   --  If syntax errors are detected during the processing then return a
   --  non zero exit code
   if Utils.Syntax_Errors then
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Stub.Main;
