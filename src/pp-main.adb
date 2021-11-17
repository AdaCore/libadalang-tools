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

with Pp.Actions;
with Pp.Command_Lines;

procedure Pp.Main is

   --  Main procedure for lalpp

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
     null;

   Tool : Actions.Pp_Tool;
   Cmd : Command_Line (Pp.Command_Lines.Descriptor'Access);

begin
   Utils.Drivers.Driver
     (Cmd,
      Tool,
      Tool_Package_Name     => "pretty_printer",
      Callback              => Callback'Unrestricted_Access);
   --  Should we pass Preprocessing_Allowed => False???

   --  If syntax errors are detected during the processing then return a
   --  non zero exit code
   if Utils.Syntax_Errors then
      GNAT.OS_Lib.OS_Exit (1);
   end if;

end Pp.Main;
