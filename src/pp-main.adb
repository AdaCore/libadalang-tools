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
--
--  Main procedure for GNATpp

with GNAT.OS_Lib;

with Utils.Command_Lines;
with Utils.Drivers;
with Utils.Err_Out;

with Pp.Actions;
with Pp.Command_Lines;

procedure Pp.Main is

   Tool : Actions.Pp_Tool;
   Cmd  :
     Utils.Command_Lines.Command_Line (Pp.Command_Lines.Descriptor'Access);

begin
   --  By default, send errors to stdout
   Utils.Err_Out.Output_Enabled := True;

   --  Deprecation message to advertise about GNATformat
   Utils.Err_Out.Put
     ("\n\1\n",
      "Info: AdaCore provides a new formatter GNATformat, currently in beta. "
      & "This will supersede GNATpp when leaving the beta program.");
   Utils.Err_Out.Put ("-----\n\n");

   --  Override trace settings by parsing the config file
   GNATCOLL.Traces.Parse_Config_File;

   Utils.Drivers.Driver
     (Cmd                   => Cmd,
      Tool                  => Tool,
      Tool_Package_Name     => "pretty_printer",
      Callback              => null,
      --  GNATpp handles preprocessing directives in its own way
      Preprocessing_Allowed => False);

   --  If syntax errors are detected during the processing then return a
   --  non zero exit code.
   if Utils.Syntax_Errors then
      GNAT.OS_Lib.OS_Exit (1);
   end if;

end Pp.Main;
