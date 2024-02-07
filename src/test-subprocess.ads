------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2014-2024, AdaCore                    --
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

--  This package provides utility types and subprograms to manipulate
--  GNATCOLL.OS.Subprocess commands.

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

with Utils.Command_Lines; use Utils.Command_Lines;

package Test.Subprocess is

   procedure PP_Cmd (Cmd : Argument_List; Prefix : String := "");
   --  Pretty-print Cmd on standard error

   procedure Run
     (Cmd         : Argument_List;
      What        : String := "";
      Out_To_Null : Boolean := False);
   --  Run the command in Cmd. If the exit status is not zero, print an
   --  error message, cleanup and exit.
   --
   --  If What is not the empty string, it is used as the command name in
   --  the error message.
   --
   --  If Out_To_Null is True, the process standard output is redirected to
   --  null. Otherwise it is forwarded. The standard error of the subprocess
   --  is forwarded to Stdout or null based on Out_To_Null.

   procedure Populate_X_Vars
     (Cmd : in out Argument_List; Gnattest_Cmd : Command_Line);
   --  Copy all the -X arguments from Gnattest_Cmd to Cmd

end Test.Subprocess;
