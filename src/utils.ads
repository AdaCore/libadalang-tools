------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2004-2022, AdaCore                    --
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

--  The top of libadalang-tools Utils hierarchy

pragma Warnings (Off); -- imported for children
with Utils_Debug; use Utils_Debug;
with Ada.Text_IO;
pragma Warnings (On);

package Utils is
   pragma Elaborate_Body;

   Debug_Mode : Boolean renames Debug_Flag_9;

   Assert_Enabled : Boolean := False;
   --  Set to True in body if assertions are enabled. This should really be a
   --  constant, but there's no easy mechanism for that.

   Main_Done : Boolean := False;
   --  This is set True at the (successful) end of each main procedure. The
   --  purpose is so assertions in Finalize operations can tell whether the
   --  main procedure exited normally. See, for example,
   --  Generic_Formatted_Output.Finalize, which insists that when we reach the
   --  end of the main procedure, the indentation level should be zero. But if
   --  an exception propagates out of the main procedure, that's just a bug
   --  which should be reported normally.

   Syntax_Errors : Boolean := False;
   --  This is set True if one of the analyzed source files has syntax errors.
   --  The purpose is to set a proper Exit Code at the end of the execution.

end Utils;
