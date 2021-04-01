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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;

with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Utils.Tool_Names is

   Exe_Suffix : constant String_Access := Get_Executable_Suffix;
   Tool_Name  : constant String        :=
     To_Lower
       (GNAT.Directory_Operations.Base_Name
          (Ada.Command_Line.Command_Name,
           Suffix => Exe_Suffix.all));

   function Target return String;
   --  If this is a cross version of the tool, Tool_Name will be of the form
   --  target-tool, and this returns "target". If the tool name starts with
   --  "gnaamp", returns "AAMP". Otherwise, this returns "".

   function Basic_Tool_Name return String;
   --  Returns the tool name without the target & "-", if any

   function Full_Tool_Name return String;
   --  Returns the full tool name

end Utils.Tool_Names;
