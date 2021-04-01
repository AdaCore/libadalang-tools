------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2001-2021, AdaCore                    --
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

--  This package contains routines for dealing with the casing exception
--  dictionaries

with Pp.Command_Lines; use Pp.Command_Lines;
with Utils.Command_Lines; use Utils.Command_Lines;
package Pp.Formatting.Dictionaries is

   procedure Scan_Dictionaries (Dictionary_File_Names : String_Ref_Array);
   --  Scans the dictionary files given by command-line switches and stores all
   --  the casing exceptions in the exception tables.

   procedure Check_With_Dictionary
     (Ada_Name : in out Wide_String;
      Casing   : PP_Casing);
   --  Checks if Ada_Name as a whole or some its subname (that is, a part of
   --  the Ada_Name surrounded by '_' is in the exception dictionary, and if it
   --  is, changes the casing of Ada_Name or of its part to what is defined in
   --  the dictionary. For the names or name parts that are not in the
   --  dictionary, changes their casing according to the value of Casing
   --  parameter

end Pp.Formatting.Dictionaries;
