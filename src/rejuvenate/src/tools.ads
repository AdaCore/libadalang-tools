------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  Common utilities to all rejuvenate tools

package Tools is

   type Tool is
     (Record_Components,
      Array_Aggregates);

   function Convert (Arg : String) return Tool;
   --  Returns Tool'Value of Arg. Raises Parse_Tool_Exception is Arg is
   --  not a Tool.

   function Tool_List return String;
   --  Returns all literals of Tool as a lower case string, concatenated with
   --  LF.

   function Find_First_Tool_Index return Natural;
   --  Find the index of the first Tool in the arguments passed to the
   --  command line.

   Parse_Tool_Exception : exception;

end Tools;
