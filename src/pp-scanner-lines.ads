------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2012-2021, AdaCore                    --
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

with Pp.Formatting; use Pp.Formatting;

package Pp.Scanner.Lines is

   --  This package is for portions of Pp.Scanner that need to depend on
   --  Pp.Formatting; Pp.Scanner can't depend on it directly, because that
   --  would cause cyclic with clauses.

   function Line_Break_Token_Index (X : Tokn_Cursor) return Line_Break_Index
     with Pre => Kind (X) in Line_Break_Token;

   function Tab_Token_Index (X : Tokn_Cursor) return Tab_Index
     with Pre => Kind (X) in Tab_Token;

   procedure Append_Line_Break_Tokn
     (V : in out Tokn_Vec; Enabled : Boolean; Index : Line_Break_Index;
      Org : String := "Append_Line_Break_Tokn");

   procedure Append_Tab_Tokn
     (V : in out Tokn_Vec; Index : Tab_Index;
      Org : String := "Append_Tab_Tokn");

   procedure Put_Index_Info (All_LB : Line_Break_Vector; X : Tokn_Cursor)
     with Pre => Kind (X) in Line_Break_Token | Tab_Token;

end Pp.Scanner.Lines;
