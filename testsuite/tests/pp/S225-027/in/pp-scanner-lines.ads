------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . S C A N N E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Pp.Formatting; use Pp.Formatting;

package Pp.Scanner.Lines is

   --  This package is for portions of Pp.Scanner that need to depend on
   --  Pp.Formatting; Pp.Scanner can't depend on it directly, because that
   --  would cause cyclic with clauses.

   function Line_Break_Token_Index
     (X : Tokn_Cursor) return Line_Break_Index with
     Pre => Kind (X) in Line_Break_Token;

   function Tab_Token_Index (X : Tokn_Cursor) return Tab_Index with
     Pre => Kind (X) in Tab_Token;

   procedure Append_Line_Break_Tokn
     (V   : in out Tokn_Vec; Enabled : Boolean; Index : Line_Break_Index;
      Org :        String := "Append_Line_Break_Tokn");

   procedure Append_Tab_Tokn
     (V   : in out Tokn_Vec; Index : Tab_Index;
      Org :        String := "Append_Tab_Tokn");

   procedure Put_Index_Info (All_LB : Line_Break_Vector; X : Tokn_Cursor) with
     Pre => Kind (X) in Line_Break_Token | Tab_Token;

end Pp.Scanner.Lines;
