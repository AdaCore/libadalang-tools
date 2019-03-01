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

with Text_IO;

package body Pp.Scanner.Lines is

   function Line_Break_Token_Index (X : Tokn_Cursor) return Line_Break_Index is
     (Line_Break_Index (Index (X)));

   function Tab_Token_Index (X : Tokn_Cursor) return Tab_Index is
     (Tab_Index (Index (X)));

   procedure Append_Line_Break_Tokn
     (V   : in out Tokn_Vec; Enabled : Boolean; Index : Line_Break_Index;
      Org :        String := "Append_Line_Break_Tokn")
   is
   begin
      if Enabled then
         Append_Tokn_With_Index (V, Enabled_LB_Token, Positive (Index), Org);
      else
         Append_Tokn_With_Index (V, Disabled_LB_Token, Positive (Index), Org);
      end if;
   end Append_Line_Break_Tokn;

   procedure Append_Tab_Tokn
     (V   : in out Tokn_Vec; Index : Tab_Index;
      Org :        String := "Append_Tab_Tokn")
   is
   begin
      Append_Tokn_With_Index (V, Tab_Token, Positive (Index), Org);
   end Append_Tab_Tokn;

   procedure Put_Index_Info (All_LB : Line_Break_Vector; X : Tokn_Cursor) is
   begin
      case Kind (X) is
         when Line_Break_Token =>
            declare
               Break : Line_Break renames All_LB (Line_Break_Token_Index (X));
            begin
               Text_IO.Put
                 (Text_IO.Standard_Output,
                  "ind = " & Image (Break.Indentation) & ", " &
                  (if Break.Hard then "hard" else "soft") & ", " &
                  (if Break.Enabled then "enabled" else "disabled") & ", " &
                  "lev = " & Image (Integer (Break.Level)));
            end;
         when Tab_Token =>
            null; -- not yet implemented
         when others =>
            raise Program_Error;
      end case;
   end Put_Index_Info;

end Pp.Scanner.Lines;
