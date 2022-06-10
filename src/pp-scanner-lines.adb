------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2012-2022, AdaCore                    --
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

with Ada.Text_IO;

package body Pp.Scanner.Lines is

   function Line_Break_Token_Index (X : Tokn_Cursor) return Line_Break_Index is
     (Line_Break_Index (Index (X)));

   function Tab_Token_Index (X : Tokn_Cursor) return Tab_Index is
     (Tab_Index (Index (X)));

   procedure Append_Line_Break_Tokn
     (V : in out Tokn_Vec; Enabled : Boolean; Index : Line_Break_Index;
      Org : String := "Append_Line_Break_Tokn") is
   begin
      if Enabled then
         Append_Tokn_With_Index  (V, Enabled_LB_Token, Positive (Index), Org);
      else
         Append_Tokn_With_Index  (V, Disabled_LB_Token, Positive (Index), Org);
      end if;
   end Append_Line_Break_Tokn;

   procedure Append_Tab_Tokn
     (V : in out Tokn_Vec; Index : Tab_Index;
      Org : String := "Append_Tab_Tokn") is
   begin
      Append_Tokn_With_Index  (V, Tab_Token, Positive (Index), Org);
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
                  "ind = " & Image (Break.Indentation) &
                  ", " & (if Break.Hard then "hard" else "soft") &
                  ", " & (if Break.Enabled then "enabled" else "disabled") &
                  ", " & "lev = " & Image (Integer (Break.Level)));
            end;
         when Tab_Token =>
            null; -- not yet implemented
         when others => raise Program_Error;
      end case;
   end Put_Index_Info;

end Pp.Scanner.Lines;
