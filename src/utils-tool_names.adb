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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils.Tool_Names is

   ------------
   -- Target --
   ------------

   function Target return String is
      Tgt_Last : constant Natural := Index (Tool_Name, "-", Backward);
      AAMP_Idx : constant Natural := Index (Tool_Name, "gnaamp");
   begin
      if AAMP_Idx = Tool_Name'First then
         return "AAMP";
      elsif Tgt_Last > 0 then
         return Tool_Name (Tool_Name'First .. Tgt_Last - 1);
      else
         return "";
      end if;
   end Target;

   ---------------------
   -- Basic_Tool_Name --
   ---------------------

   function Basic_Tool_Name return String is
      Tgt_Last : constant Natural := Index (Tool_Name, "-", Backward);
   begin
      if Tgt_Last > 0 then
         return Tool_Name (Tgt_Last + 1 .. Tool_Name'Last);
      else
         return Tool_Name;
      end if;
   end Basic_Tool_Name;

   --------------------
   -- Full_Tool_Name --
   --------------------

   function Full_Tool_Name return String
   is
      Tool_Access : String_Access :=
        Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
      Res : constant String := Tool_Access.all;
   begin
      Free (Tool_Access);
      return Res;
   end Full_Tool_Name;

end Utils.Tool_Names;
