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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package body Tools is

   -------------
   -- Convert --
   -------------

   function Convert (Arg : String) return Tool is
   begin
      return Tool'Value (Arg);
   exception
      when others =>
         raise Parse_Tool_Exception;
   end Convert;

   ---------------
   -- Tool_List --
   ---------------

   function Tool_List return String is
      use Ada.Characters.Handling;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
      H : Unbounded_String;
   begin
      if Tool'Range_Length = 1 then
         Append (H, To_Lower (Tool'Image (Tool'First)));
      else
         Append (H, To_Lower (Tool'Image (Tool'First)));
         for J in Tool'Succ (Tool'First) .. Tool'Last loop
            Append (H, (LF & "         " & To_Lower (Tool'Image (J))));
         end loop;
      end if;
      return To_String (H);
   end Tool_List;

   ---------------------------
   -- Find_First_Tool_Index --
   ---------------------------

   function Find_First_Tool_Index return Natural
   is
      package String_Hashed_Sets is new Ada.Containers.Indefinite_Hashed_Sets
        (Element_Type        => String,
         Hash                => Ada.Strings.Hash,
         Equivalent_Elements => "=");

      Tools_Set : String_Hashed_Sets.Set;

      use Ada.Characters.Handling;
      use Ada.Command_Line;

   begin
      for J in Tool'First .. Tool'Last loop
         Tools_Set.Insert (To_Lower (Tool'Image (J)));
      end loop;

      for J in 1 .. Argument_Count loop
         if Tools_Set.Contains (To_Lower (Argument (J))) then
            return J;
         end if;
      end loop;

      return 0;
   end Find_First_Tool_Index;

end Tools;
