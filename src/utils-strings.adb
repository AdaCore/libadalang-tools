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

with Ada.Strings.Unbounded;

package body Utils.Strings is

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            Slice : String renames X (X'First .. X'First + Prefix'Length - 1);
         begin
            return Slice = Prefix;
         end;
      end if;
      return False;
   end Has_Prefix;

   function Has_Suffix (X, Suffix : String) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         declare
            Slice : constant String :=
              X (X'Last - Suffix'Length + 1 .. X'Last);
         begin
            return Slice = Suffix;
         end;
      end if;
      return False;
   end Has_Suffix;

   function Replace_String (S, From, To : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      J : Positive := S'First;
   begin
      while J <= S'Last loop
         if J + From'Length - 1 <= S'Last
           and then S (J .. J + From'Length - 1) = From
         then
            Append (Result, To);
            J := J + From'Length;
         else
            Append (Result, S (J));
            J := J + 1;
         end if;
      end loop;

      return To_String (Result);
   end Replace_String;

end Utils.Strings;
