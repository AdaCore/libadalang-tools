------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2013-2021, AdaCore                    --
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

package body Utils.Slow_Vectors is
   use all type Vector;

   procedure Free (Container : in out Vector) is
   begin
      Vectors.Reserve_Capacity (Container, Capacity => 0);
   end Free;

   function Slice
     (V     : Vector;
      First : Index_Type;
      Last  : Vectors.Extended_Index)
      return  Elements_Array
   is

      Jj : Vectors.Extended_Index          := Index_Type'First;
      L  : constant Vectors.Extended_Index :=
        (if Last < First then Jj - 1 else Last - First + Index_Type'First);
   --  Handle super-null slices properly

   begin
      return Result : Elements_Array (Index_Type'First .. L) do
         for J in First .. Last loop
            Result (Jj) := V (J);
            Jj          := Jj + 1;
         end loop;
         pragma Assert (Jj = Result'Last + 1);
      end return;
   end Slice;

   function To_Array (V : Vector) return Elements_Array is
   begin
      return Slice (V, First => 1, Last => Last_Index (V));
   end To_Array;

   procedure Append (V : in out Vector; A : Elements_Array) is
   begin
      for X of A loop
         Append (V, X);
      end loop;
   end Append;

end Utils.Slow_Vectors;
