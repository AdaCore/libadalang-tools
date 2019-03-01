------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               V E C T O R S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2013-2017, AdaCore                    --
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

package body Utils.Slow_Vectors is
   pragma Warnings (Off);
   use all type Vector;
   --  ???Compiler warns "ineffective use clause...", which is incorrect.
   pragma Warnings (On);

   procedure Free (Container : in out Vector) is
   begin
      Vectors.Reserve_Capacity (Container, Capacity => 0);
   end Free;

   function Slice
     (V : Vector; First : Index_Type; Last : Vectors.Extended_Index)
      return Elements_Array
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
