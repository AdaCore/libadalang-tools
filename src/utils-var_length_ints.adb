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

package body Utils.Var_Length_Ints is

   --  Each integer is encoded as one or more Octets, in base 2**7. The
   --  low-order bit of each Octet indicates whether it is the first one in the
   --  sequence -- 0 for the first, 1 for the rest. The other 7 bits are the
   --  "value" of that Octet. The Octets are stored in little-endian order,
   --  because that simplifies the Encode procedure, because the Octets are
   --  naturally generated starting with the low 7 bits. Otherwise, it would
   --  have to store up the Octets somewhere, and reverse the order when
   --  appending.

   Radix : constant := 2**7;

   function Is_First (X : Octet) return Boolean is (X mod 2 = 0);
   --  True if X is the first one in a sequence. Distinguishing
   --  first-versus-other allows us to move both forward and backward through a
   --  sequence of encoded integers.

   package body Encodings is

      subtype Base is Int'Base;
      --  We need to store various intermediate results in the base subtype in
      --  case Int is more constrained.

      function Val (X : Octet) return Base is (Base (X / 2));
      --  The value represented by X, as a "digit" in a base-2**7 number.
      --  Shift right one bit to get the value.

      function To_Octet (X : Base; First_Octet : Boolean) return Octet is
         (if First_Octet then Octet (X * 2) else Octet ((X * 2) + 1)) with
           Pre => X < Radix;
      --  Turn X into the relevant Octet by shifting left one bit, then setting
      --  the low bit according to First_Octet.

      procedure Encode (V : in out Octet_Vector; X : Int) is
         Temp : Base := X;
         First_Octet : Boolean := True;
      begin
         --  Note that a 0 Int is represented as 1 Octet equal to 0,
         --  not as 0 Octets. Hence the exit in the middle.

         loop
            Append (V, To_Octet (Temp mod Radix, First_Octet));
            exit when Temp = 0;
            First_Octet := False;
            Temp := Temp / Radix;
         end loop;
      end Encode;

      function Decode (V : Octet_Vector; Index : Octet_Index) return Int is
         A : Octet_Array renames Elems (V) (1 .. Last_Index (V));
      begin
         return Decode (A, Index);
      end Decode;

      function Decode (A : Octet_Array; Index : Octet_Index) return Int is
         pragma Assert (Is_First (A (Index)));
         Temp_Index : Octet_Index := Index;
         Result : Base := 0;
      begin
         --  It's little endian, so we need to loop through the Octets
         --  backwards:

         Next (A, Temp_Index);
         loop
            Temp_Index := Temp_Index - 1;
            Result := (Result * Radix) + Val (A (Temp_Index));
            exit when Is_First (A (Temp_Index));
         end loop;
         return Result;
      end Decode;

   end Encodings;

   function Next (V : Octet_Vector; Index : Octet_Index) return Octet_Index is
      A : Octet_Array renames Elems (V) (1 .. Last_Index (V));
   begin
      return Next (A, Index);
   end Next;

   function Next (A : Octet_Array; Index : Octet_Index) return Octet_Index is
   begin
      return Result : Octet_Index := Index do
         Next (A, Result);
      end return;
   end Next;

   procedure Next (V : Octet_Vector; Index : in out Octet_Index) is
      A : Octet_Array renames Elems (V) (1 .. Last_Index (V));
   begin
      Next (A, Index);
   end Next;

   procedure Next (A : Octet_Array; Index : in out Octet_Index) is
   begin
      pragma Assert (Is_First (A (Index)));
      loop
         Index := Index + 1;
         exit when Index = A'Last + 1 or else Is_First (A (Index));
      end loop;
   end Next;

   function Prev (V : Octet_Vector; Index : Octet_Index) return Octet_Index is
      A : Octet_Array renames Elems (V) (1 .. Last_Index (V));
   begin
      return Prev (A, Index);
   end Prev;

   function Prev (A : Octet_Array; Index : Octet_Index) return Octet_Index is
   begin
      return Result : Octet_Index := Index do
         Prev (A, Result);
      end return;
   end Prev;

   procedure Prev (V : Octet_Vector; Index : in out Octet_Index) is
      A : Octet_Array renames Elems (V) (1 .. Last_Index (V));
   begin
      Prev (A, Index);
   end Prev;

   procedure Prev (A : Octet_Array; Index : in out Octet_Index) is
   begin
      pragma Assert (Index = A'Last + 1 or else Is_First (A (Index)));
      loop
         Index := Index - 1;
         exit when Is_First (A (Index));
      end loop;
   end Prev;

end Utils.Var_Length_Ints;
