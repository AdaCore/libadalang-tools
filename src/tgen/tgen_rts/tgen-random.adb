------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Numerics.Generic_Elementary_Functions;

package body TGen.Random is

   ---------------
   -- Draw_Bits --
   ---------------

   function Draw_Bits (N : Positive) return Unsigned_128 is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (Unsigned_128);
   begin
      return Rand (Generator_Instance, Min => 0, Max => 2 ** N);
   end Draw_Bits;

   function Draw_Bits (N : Positive) return Unsigned_64 is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (Unsigned_64);
   begin
      return Rand (Generator_Instance, Min => 0, Max => 2 ** N);
   end Draw_Bits;

   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Float);
   use Value_Functions;

   -----------------
   -- Biased_Coin --
   -----------------

   function Biased_Coin (P_True : Float) return Boolean is
      Bits : Natural;
      Size : Unsigned_64;
      P    : Float := P_True;
   begin
      if P <= 0.0 then
         return False;
      elsif P >= 1.0 then
         return True;
      else
         --  Meaningful draw

         Bits :=
           Positive (Float'Ceiling
                     (-Log (X => Float'Min (P, 1.0 - P), Base => 2.0)));

         if Bits >= 64 then
            --  Let's avoid large draws and treat that as effectively zero.
            --  We draw only 64 bits anyway so we won't have enough precision
            --  to pull 1's.

            return False;
         end if;

         Size := 2 ** Bits;

         while True loop
            declare
               Falsey : constant Unsigned_64 :=
                 Unsigned_64 (Float'Floor (Float (Size) * (1.0 - P)));
               --  Number of Falsey parts

               Truthy : constant Unsigned_64 :=
                 Unsigned_64 (Float'Floor (Float (Size) * P));
               --  Number of Truthy parts.

               Remainder : constant Float :=
                 ((Float (Size) * P) - Float (Truthy));

               Partial : Boolean;
               --  Whether Falsey + Truthy makes the whole size. If this is
               --  not the case, we will have to dump one value and to draw
               --  again if this value if picked, using the Remainder as our
               --  new probability P to pick True.

               Draw : constant Unsigned_64 := Draw_Bits (Bits);
            begin
               if Falsey + Truthy = Size then
                  Partial := False;
               else
                  Partial := True;
               end if;

               if Partial and then Draw = (Size - 1) then
                  P := Remainder;
                  goto Continue;
               end if;

               return Draw >= Falsey;
            end;
            <<Continue>>
         end loop;
      end if;
      raise Program_Error with "unreachable code";
   end Biased_Coin;

   ----------
   -- Many --
   ----------

   function Many
     (Min_Size, Max_Size, Average_Size : Natural) return Many_Type
   is
   begin
      return Many_Type'
        (Min_Size, Max_Size,
         Count          => 0,
         Stopping_Value => 1.0 - 1.0 / Float (1 + Average_Size));
   end Many;

   ----------
   -- More --
   ----------

   function More (Self : in out Many_Type) return Boolean
   is
      Should_Continue : Boolean;
   begin
      if Self.Min_Size = Self.Max_Size then
         Self.Count := Self.Count + 1;
         return Self.Count < Self.Min_Size;
      end if;

      if Self.Count < Self.Min_Size then
         Self.Count := Self.Count + 1;
         return True;
      end if;

      if Self.Count >= Self.Max_Size then
         return False;
      end if;

      Should_Continue := Biased_Coin (Self.Stopping_Value);

      if Should_Continue then
         Self.Count := Self.Count + 1;
      end if;

      return Should_Continue;
   end More;

   function Rand_Float return Float is
      function Rand is new GNAT.Random_Numbers.Random_Float (Float);
   begin
      return Rand (Generator_Instance);
   end Rand_Float;

   function Rand_Int (Min, Max : Integer) return Integer is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (Integer);
   begin
      return Rand (Generator_Instance, Min, Max);
   end Rand_Int;

   function Rand_LLLI
     (Min, Max : Long_Long_Long_Integer) return Long_Long_Long_Integer is
      function Rand is
        new GNAT.Random_Numbers.Random_Discrete (Long_Long_Long_Integer);
   begin
      return Rand (Generator_Instance, Min, Max);
   end Rand_LLLI;
begin
   GNAT.Random_Numbers.Reset (Generator_Instance);
end TGen.Random;
