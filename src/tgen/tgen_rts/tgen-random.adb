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

with Ada.Calendar; use Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;

with TGen.Logging;

package body TGen.Random is

   Random_Trace : constant TGen.Logging.GNATCOLL_Trace :=
     TGen.Logging.Create_Trace ("RANDOM");

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

   generic
      type Float_Type is digits <>;
      --  Floating point type of a specific precision

      type Unsigned_Type is mod <>;
      --  Unsigned type to transparently manipulate the binary representation,
      --  which should be of the same size as the floating point type

      type Int_Type is range <>;
      --  Type used for intermediate (possibly negative) computations over
      --  the exponent / the mantissa. It should be able to hold at least a
      --  mantissa binary representation.

      Exponent_Size : Natural;
      --  Size of the exponent in bits

      Mantissa_Size : Natural;
      --  Size of the mantissa in bits

      --  Conversions functions, to convert a value of the Float_Type to the
      --  Unsigned_Type and reciprocally.

      with function Float_To_Unsigned
        (F : Float_Type) return Unsigned_Type;
      with function Unsigned_To_Float
        (U : Unsigned_Type) return Float_Type;

      with function Random
        (Gen : GNAT.Random_Numbers.Generator;
         Min, Max : Unsigned_Type) return Unsigned_Type;

   function Random_Float (LB, HB : Float_Type) return Float_Type;

   ------------------
   -- Random_Float --
   ------------------

   function Random_Float (LB, HB : Float_Type) return Float_Type
   is
      MS_Part : constant Unsigned_Type := 2**Mantissa_Size;
      XP_Part : constant Unsigned_Type := 2**Exponent_Size;

      LB_Unsigned : constant Unsigned_Type := Float_To_Unsigned (LB);
      HB_Unsigned : constant Unsigned_Type := Float_To_Unsigned (HB);

      --  We get the biased exponent and do not use the Exponent attribute. We
      --  are only interested in the type representation, so the Exponent value
      --  is not of much interest.

      --  Get the lower bound characteristics

      LB_Exp      : constant Unsigned_Type :=
        (LB_Unsigned / MS_Part) mod XP_Part;
      LB_Mantissa : constant Unsigned_Type :=
        LB_Unsigned mod MS_Part;
      LB_Sign     : constant Int_Type :=
        (if LB < 0.0 then -1 else 1);

      --  Get the higher bound characteristics

      HB_Exp      : constant Unsigned_Type :=
        (HB_Unsigned / MS_Part) mod XP_Part;
      HB_Mantissa : constant Unsigned_Type :=
        HB_Unsigned mod MS_Part;
      HB_Sign     : constant Int_Type := (if HB < 0.0 then -1 else 1);

      Nb_Values_In_First_Binade : Unsigned_Type := 0;
      Nb_Values_In_Last_Binade  : Unsigned_Type := 0;
      Nb_Values_Between         : Unsigned_Type := 0;
      Nb_Values                 : Unsigned_Type := 0;
      --  Number of floating point values in the type range.

   begin
      --  Check the number of floating point that can be represented in the
      --  given range

      --  If 'First and 'Last lie in the same binade, retrieve the list of
      --  mantissa values between 'First and 'Last in this binade: this will
      --  give us the number of values this floating point can have.
      --
      --  Otherwise, proceed more carefully (see the else case below).

      if LB_Sign = HB_Sign and then LB_Exp = HB_Exp then
         Nb_Values :=
           Unsigned_Type (HB_Sign * Int_Type (HB_Mantissa)
                          - LB_Sign * (Int_Type (LB_Mantissa)))
           + 1;
      else
         --  Otherwise, enumerate the number of values in 'First's binade, the
         --  number of values between 'First and 'Last binades (excluded), and
         --  the number of values in 'Last's binade.

         Nb_Values_In_First_Binade :=

         --  Everything between the mantissa and a higher / lower
         --  exponent depending on the sign

           Unsigned_Type (Int_Type (MS_Part) - LB_Sign
                          * Int_Type (LB_Mantissa)) mod MS_Part

         --  Don't forget the LB_Mantissa value itself

           + 1;

         --  Grab the floating points in the same binade as HB. Same as
         --  for LB, but symetrically.

         Nb_Values_In_Last_Binade :=
           Unsigned_Type (Int_Type (MS_Part) + HB_Sign
                          * Int_Type (HB_Mantissa)) mod MS_Part + 1;

         --  Grab the values in between

         Nb_Values_Between :=
           Unsigned_Type
             (HB_Sign * Int_Type (HB_Exp) - LB_Sign * Int_Type (LB_Exp))
               * MS_Part;

         Nb_Values :=
           Nb_Values_In_First_Binade + Nb_Values_In_Last_Binade
             + Nb_Values_Between;
      end if;

      --  Now that we have the number of values that can be
      --  represented, pick an integer in [0, Nb_Values[.

      declare
         Rand : Unsigned_Type :=
           Random (Generator_Instance, 0, Nb_Values - 1);

         --  Save the result characteristics: the sign, the exponent and the
         --  mantissa.

         Result          : Unsigned_Type;
         Result_Sign     : Int_Type;
         Result_Exp      : Unsigned_Type;
         Result_Mantissa : Unsigned_Type;
      begin
         --  Find the float to which it corresponds

         --  Case where the picked number lies in 'First's binade

         if Rand < Nb_Values_In_First_Binade then
            Result_Sign := LB_Sign;
            Result_Exp := LB_Exp;
            Result_Mantissa :=
              Unsigned_Type
                (Int_Type (LB_Mantissa) + LB_Sign * Int_Type (Rand));

         else
            Rand := Rand - Nb_Values_In_First_Binade;

            --  Case where it lies in between 'First's and 'Last's
            --  binade

            if Rand < Nb_Values_Between then
               declare
                  Intermediate_Exp : constant Int_Type :=
                    Int_Type (Rand / MS_Part)
                    + (LB_Sign * Int_Type (LB_Exp)) + 1;
               begin
                  Result_Sign :=
                    (if Intermediate_Exp < 0 then -1 else 1);
                  Result_Exp :=
                    Unsigned_Type (abs (Intermediate_Exp));
                  Result_Mantissa :=
                    Unsigned_Type
                      (Int_Type (MS_Part) + Result_Sign
                       * Int_Type (Rand mod MS_Part))
                  mod MS_Part;
               end;

               --  Case where it lies in 'Last's binade

            else
               Rand := Rand - Nb_Values_Between;

               Result_Sign := HB_Sign;
               Result_Exp := HB_Exp;
               Result_Mantissa :=
                 Unsigned_Type
                   (Int_Type (HB_Mantissa)
                    - HB_Sign * Int_Type (Rand));
            end if;
         end if;

         --  Build the result from its components

         Result := Result_Exp * MS_Part + Result_Mantissa;
         if Result_Sign = -1 then

            --  Set the sign bit

            Result := XP_Part * MS_Part + Result;
         end if;

         --  Floating point result is an unchecked conversion of the
         --  Unsigned_Type Result.

         return Unsigned_To_Float (Result);

      end;
   end Random_Float;

   function Float_To_Unsigned_32 is
     new Ada.Unchecked_Conversion (Source => Float, Target => Unsigned_32);
   function Unsigned_32_To_Float is
     new Ada.Unchecked_Conversion (Source => Unsigned_32, Target => Float);
   function Random_Unsigned_32 is
     new GNAT.Random_Numbers.Random_Discrete (Unsigned_32);

   function Random_F is new Random_Float
     (Float_Type        => Float,
      Unsigned_Type     => Unsigned_32,
      Int_Type          => Integer,
      Exponent_Size     => 8,
      Mantissa_Size     => 23,
      Float_To_Unsigned => Float_To_Unsigned_32,
      Unsigned_To_Float => Unsigned_32_To_Float,
      Random            => Random_Unsigned_32);

   function Long_Float_To_Unsigned_64 is
     new Ada.Unchecked_Conversion
       (Source => Long_Float, Target => Unsigned_64);
   function Unsigned_64_To_Long_Float is
     new Ada.Unchecked_Conversion
       (Source => Unsigned_64, Target => Long_Float);
   function Random_Unsigned_64 is
     new GNAT.Random_Numbers.Random_Discrete (Unsigned_64);

   function Random_LF is new Random_Float
     (Float_Type => Long_Float,
      Unsigned_Type     => Unsigned_64,
      Int_Type          => Long_Long_Integer,
      Exponent_Size     => 11,
      Mantissa_Size     => 52,
      Float_To_Unsigned => Long_Float_To_Unsigned_64,
      Unsigned_To_Float => Unsigned_64_To_Long_Float,
      Random            => Random_Unsigned_64);

   function Long_Long_Float_To_Unsigned_128 is
     new Ada.Unchecked_Conversion
       (Source => Long_Long_Float, Target => Unsigned_128);
   function Unsigned_128_To_Long_Long_Float is
     new Ada.Unchecked_Conversion
       (Source => Unsigned_128, Target => Long_Long_Float);
   function Random_Unsigned_128 is
     new GNAT.Random_Numbers.Random_Discrete (Unsigned_128);

   function Random_LLF is new Random_Float
     (Float_Type        => Long_Long_Float,
      Unsigned_Type     => Unsigned_128,
      Int_Type          => Long_Long_Long_Integer,
      Exponent_Size     => 15,
      Mantissa_Size     => 63,
      Float_To_Unsigned => Long_Long_Float_To_Unsigned_128,
      Unsigned_To_Float => Unsigned_128_To_Long_Long_Float,
      Random            => Random_Unsigned_128);

   ------------
   -- Random --
   ------------

   function Random (Min, Max : Any_Float) return Any_Float is
   begin
      case Min.Precision is
         when Single =>
            return Create (Random_F (Value (Min), Value (Max)));
         when Double =>
            return Create (Random_LF (Value (Min), Value (Max)));
         when Extended =>
            return Create (Random_LLF (Value (Min), Value (Max)));
      end case;
   end Random;

   Seed_Env_Var : constant String := "TGEN_RANDOM_SEED";
   Seed         : Unsigned_32;
   Y2K          : constant Time :=
     Time_Of (Year => 2000, Month => 1, Day => 1, Seconds => 0.0);
   --  First day of Year 2000, to get a duration

   function To_U64 is
     new Ada.Unchecked_Conversion (Duration, Interfaces.Unsigned_64);

begin
   if Ada.Environment_Variables.Exists (Seed_Env_Var) then
      Seed :=
        Unsigned_32'Value (Ada.Environment_Variables.Value (Seed_Env_Var));
   else
      Seed := Unsigned_32'Mod (To_U64 (Clock - Y2K));
   end if;
   Random_Trace.Trace ("Random generator seed is " & Unsigned_32'Image (Seed));
   GNAT.Random_Numbers.Reset (Generator_Instance, Seed);
end TGen.Random;
