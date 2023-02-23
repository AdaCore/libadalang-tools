------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with System.Unsigned_Types; use System.Unsigned_Types;

with TGen.Big_Int; use TGen.Big_Int;

package body TGen.Big_Reals_Aux is

   package body Decimal_Fixed_Conversions is

      package Float_Aux is new Float_Conversions (Long_Float);

      subtype LLLI is Long_Long_Long_Integer;
      subtype LLLU is Long_Long_Long_Unsigned;

      Too_Large : constant Boolean :=
        Num'Small_Numerator > LLLU'Last
        or else Num'Small_Denominator > LLLU'Last;
      --  True if the Small is too large for Long_Long_Long_Unsigned, in which
      --  case we convert to/from Long_Float as an intermediate step.

      package Conv_I is new Big_Int.Signed_Conversions (LLLI);
      package Conv_U is new Big_Int.Unsigned_Conversions (LLLU);

      -----------------
      -- To_Big_Real --
      -----------------

      --  We just compute V * N / D where V is the mantissa value of the fixed
      --  point number, and N resp. D is the numerator resp. the denominator of
      --  the Small of the fixed-point type.

      function To_Big_Real (Arg : Num) return Valid_Big_Real is
         N, D, V : Big_Integer;

      begin
         if Too_Large then
            return Float_Aux.To_Big_Real (Long_Float (Arg));
         end if;

         N := Conv_U.To_Big_Integer (Num'Small_Numerator);
         D := Conv_U.To_Big_Integer (Num'Small_Denominator);
         V := Conv_I.To_Big_Integer (LLLI'Integer_Value (Arg));

         return V * N / D;
      end To_Big_Real;

      -------------------
      -- From_Big_Real --
      -------------------

      --  We first compute A / B = Arg * D / N where N resp. D is the numerator
      --  resp. the denominator of the Small of the fixed-point type. Then we
      --  divide A by B and convert the result to the mantissa value.

      function From_Big_Real (Arg : Big_Real) return Num is
         N, D, A, B, Q, X : Big_Integer;

      begin
         if Too_Large then
            return Num (Float_Aux.From_Big_Real (Arg));
         end if;

         N := Conv_U.To_Big_Integer (Num'Small_Numerator);
         D := Conv_U.To_Big_Integer (Num'Small_Denominator);
         A := Numerator (Arg) * D;
         B := Denominator (Arg) * N;

         Q := A / B;

         --  Round to nearest, ties to away, by comparing twice the remainder

         X := (A - Q * B) * To_Big_Integer (2);

         if X >= B then
            Q := Q + To_Big_Integer (1);

         elsif X <= -B then
            Q := Q - To_Big_Integer (1);
         end if;

         return Num'Fixed_Value (Conv_I.From_Big_Integer (Q));
      end From_Big_Real;

   end Decimal_Fixed_Conversions;

end TGen.Big_Reals_Aux;
