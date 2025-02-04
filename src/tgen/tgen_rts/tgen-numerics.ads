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
--
--  Numeric utility functions for Big integers

with Ada.Numerics.Generic_Elementary_Functions;

with TGen.Big_Int;
with TGen.Big_Reals;

package TGen.Numerics is

   pragma Preelaborate;

   function From_Universal_Image (Num, Den : String) return Big_Reals.Big_Real
   is (Big_Reals."/" (Big_Int.From_String (Num), Big_Int.From_String (Den)));

   package F_Conversions is new Big_Reals.Float_Conversions (Float);
   package LF_Conversions is new Big_Reals.Float_Conversions (Long_Float);
   --  We use Long_Float instead of Long_Long_Float for random generation
   --  purposes because there is a bug Float_Conversions which results in
   --  Storage_Errors when using the package instantiated with Long_Long_Float.
   --
   --  TODO???: this still means that we will be unable to convert / generate
   --  Long_Long_Floats that do not fit on a Long_Float.

   package LF_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   package LLLI_Conversions is new
     Big_Int.Signed_Conversions (Long_Long_Long_Integer);

   package Nat_Conversions is new Big_Int.Signed_Conversions (Natural);

   function Log (N : Big_Int.Big_Integer; Base : Long_Float) return Integer
   is (Integer
         (LF_Functions.Log
            (Long_Float (LLLI_Conversions.From_Big_Integer (N)), Base)));

   type Precision_Type is (Single, Double, Extended);

   type Any_Float (Precision : Precision_Type) is private;

   function Value (F : Any_Float) return Float
   with Pre => F.Precision = Single;

   function Value (F : Any_Float) return Long_Float
   with Pre => F.Precision = Double;

   function Value (F : Any_Float) return Long_Long_Float
   with Pre => F.Precision = Extended;

   function Create (F : Float) return Any_Float
   with Post => Create'Result.Precision = Single;

   function Create (LF : Long_Float) return Any_Float
   with Post => Create'Result.Precision = Double;

   function Create (LLF : Long_Long_Float) return Any_Float
   with Post => Create'Result.Precision = Extended;

   function Create
     (Digits_Value : Natural; R : Big_Reals.Big_Real) return Any_Float;

   function Value (F : Any_Float) return Big_Reals.Big_Real;

   function Digits_To_Precision (Digits_Value : Natural) return Precision_Type
   is
      --  GNAT makes conservative assumption about the number of digits for
      --  each of the floating point precision: a single-precision floating
      --  point will have at the very minimum 6 digits of precision, a double
      --  15 digits, and in the extended precision format, 18 digits. See the
      --  specification of the IEEE floating point format for more information.
      --
      --  TODO???: this should use the Machine_Mantissa attribute.

      (if Digits_Value <= 6 then Single
       elsif Digits_Value <= 15 then Double
       elsif Digits_Value <= 18 then Extended
       else raise Program_Error with "Unsupported digits value.");

   function First (Digits_Value : Natural) return Any_Float
   is (case Digits_To_Precision (Digits_Value) is
         when Single => Create (Float'First),
         when Double => Create (Long_Float'First),
         when Extended => Create (Long_Long_Float'First));

   function Last (Digits_Value : Natural) return Any_Float
   is (case Digits_To_Precision (Digits_Value) is
         when Single => Create (Float'Last),
         when Double => Create (Long_Float'Last),
         when Extended => Create (Long_Long_Float'Last));

private
   type Any_Float (Precision : Precision_Type) is record
      case Precision is
         when Single =>
            F : Float;

         when Double =>
            LF : Long_Float;

         when Extended =>
            LLF : Long_Long_Float;
      end case;
   end record;

   function Value (F : Any_Float) return Float
   is (F.F);
   function Value (F : Any_Float) return Long_Float
   is (F.LF);
   function Value (F : Any_Float) return Long_Long_Float
   is (F.LLF);

   function Create (F : Float) return Any_Float
   is ((Precision => Single, F => F));

   function Create (LF : Long_Float) return Any_Float
   is ((Precision => Double, LF => LF));

   function Create (LLF : Long_Long_Float) return Any_Float
   is ((Precision => Extended, LLF => LLF));

   function Create
     (Digits_Value : Natural; R : Big_Reals.Big_Real) return Any_Float
   is (case Digits_To_Precision (Digits_Value) is
         when Single => (Single, F_Conversions.From_Big_Real (R)),
         when Double => (Double, LF_Conversions.From_Big_Real (R)),
         when Extended =>
           (Extended, Long_Long_Float (LF_Conversions.From_Big_Real (R))));

   function Value (F : Any_Float) return Big_Reals.Big_Real
   is (case F.Precision is
         when Single => F_Conversions.To_Big_Real (F.F),
         when Double => LF_Conversions.To_Big_Real (F.LF),
         when Extended => LF_Conversions.To_Big_Real (Long_Float (F.LLF)));

end TGen.Numerics;
