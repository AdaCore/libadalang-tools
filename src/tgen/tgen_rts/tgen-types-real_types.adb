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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Random_Numbers;

with TGen.Strings; use TGen.Strings;
with TGen.Random;  use TGen.Random;

package body TGen.Types.Real_Types is

   function Image (Self : Float_Typ) return String is
     (Typ (Self).Image & ": Real Type"
      & (if Self.Is_Static
         then " digits " & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range " & Big_Reals.To_String (Self.Range_Value.Min)
                      & " .. " & Big_Reals.To_String (Self.Range_Value.Max)
                 else "")
         else " (non static)"));

   function Image (Self : Ordinary_Fixed_Typ) return String is
     (Typ (Self).Image & ": Ordinary Fixed Point"
      & (if Self.Is_Static
         then " delta " & Big_Reals.To_String (Self.Delta_Value) & " range "
              & Big_Reals.To_String (Self.Range_Value.Min) & " .. "
              & Big_Reals.To_String (Self.Range_Value.Max)
         else " (non static)"));

   function Image (Self : Decimal_Fixed_Typ) return String is
     (Typ (Self).Image & ": Decimal Fixed Point"
      & (if Self.Is_Static
         then " delta " & Big_Reals.To_String (Self.Delta_Value) & " digits"
              & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range " & Big_Reals.To_String (Self.Range_Value.Min)
                      & " .. " & Big_Reals.To_String (Self.Range_Value.Max)
                 else "")
         else " (non static)"));

   function Low_Bound_Or_Default (Self : Float_Typ) return Big_Real is
     (if Self.Has_Range and then Big_Reals.Is_Valid (Self.Range_Value.Min)
      then Self.Range_Value.Min
      else LF_Conversions.To_Big_Real (Long_Float'First));

   function High_Bound_Or_Default (Self : Float_Typ) return Big_Real is
     (if Self.Has_Range and then Big_Reals.Is_Valid (Self.Range_Value.Max)
      then Self.Range_Value.Max
      else LF_Conversions.To_Big_Real (Long_Float'Last));

   function Low_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   is (Self.Range_Value.Min);

   function High_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   is (Self.Range_Value.Max);

   function Low_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   is
   begin
      return (if Self.Has_Range
              then Self.Range_Value.Min
              else -(To_Real (10) ** Self.Digits_Value - To_Real (1))
                    * Self.Delta_Value);
   end Low_Bound_Or_Default;

   function High_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   is
   begin
      return (if Self.Has_Range
              then Self.Range_Value.Max
              else (To_Real (10) ** Self.Digits_Value - To_Real (1))
                   * Self.Delta_Value);
   end High_Bound_Or_Default;

   function Gen return T is
      function Rand is new GNAT.Random_Numbers.Random_Float (T);

      --  TODO: the random number generator generates number between 0 and 1:
      --  enhance that to generate number over the whole type span.

   begin
      return Rand (Generator_Instance);
   end Gen;

   function Generate_Float_Typ (Ty : Typ'Class) return JSON_Value;

   ------------------------
   -- Generate_Float_Typ --
   ------------------------

   function Generate_Float_Typ (Ty : Typ'Class) return JSON_Value
   is
      Result : constant JSON_Value := Create_Object;
      Self   : constant Float_Typ := Float_Typ (Ty);

      LB : constant Big_Real := Self.Low_Bound_Or_Default;

      HB : constant Big_Real := Self.High_Bound_Or_Default;

      subtype T is Long_Float
      range LF_Conversions.From_Big_Real (LB)
        .. LF_Conversions.From_Big_Real (HB);

      function Rand is new Gen (T);
   begin
      Set_Field (Result, "quotient", True);
      Set_Field (Result,
                 "value",
                 To_Quotient_String (LF_Conversions.To_Big_Real (Rand)));
      return Result;
   end Generate_Float_Typ;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self    : Float_Typ) return Strategy_Type'Class
   is
      --  TODO: use Long_Long_Long_Integer (as it is the biggest possible type
      --  for which ranges can be defined), and add support to it in
      --  GNATCOLL.JSON.

      Type_Ref : SP.Ref;
      Strat : Basic_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Float_Typ'Access;
      return Strat;
   end Default_Strategy;

   function Generate_Ordinary_Fixed_Typ
     (Ty : Typ'Class) return JSON_Value;

   function Generate_Decimal_Fixed_Typ
     (Ty : Typ'Class) return JSON_Value;

   ---------------------------------
   -- Generate_Ordinary_Fixed_Typ --
   ---------------------------------

   function Generate_Ordinary_Fixed_Typ
     (Ty : Typ'Class) return JSON_Value
   is
      use LLLI_Conversions;
      Result : constant JSON_Value := Create_Object;
      Self   : constant Ordinary_Fixed_Typ := Ordinary_Fixed_Typ (Ty);

      --  Translate the fixed type to the integer type

      Low_Bound  : constant Big_Real :=
        Self.Range_Value.Min / Self.Delta_Value;
      High_Bound : constant Big_Real :=
        Self.Range_Value.Max / Self.Delta_Value;

      Low_Bound_Int  : Long_Long_Long_Integer;
      High_Bound_Int : Long_Long_Long_Integer;

   begin
      --  Check that the denominator is 1 for each of the integer bound

      pragma Assert (Denominator (Low_Bound) = 1);
      pragma Assert (Denominator (High_Bound) = 1);

      Low_Bound_Int := From_Big_Integer (Numerator (Low_Bound));
      High_Bound_Int := From_Big_Integer (Numerator (High_Bound));

      --  Now, generate a random integer value

      declare
         Rand_Val : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Rand_LLLI (Low_Bound_Int, High_Bound_Int));
      begin
         --  Cast it back to a fixed point value

         Set_Field (Result, "quotient", True);
         Set_Field (Result,
                    "value",
                    To_Quotient_String
                      (To_Big_Real (Rand_Val) * Self.Delta_Value));
         return Result;
      end;
   end Generate_Ordinary_Fixed_Typ;

   --------------------------------
   -- Generate_Decimal_Fixed_Typ --
   --------------------------------

   function Generate_Decimal_Fixed_Typ
     (Ty : Typ'Class) return JSON_Value
   is
      use LLLI_Conversions;
      Result : constant JSON_Value := Create_Object;
      Self   : constant Decimal_Fixed_Typ := Decimal_Fixed_Typ (Ty);

      --  TODO: Using High/Low_Bound_Or_Default ignores the digits value, which
      --  may not play nice with the digits value if it is too low. We may need
      --  to generate some "sparse" integer ranges to ensure we do not go
      --  beyond the specified precision.

      --  Translate the fixed type to the integer type

      Low_Bound  : constant Big_Real :=
        Self.Low_Bound_Or_Default / Self.Delta_Value;
      High_Bound : constant Big_Real :=
        Self.High_Bound_Or_Default / Self.Delta_Value;

      Low_Bound_Int  : Long_Long_Long_Integer;
      High_Bound_Int : Long_Long_Long_Integer;

   begin
      --  Check that the denominator is 1 for each of the integer bound

      pragma Assert (Denominator (Low_Bound) = 1);
      pragma Assert (Denominator (High_Bound) = 1);

      Low_Bound_Int := From_Big_Integer (Numerator (Low_Bound));
      High_Bound_Int := From_Big_Integer (Numerator (High_Bound));

      --  Now, generate a random integer value

      declare
         Rand_Val : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Rand_LLLI (Low_Bound_Int, High_Bound_Int));
      begin
         --  Cast it back to a fixed point value

         Set_Field (Result, "quotient", True);
         Set_Field (Result,
                    "value",
                    To_Quotient_String
                      (To_Big_Real (Rand_Val) * Self.Delta_Value));
         return Result;
      end;
   end Generate_Decimal_Fixed_Typ;

   overriding function Default_Strategy
     (Self    : Ordinary_Fixed_Typ) return Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Ordinary_Fixed_Typ'Access;
      return Strat;
   end Default_Strategy;

   overriding function Default_Strategy
     (Self    : Decimal_Fixed_Typ) return Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Decimal_Fixed_Typ'Access;
      return Strat;
   end Default_Strategy;

end TGen.Types.Real_Types;
