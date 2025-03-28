------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with Ada.Containers.Vectors;

with GNAT.Random_Numbers;

with TGen.Numerics; use TGen.Numerics;
with TGen.Strings;  use TGen.Strings;
with TGen.Random;   use TGen.Random;

package body TGen.Types.Int_Types is

   function Image (Self : Signed_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image
         & ": Signed Integer"
         & (if Self.Is_Static
            then
              " range "
              & Big_Int.To_String (Self.Range_Value.Min)
              & " .."
              & Big_Int.To_String (Self.Range_Value.Max)
            else " (non static)"));
   end Image;

   function Low_Bound (Self : Signed_Int_Typ) return Big_Integer
   is (Self.Range_Value.Min);

   function High_Bound (Self : Signed_Int_Typ) return Big_Integer
   is (Self.Range_Value.Max);

   function Image (Self : Mod_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image
         & ": Modular Integer"
         & (if Self.Is_Static
            then " mod" & Big_Int.To_String (Self.Mod_Value)
            else "(non static)"));
   end Image;

   function Low_Bound (Self : Mod_Int_Typ) return Big_Integer is
   begin
      return Self.Range_Value.Min;
   end Low_Bound;

   function High_Bound (Self : Mod_Int_Typ) return Big_Integer is
   begin
      return Self.Range_Value.Max;
   end High_Bound;

   function Gen return T is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (T, T'First);
   begin
      return Rand (Generator_Instance);
   end Gen;

   package Interval_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Int_Range);
   subtype Interval_Vector is Interval_Vectors.Vector;

   function Get_Digits_Equivalence_Classes
     (R : Int_Range) return Interval_Vector;
   --  Cut the Int_Range space in same-number-of-digits partition, e.g.
   --  passing [0, 999] will return [0, 9], [10, 99], [100, 999].

   ------------------------------------
   -- Get_Digits_Equivalence_Classes --
   ------------------------------------

   function Get_Digits_Equivalence_Classes
     (R : Int_Range) return Interval_Vector
   is

      Result : Interval_Vector;

      LLI_Last  : constant Big_Integer :=
        LLLI_Conversions.To_Big_Integer
          (Long_Long_Long_Integer (Long_Long_Integer'Last - 1));
      LLI_First : constant Big_Integer :=
        LLLI_Conversions.To_Big_Integer
          (Long_Long_Long_Integer (Long_Long_Integer'First + 1));
   begin

      --  Positive intervals: for Integer -> [0, 9], [10, 99], ...
      --                      for Positive -> [1, 9], [10, 99], ...
      declare

         Low_Bound        : Big_Integer := Big_Int.Max (R.Min, 0);
         Number_Of_Digits : Integer :=
           (if Low_Bound = 0 then 1 else Log (Low_Bound, 10.0) + 1);
         High_Bound       : Big_Integer :=
           Big_Int.Min (10**Number_Of_Digits - 1, R.Max);
      begin

         --  While LLLI_Conversions do not actually support values outside of
         --  Long_Long_Integer'Range, we must limit the equivalence classes
         --  that we create to not have empty ranges.
         --  TODO: Remove when V307-012 is closed

         while Low_Bound < LLI_Last and then Low_Bound <= R.Max loop
            Result.Append (Int_Range'(Min => Low_Bound, Max => High_Bound));
            Low_Bound := High_Bound + 1;
            Number_Of_Digits := Number_Of_Digits + 1;
            High_Bound := Big_Int.Min (10**Number_Of_Digits - 1, R.Max);
         end loop;
      end;

      --  Negative intervals: for Integer -> [-9, -1], [-99, -10], ...

      declare
         High_Bound       : Big_Integer := Big_Int.Min (R.Max, -1);
         Number_Of_Digits : Integer := Log (-High_Bound, 10.0) + 1;
         Low_Bound        : Big_Integer :=
           Big_Int.Max (-10**Number_Of_Digits + 1, R.Min);
      begin
         while High_Bound > LLI_First and then High_Bound >= R.Min loop
            Result.Append (Int_Range'(Min => Low_Bound, Max => High_Bound));
            High_Bound := Low_Bound - 1;
            Number_Of_Digits := Number_Of_Digits + 1;
            Low_Bound := Big_Int.Max (-10**Number_Of_Digits + 1, R.Min);
         end loop;
      end;
      return Result;
   end Get_Digits_Equivalence_Classes;

   package Equivalence_Classes_Strategy_Int_Typ is

      package Equivalence_Classes_Strategy_Internal is new
        Equivalence_Classes_Strategy_Package
          (Equivalence_Class_Type      => Int_Range,
           Equivalence_Classes_Vectors => Interval_Vectors);

      subtype Strategy is
        Equivalence_Classes_Strategy_Internal.Equivalence_Class_Strategy_Type;

      use LLLI_Conversions;

      function Draw (T : Typ_Access; R : Int_Range) return JSON_Value;

   end Equivalence_Classes_Strategy_Int_Typ;

   package body Equivalence_Classes_Strategy_Int_Typ is
      function Draw
        (T : Typ_Access with Unreferenced; R : Int_Range) return JSON_Value
      is

         --  Constrain the range of possible values to
         --  LLI'First + 1 .. LLI'Last - 1 until V307-012 is fixed.

         First_LLI : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Long_Long_Long_Integer (Long_Long_Integer'First + 1));
         Last_LLI  : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Long_Long_Long_Integer (Long_Long_Integer'Last - 1));
      begin
         return
           (Create
              (LLLI_Conversions.To_Big_Integer
                 (Rand_LLLI
                    (From_Big_Integer
                       (if R.Min <= First_LLI then First_LLI else R.Min),
                     From_Big_Integer
                       (if R.Max >= Last_LLI then Last_LLI else R.Max)))));
      end Draw;

   end Equivalence_Classes_Strategy_Int_Typ;

   function Generate_Equivalence_Class_Digit_Strategy
     (T : Signed_Int_Typ'Class) return Strategy_Type'Class;

   -----------------------------------------------
   -- Generate_Equivalence_Class_Digit_Strategy --
   -----------------------------------------------

   function Generate_Equivalence_Class_Digit_Strategy
     (T : Signed_Int_Typ'Class) return Strategy_Type'Class
   is
      Strat : Equivalence_Classes_Strategy_Int_Typ.Strategy;
   begin
      Strat.T := T'Unrestricted_Access;
      Strat.Classes := Get_Digits_Equivalence_Classes (T.Range_Value);
      Strat.Draw := Equivalence_Classes_Strategy_Int_Typ.Draw'Access;
      return Strat;
   end Generate_Equivalence_Class_Digit_Strategy;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy (Self : Signed_Int_Typ) return Strategy_Type'Class
   is
      Strat_Random : constant Strategy_Type'Class :=
        Generate_Static_Common (Discrete_Typ'Class (Self));

      Strat_Equivalence_Classes : constant Strategy_Type'Class :=
        Generate_Equivalence_Class_Digit_Strategy (Self);
   begin
      return
        Make_Dispatching_Strat (Strat_Random, Strat_Equivalence_Classes, 0.5);
   end Default_Strategy;

end TGen.Types.Int_Types;
