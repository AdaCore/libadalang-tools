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
with TGen.Strings; use TGen.Strings;
with TGen.Random; use TGen.Random;

package body TGen.Types.Int_Types is

   function Image (Self : Signed_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Signed Integer"
         & (if Self.Is_Static
            then " range " & Big_Int.To_String (Self.Range_Value.Min) & " .."
                 & Big_Int.To_String (Self.Range_Value.Max)
            else " (non static)"));
   end Image;

   function Low_Bound (Self : Signed_Int_Typ) return Big_Integer is
     (Self.Range_Value.Min);

   function High_Bound (Self : Signed_Int_Typ) return Big_Integer is
     (Self.Range_Value.Max);

   function Image (Self : Mod_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Modular Integer"
         & (if Self.Is_Static
            then " mod" & Big_Int.To_String (Self.Mod_Value)
            else "(non static)"));
   end Image;

   function High_Bound (Self : Mod_Int_Typ) return Big_Integer is
      use Big_Int;
   begin
      return Self.Mod_Value - To_Big_Integer (1);
   end High_Bound;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self    : Int_Typ;
      Context : in out Generation_Context) return Strategy_Type'Class
   is
      Result : Dynamic_Strategy_Type
        (Kind => Random_Kind, Constrained => False);
      F_Body : Unbounded_String;
      Indent : Natural := 0;
   begin
      Write_Line (F_Body, "declare", Indent);
      Indent := @ + 3;
      Write_Line
        (F_Body,
         "function Gen is new TGen.Types.Int_Types.Gen ("
         & Self.Fully_Qualified_Name & ");",
         Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "begin", Indent);
      Indent := @ + 3;
      Write_Line (F_Body, "return Gen;", Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "end;", Indent);

      Result.Strategy_Function := Self.Random_Strategy_Function;
      Result.Strategy_Body := +(+F_Body);
      return Result;
   end Generate_Random_Strategy;

   --  Code for dynamic generation. This is paused for the moment but we do not
   --  want to throw away all the ideas in it so keep these comments for the
   --  moment.
   ------------------------------
   -- Generate_Sample_Strategy --
   ------------------------------

   --  function Generate_Sample_Strategy
   --    (Self   : Int_Typ;
   --     GC     : Generation_Context;
   --     Initialize_F_Name : String;
   --     F_Name : String;
   --     Sample : Alternatives_Set) return String is
   --     Result : Unbounded_String;
   --     Indent : Natural := 0;
   --  begin
   --
   --     --  Write the Initialize function
   --
   --     Write_Line
   --       (Result,
   --        "procedure " & Initialize_F_Name
   --        & " (State : in out Alternatives_Set) ",
   --       Indent);
   --     Write_Line (Result, "is", Indent);
   --     Write_Line (Result, "begin", Indent);
   --     Indent := Indent + 3;
   --     Write_Line
   --       (Result, "State := " & Alternatives_Set'Image (Sample), Indent);
   --     Indent := Indent - 3;
   --     Write_Line ("end;");
   --
   --     Write_Line
   --       (Result,
   --        "function " & F_Name & " (State : Alternatives_Set) return "
   --        & (+Self.Fully_Qualified_Name),
   --        Indent);
   --     Indent := @ + 3;
   --     Write_Line (Result, "is", Indent);
   --     Indent := @ + 3;
   --     Write_Line
   --       (Result,
   --        "function Gen is new TGen.Types.Int_Types.Gen ("
   --        & (+Self.Fully_Qualified_Name) & ");",
   --        Indent);
   --     Indent := @ - 3;
   --     Write_Line (Result, "begin", Indent);
   --     Indent := @ + 3;
   --     Write_Line (Result, "return Gen;", Indent);
   --     Indent := @ - 3;
   --     Write_Line (Result, "end " & F_Name & ";", Indent);
   --     return +Result;
   --  end Generate_Sample_Strategy;

   function Gen return T is
      function Rand is new
        GNAT.Random_Numbers.Random_Discrete (T, T'First);
   begin
      return Rand (Generator_Instance);
   end Gen;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Int_Range);
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
      use type Big_Int.Big_Integer;

      Result : Interval_Vector;

      LLI_Last : constant Big_Integer :=
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
           Big_Int.Min
             (10 ** Number_Of_Digits - 1, R.Max);
      begin

         --  While LLLI_Conversions do not actually support values outside of
         --  Long_Long_Integer'Range, we must limit the equivalence classes
         --  that we create to not have empty ranges.
         --  TODO: Remove when V307-012 is closed

         while Low_Bound < LLI_Last and then Low_Bound < R.Max loop
            Result.Append (Int_Range'(Min => Low_Bound, Max => High_Bound));
            Low_Bound := High_Bound + 1;
            Number_Of_Digits := Number_Of_Digits + 1;
            High_Bound :=
              Big_Int.Min (10 ** Number_Of_Digits - 1, R.Max);
         end loop;
      end;

      --  Negative intervals: for Integer -> [-9, -1], [-99, -10], ...

      declare
         High_Bound       : Big_Integer := Big_Int.Min (R.Max, -1);
         Number_Of_Digits : Integer := Log (-High_Bound, 10.0) + 1;
         Low_Bound        : Big_Integer :=
           Big_Int.Max (-10 ** Number_Of_Digits + 1, R.Min);
      begin
         while High_Bound > LLI_First and then High_Bound > R.Min loop
            Result.Append (Int_Range'(Min => Low_Bound, Max => High_Bound));
            High_Bound := Low_Bound - 1;
            Number_Of_Digits := Number_Of_Digits + 1;
            Low_Bound :=
              Big_Int.Max (-10 ** Number_Of_Digits + 1, R.Min);
         end loop;
      end;
      return Result;
   end Get_Digits_Equivalence_Classes;

   package Equivalence_Classes_Strategy_Int_Typ is

      package Equivalence_Classes_Strategy_Internal is
        new Equivalence_Classes_Strategy_Package
          (Equivalence_Class_Type      => Int_Range,
           Equivalence_Classes_Vectors => Interval_Vectors);

      subtype Strategy is
        Equivalence_Classes_Strategy_Internal.Equivalence_Class_Strategy_Type;

      use LLLI_Conversions;

      function Draw (T : SP.Ref; R : Int_Range) return Static_Value'Class;

   end Equivalence_Classes_Strategy_Int_Typ;

   package body Equivalence_Classes_Strategy_Int_Typ is
      function Draw (T : SP.Ref; R : Int_Range) return Static_Value'Class is
         use Big_Int;

         --  Constrain the range of possible values to
         --  LLI'First + 1 .. LLI'Last - 1 until V307-012 is fixed.

         First_LLI : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Long_Long_Long_Integer (Long_Long_Integer'First + 1));
         Last_LLI  : constant Big_Integer :=
           LLLI_Conversions.To_Big_Integer
             (Long_Long_Long_Integer (Long_Long_Integer'Last - 1));
      begin
         return (Discrete_Static_Value'
                   (T     => T,
                    Value =>
                      LLLI_Conversions.To_Big_Integer
                        (Rand_LLLI
                          (From_Big_Integer (if R.Min <= First_LLI
                                             then First_LLI
                                             else R.Min),
                           From_Big_Integer (if R.Max >= Last_LLI
                                             then Last_LLI
                                             else R.Max)))));
      end Draw;

   end Equivalence_Classes_Strategy_Int_Typ;

   function Generate_Equivalence_Class_Digit_Strategy
     (T : Signed_Int_Typ'Class) return Static_Strategy_Type'Class;

   -----------------------------------------------
   -- Generate_Equivalence_Class_Digit_Strategy --
   -----------------------------------------------

   function Generate_Equivalence_Class_Digit_Strategy
     (T : Signed_Int_Typ'Class) return Static_Strategy_Type'Class is
      Strat : Equivalence_Classes_Strategy_Int_Typ.Strategy;
   begin
      SP.From_Element (Strat.T, T'Unrestricted_Access);
      Strat.Classes := Get_Digits_Equivalence_Classes (T.Range_Value);
      Strat.Draw := Equivalence_Classes_Strategy_Int_Typ.Draw'Access;
      return Strat;
   end Generate_Equivalence_Class_Digit_Strategy;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Signed_Int_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Strat_Random : constant Static_Strategy_Type'Class :=
        Generate_Static_Common (Discrete_Typ'Class (Self), Context);

      Strat_Equivalence_Classes : constant Static_Strategy_Type'Class :=
        Generate_Equivalence_Class_Digit_Strategy (Self);
   begin
      return Make_Dispatching_Strat
        (Strat_Random, Strat_Equivalence_Classes, 0.5);
   end Generate_Static;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (Strat : in out Static_Array_Constraint_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      package N_Conversions is
        new Big_Int.Signed_Conversions (Int => Natural);
      use N_Conversions;

      use Big_Int;

      Elements : Many_Type :=
        Many
          (0,
           From_Big_Integer
             (Strat.T.Range_Value.Max - Strat.T.Range_Value.Min),
           Strat.Avg_Size);
   begin
      while Elements.More loop
         null;
      end loop;
      return Base_Static_Value'(Value => +Natural'Image (Elements.Count));
   end Generate_Static_Value;

   ----------------------------------------
   -- Generate_Array_Constraint_Strategy --
   ----------------------------------------

   function Generate_Array_Constraint_Strategy
     (Self : Signed_Int_Typ) return Static_Array_Constraint_Strategy_Type'Class
   is
   begin
      case Self.Is_Static is
         when True =>
            declare
               T : constant Static_Array_Constraint_Strategy_Type :=
                 (T => Self, Avg_Size => 5);
            begin
               return T;
            end;
         when others =>
            return raise Program_Error with "unsupported non static type";
      end case;
   end Generate_Array_Constraint_Strategy;

end TGen.Types.Int_Types;
