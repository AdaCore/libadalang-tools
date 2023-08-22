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

with GNAT.Random_Numbers;

with TGen.Numerics;
with TGen.Random;            use TGen.Random;
with TGen.Types.Array_Types; use TGen.Types.Array_Types;

package body TGen.Types.Discrete_Types is

   --  TODO: pick a value in the intervals and not the first one

   ----------
   -- Draw --
   ----------

   function Draw (Intervals : Alternatives_Set) return Big_Integer is
   begin
      return Intervals.First_Element.Min;
   end Draw;

   ---------------
   -- Lit_Image --
   ---------------

   function Lit_Image
     (Self : Discrete_Typ; Lit : Big_Integer) return String is
     (Big_Int.To_String (Lit));

   ---------------
   -- Low_Bound --
   ---------------

   function Low_Bound (Self : Discrete_Typ) return Big_Integer is
     (Big_Zero);

   ----------------
   -- High_Bound --
   ----------------

   function High_Bound (Self : Discrete_Typ) return Big_Integer is
     (Big_Zero);

   --  Sampling strategy: draw an arbitrary value from an arbitrary sample in
   --  a list of samples.

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Sample_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Picked_Index  : constant Positive :=
        Positive (Rand_Int (1, Integer (S.Samples.Length)));
      Picked_Sample : constant Alternatives_Set :=
        S.Samples.Element (Picked_Index);
   begin
      return Create (Draw (Picked_Sample));
   end Generate;

   --------------------------------
   -- Generate_Sampling_Strategy --
   --------------------------------

   function Generate_Sampling_Strategy
     (Self    : Discrete_Typ;
      Samples : Alternatives_Set_Vector) return Strategy_Type'Class
   is
      Strat : Sample_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Samples := Samples;
      return Strat;
   end Generate_Sampling_Strategy;

   generic
      type T is (<>);
   function Gen return T;

   ---------
   -- Gen --
   ---------

   function Gen return T is
      function Rand is new
        GNAT.Random_Numbers.Random_Discrete (T, T'First);
   begin
      return Rand (Generator_Instance);
   end Gen;

   function Generate_Value_Random
     (Ty : Typ'Class) return JSON_Value;

   ---------------------------
   -- Generate_Value_Random --
   ---------------------------

   function Generate_Value_Random
     (Ty : Typ'Class) return JSON_Value
   is
      Self : constant Discrete_Typ'Class := Discrete_Typ'Class (Ty);
      package LLI_Conversions is
        new Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      --  TODO??? Work around until V307-012 is fixed (can't convert to values
      --  outside of Long_Long_Integer bounds).

      type T is new Long_Long_Integer range
        LLI_Conversions.From_Big_Integer
          (Big_Int.Max
             (Self.Low_Bound,
              LLI_Conversions.To_Big_Integer (Long_Long_Integer'First + 1)))
          ..
            LLI_Conversions.From_Big_Integer
              (Big_Int.Min
                 (Self.High_Bound,
                  LLI_Conversions.To_Big_Integer
                    (Long_Long_Integer'Last - 1)));

      function Rand is new Gen (T);
   begin
      return Create
        (LLI_Conversions.To_Big_Integer (Long_Long_Integer (Rand)));
   end Generate_Value_Random;

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Array_Index_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      T_Classwide : constant Typ'Class := S.T.Get;
      T_Discrete  : constant Discrete_Typ'Class :=
        Discrete_Typ'Class (T_Classwide);
   begin

      --  If the other index type is a discriminant for which no value has
      --  been generated yet, let's piggyback on our fallback_strategy.

      if S.Other_Index_Constraint.Kind = TGen.Types.Constraints.Discriminant
        and then not Disc_Context.Contains (S.Other_Index_Constraint.Disc_Name)
      then
         return S.Fallback_Strategy.Generate (Disc_Context);
      end if;

      --  Pick the size. We have to be careful and restrain Max_Size here: we
      --  may go out of the type bounds if the generated value for the
      --  Start_Index constraint was too high, or if the generated value for
      --  the End_Index constraint was too low.
      --
      --  Let's work on the following example:
      --
      --  type Integer_Array is array (Integer range <>) of Integer;
      --
      --  type R (I, J : Integer) is
      --     record
      --        T : Integer_Array (I .. J);
      --     end record;

      declare

         Min_Size : Natural := S.Min_Size;
         Max_Size : Natural := S.Max_Size;
         Avg_Size : Natural := S.Average_Size;

         Other_Index_Value : constant Big_Integer :=
           (if S.Other_Index_Constraint.Kind =
              TGen.Types.Constraints.Discriminant
            then
               Disc_Context (S.Other_Index_Constraint.Disc_Name).Get
            else S.Other_Index_Constraint.Int_Val);

         Elements : Many_Type;

         Result : JSON_Value;
      begin
         if S.Index = End_Index
           and then T_Discrete.High_Bound - Other_Index_Value
             < Nat_Conversions.To_Big_Integer (Max_Size)
         then
            --  We are generating J, and Integer'Last - I < Max_Size,
            --  e.g. with:
            --
            --  Min_Size := 2
            --  Max_Size := 10
            --  I := Integer'Last
            --
            --  We need to shrink Min_Size, and Max_Size, as we won't be able
            --  to have an array larger than 1.

            Max_Size :=
              Nat_Conversions.From_Big_Integer
                (T_Discrete.High_Bound - Other_Index_Value);
            Min_Size := Natural'Min (Min_Size, Max_Size);
         end if;

         if S.Index = Start_Index
           and then Other_Index_Value - T_Discrete.Low_Bound
             < Nat_Conversions.To_Big_Integer (Max_Size)
         then
            --  We are generating I, and J - Integer'First < Max_Size,
            --  e.g. with:
            --
            --  Min_Size := 2
            --  Max_Size := 10
            --  J := Integer'First
            --
            --  We need to shrink Min_Size, and Max_Size, as we won't be able
            --  to have an array larger than 1.

            Max_Size :=
              Nat_Conversions.From_Big_Integer
                (Other_Index_Value - T_Discrete.Low_Bound);
            Min_Size := Natural'Min (Min_Size, Max_Size);
         end if;

         --  We cannot generate a zero length array if the lower index
         --  constraint is already equal to the low bound of the index type, or
         --  if the higher index constraint is equal to the high bound of the
         --  index type.
         --
         --  For instance, if we have:
         --
         --     type Arr is array (Positive range <>) of Integer;
         --
         --     type Rec (I : Positive) is record
         --        Comp : Arr (1 .. I);
         --     end record;
         --
         --  The generating an array length of 0 is impossible, because we
         --  would need to have I equal to 0, which would raise a
         --  Constraint_Error. We must then constrain the length to be at
         --  least one.

         if S.Index = End_Index
           and then Other_Index_Value = T_Discrete.Low_Bound
           and then Min_Size = 0
         then
            Min_Size := 1;
            Max_Size := Natural'Max (Min_Size, Max_Size);
         end if;

         if S.Index = Start_Index
           and then Other_Index_Value = T_Discrete.High_Bound
           and then Min_Size = 0
         then
            Min_Size := 1;
            Max_Size := Natural'Max (Min_Size, Max_Size);
         end if;

         --  Recompute the average size if it no longer fits in between
         --  Min_Size and Max_Size.

         if Avg_Size < Min_Size or else Avg_Size > Max_Size then
            Avg_Size := Natural'Min (Natural'Max (Min_Size * 2, Min_Size + 5),
                                     Min_Size + ((Max_Size - Min_Size) / 2));
         end if;

         --  Then, we can safely generate our size

         Elements := Many (Min_Size, Max_Size, Avg_Size);

         while Elements.More loop
            null;
         end loop;

         --  Now that we have computed our size, generate the value accordingly
         --  We are sure that we won't go out of the bounds because of the
         --  whole machinery above.

         if S.Index = Start_Index then
            Result := Create
              (Other_Index_Value - To_Big_Integer (Elements.Count) + 1);
         else
            Result := Create
              (Other_Index_Value + To_Big_Integer (Elements.Count) - 1);
         end if;

         return Result;
      end;
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Identity_Constraint_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is (Create (Value (S.Constraint, Disc_Context)));

   ----------------------------------------------
   -- Generate_Array_Index_Constraint_Strategy --
   ----------------------------------------------

   function Generate_Array_Index_Constraint_Strategy
     (Self       : Discrete_Typ'Class;
      Var_Name   : Unbounded_String;
      Constraint : TGen.Types.Constraints.Index_Constraint)
      return Strategy_Type'Class
   is
      use TGen.Numerics.Nat_Conversions;

      Strat : Array_Index_Strategy_Type;

      ILB : constant Discrete_Constraint_Value :=
        Constraint.Discrete_Range.Low_Bound;
      IHB : constant Discrete_Constraint_Value :=
        Constraint.Discrete_Range.High_Bound;

      Min_Size, Max_Size, Average_Size : Natural;
   begin

      --  We don't know yet what could possibly be the size of the array, just
      --  that it can be modulated by at least one discriminant. Let's just
      --  encode arbitrary Min/Max_Sizes.

      Min_Size := Unconstrained_Array_Size_Min;
      Max_Size :=
        From_Big_Integer
          (Big_Int.Min
             (Self.High_Bound - Self.Low_Bound,
              Nat_Conversions.To_Big_Integer (Unconstrained_Array_Size_Max)));
      Average_Size :=
        Natural'Min (Natural'Max (Min_Size * 2, Min_Size + 5),
                     Min_Size + ((Max_Size - Min_Size) / 2));

      if ILB.Kind = Discriminant and then ILB.Disc_Name = Var_Name
      then
         Strat.Index := Start_Index;
         Strat.Other_Index_Constraint := IHB;
      end if;

      if IHB.Kind = Discriminant and then IHB.Disc_Name = Var_Name
      then
         Strat.Index := End_Index;
         Strat.Other_Index_Constraint := ILB;
      end if;

      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Average_Size := Average_Size;
      Strat.Min_Size := Min_Size;
      Strat.Max_Size := Max_Size;

      Strat.Fallback_Strategy :=
        new Strategy_Type'Class'(Self.Default_Strategy);
      return Strat;
   end Generate_Array_Index_Constraint_Strategy;

   -------------------------------------------
   -- Generate_Identity_Constraint_Strategy --
   -------------------------------------------

   function Generate_Identity_Constraint_Strategy
     (Self       : Discrete_Typ'Class;
      Constraint : Discrete_Constraint_Value) return Strategy_Type'Class
   is
      Strat : Identity_Constraint_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Constraint := Constraint;
      return Strat;
   end Generate_Identity_Constraint_Strategy;

   ----------
   -- Init --
   ----------

   procedure Init (S : in out First_Last_Strategy_Type) is
   begin
      S.Generation := First;
   end Init;

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out First_Last_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Discrete_T : Discrete_Typ'Class renames
        Discrete_Typ'Class (S.T.Unchecked_Get.all);
   begin
      case S.Generation is
         when First =>
            S.Generation := Last;
            return Create (Discrete_T.Low_Bound);
         when Last =>
            S.Generation := Unknown;
            return Create (Discrete_T.High_Bound);
         when others =>
            raise Program_Error;
      end case;
   end Generate;

   ---------------------------
   -- Default_Enum_Strategy --
   ---------------------------

   overriding function Default_Enum_Strategy
     (Self : Discrete_Typ) return Enum_Strategy_Type'Class
   is
      Strat : First_Last_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      return Strat;
   end Default_Enum_Strategy;

   type Big_Int_Array is array (Positive range <>) of Big_Integer;

   type Sequence_Enum_Strategy (Num_Values : Positive) is
      new Enum_Strategy_Type with
   record
      Values      : Big_Int_Array (1 .. Num_Values);
      --  Sequence of values to be output

      Current_Val : Positive;
      --  Index in the above array for the next value to be generated
   end record;

   overriding procedure Init (S : in out Sequence_Enum_Strategy);

   overriding function Has_Next (S : Sequence_Enum_Strategy) return Boolean;

   overriding function Generate
     (S            : in out Sequence_Enum_Strategy;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   ----------
   -- Init --
   ----------

   overriding procedure Init (S : in out Sequence_Enum_Strategy) is
   begin
      S.Current_Val := 1;
   end Init;

   --------------
   -- Has_Next --
   --------------

   overriding function Has_Next (S : Sequence_Enum_Strategy) return Boolean is
     (S.Current_Val <= S.Num_Values);

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Sequence_Enum_Strategy;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      pragma Unreferenced (Disc_Context);
      Res : constant JSON_Value := Create (S.Values (S.Current_Val));
   begin
      S.Current_Val := S.Current_Val + 1;
      return Res;
   end Generate;

   ----------------------------------------
   -- Make_Single_Array_Constraint_Strat --
   ----------------------------------------

   function Make_Single_Array_Constraint_Strat
     (T : SP.Ref; Constraints : Index_Constraint)
     return Enum_Strategy_Type'Class
   is
      Self       : Discrete_Typ'Class renames
        Discrete_Typ'Class (T.Unchecked_Get.all);
      Res        : Sequence_Enum_Strategy (3);
      Disc_Is_LB : constant Boolean :=
        Constraints.Discrete_Range.Low_Bound.Kind = Discriminant;

      Op  : constant access
        function (L, R : Valid_Big_Integer) return Valid_Big_Integer :=
          (if Disc_Is_LB then Big_Int."-"'Access else Big_Int."+"'Access);

      Offsets     : constant Big_Int_Array :=
        [To_Big_Integer (0), To_Big_Integer (1), To_Big_Integer (1000)];
      Base        : constant Big_Integer :=
        (if Disc_Is_LB
         then Constraints.Discrete_Range.High_Bound.Int_Val
         else Constraints.Discrete_Range.Low_Bound.Int_Val);
   begin
      for I in Offsets'Range loop
         Res.Values (I) := Op (Base, Offsets (I));

         --  Cap the value to the bounds of the type if needed.

         if Disc_Is_LB and then Res.Values (I) < Self.Low_Bound then
            Res.Values (I) := Self.Low_Bound;
         elsif not Disc_Is_LB and then Res.Values (I) > Self.High_Bound then
            Res.Values (I) := Self.High_Bound;
         end if;
      end loop;
      return Res;
   end Make_Single_Array_Constraint_Strat;

   --------------------------------------
   -- Make_Dual_Array_Constraint_Strat --
   --------------------------------------

   function Make_Dual_Array_Constraint_Strat
     (T          : SP.Ref;
      Constraint : Index_Constraint;
      Disc_Name  : Unbounded_String) return Enum_Strategy_Type'Class
   is
      Self         : Discrete_Typ'Class renames
        Discrete_Typ'Class (T.Unchecked_Get.all);
      Is_Low_Bound : constant Boolean :=
        Constraint.Discrete_Range.Low_Bound.Disc_Name = Disc_Name;
      Res : Sequence_Enum_Strategy (if Is_Low_Bound then 2 else 3);
   begin
      Res.Values (1) := Self.Low_Bound;
      Res.Values (2) := Self.Low_Bound + To_Big_Integer (1);
      if not Is_Low_Bound then
         Res.Values (3) := Self.Low_Bound + To_Big_Integer (1000);
      end if;
      for I in 2 .. Res.Num_Values loop
         if Res.Values (I) > Self.High_Bound then
            Res.Values (I) := Self.High_Bound;
         end if;
      end loop;
      return Res;
   end Make_Dual_Array_Constraint_Strat;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Generate_Static_Common
     (Self : Discrete_Typ'Class) return Strategy_Type'Class
   is
      Strat : Basic_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.F := Generate_Value_Random'Access;
      return Strat;
   end Generate_Static_Common;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self : Discrete_Typ) return Strategy_Type'Class
   is
   begin
      return Generate_Static_Common (Self);
   end Default_Strategy;

end TGen.Types.Discrete_Types;
