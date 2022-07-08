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

with Ada.Tags; use Ada.Tags;

with TGen.Numerics;             use TGen.Numerics;
with TGen.Random;               use TGen.Random;
with TGen.Strings;              use TGen.Strings;
with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;

package body TGen.Types.Array_Types is

   use TGen.Types.SP;

   function Image (Self : Unconstrained_Array_Typ) return String is
      Res : Unbounded_String :=
        To_Unbounded_String (Typ (Self).Image & " : array (");
   begin
      for J in Self.Index_Types'Range loop
         if Self.Index_Types (J) /= SP.Null_Ref then
            Res := Res & Typ (Self.Index_Types (J).Get.Element.all).Image;
         else
            Res := Res & "null type ..";
         end if;
         if J /= Self.Index_Types'Last then
            Res := Res & " range <>, ";
         else
            Res := Res & " range <>) ";
         end if;
      end loop;
      Res := Res & "of " & Self.Component_Type.Get.Image;
      return To_String (Res);
   end Image;

   function Image (Self : Constrained_Array_Typ) return String is
      Res : Unbounded_String :=
        To_Unbounded_String (Typ (Self).Image & " : array (");
   begin
      for J in Self.Index_Types'Range loop
         if Self.Index_Types (J) /= SP.Null_Ref then
            Res := Res & Typ (Self.Index_Types (J).Get.Element.all).Image;
            if Self.Index_Constraints (J).Present
              and then Self.Index_Constraints (J)
                       .Discrete_Range.Low_Bound.Kind = Static
              and then Self.Index_Constraints (J)
                       .Discrete_Range.High_Bound.Kind = Static
            then
               Res :=
                 Res & " range "
                 & As_Discrete_Typ (Self.Index_Types (J)).Lit_Image
                     (Self.Index_Constraints (J).Discrete_Range.Low_Bound
                      .Int_Val)
                 & " .. "
                 & As_Discrete_Typ (Self.Index_Types (J)).Lit_Image
                     (Self.Index_Constraints (J).Discrete_Range.High_Bound
                      .Int_Val);
            end if;
         else
            Res := Res & "null type ..";
         end if;

         if J /= Self.Index_Types'Last then
            Res := Res & ", ";
         else
            Res := Res & ") ";
         end if;
      end loop;
      Res := Res & "of " & (if Self.Component_Type /= SP.Null_Ref
                            then Self.Component_Type.Get.Image
                            else "null type ..");
      return To_String (Res);
   end Image;

   procedure Callback_On_Constraint
     (Self     : Constrained_Array_Typ;
      Var_Name : Unbounded_Text_Type;
      Callback : access procedure
        (T          : Discrete_Typ'Class;
         Constraint : TGen.Types.Constraints.Index_Constraint));
   --  Call callback whenever an indexing constraint referencing Var_Name is
   --  found. If the variable constrains several indexing constraints, then
   --  callback will be called multiple times.

   ---------------------------
   -- Callback_On_Constrain --
   ---------------------------

   procedure Callback_On_Constraint
     (Self     : Constrained_Array_Typ;
      Var_Name : Unbounded_Text_Type;
      Callback : access procedure
        (T          : Discrete_Typ'Class;
         Constraint : TGen.Types.Constraints.Index_Constraint))
   is
   begin
      for I in Self.Index_Constraints'Range loop
         declare
            Index_Constraint : TGen.Types.Constraints.Index_Constraint
              renames Self.Index_Constraints (I);
            Index_Type_Classwide : constant Typ'Class :=
              Self.Index_Types (I).Get;
            Index_Type : constant Discrete_Typ'Class :=
              Discrete_Typ'Class (Index_Type_Classwide);
         begin
            if Index_Constraint.Present then
               declare
                  ILB : constant Discrete_Constraint_Value :=
                    Index_Constraint.Discrete_Range.Low_Bound;
                  IHB : constant Discrete_Constraint_Value :=
                    Index_Constraint.Discrete_Range.High_Bound;
               begin
                  if ILB.Kind = Discriminant and then ILB.Disc_Name = Var_Name
                  then
                     Callback (Index_Type, Index_Constraint);
                  end if;
                  if IHB.Kind = Discriminant and then IHB.Disc_Name = Var_Name
                  then
                     Callback (Index_Type, Index_Constraint);
                  end if;
               end;
            end if;
         end;
      end loop;
   end Callback_On_Constraint;

   --------------------------------
   -- Is_Constrained_By_Variable --
   --------------------------------

   procedure Is_Constrained_By_Variable
     (Self       : Constrained_Array_Typ;
      Var_Name   : Unbounded_Text_Type;
      Found      : out Boolean;
      Constraint : out TGen.Types.Constraints.Index_Constraint)
   is

      procedure Callback
        (T   : Discrete_Typ'Class;
         Cst : TGen.Types.Constraints.Index_Constraint);

      --------------
      -- Callback --
      --------------

      procedure Callback
        (T    : Discrete_Typ'Class with Unreferenced;
         Cst : TGen.Types.Constraints.Index_Constraint)
      is
      begin
         Found := True;
         Constraint := Cst;
      end Callback;

   begin
      Found := False;
      Constraint := (Present => False);
      Callback_On_Constraint (Self, Var_Name, Callback'Access);
   end Is_Constrained_By_Variable;

   type Data_Type is null record;

   type Size_Interval is record
      Min_Size, Max_Size : Natural;
   end record;

   type Size_Interval_Array is array (Natural range <>) of Size_Interval;
   type Nat_Array is array (Natural range <>) of Natural;

   generic
      type Element_Type is private;
      with function Generate_Value (Data : Data_Type) return Element_Type;
   package Array_Generation_Package is
      type Array_Type is array (Positive range <>) of Element_Type;

      type Array_Strategy_Type (Dimensions : Natural) is tagged record
         Size_Intervals : Size_Interval_Array (1 .. Dimensions);
         Average_Sizes  : Nat_Array (1 .. Dimensions);
      end record;

      function Array_Strategy
        (Size_Intervals : Size_Interval_Array) return Array_Strategy_Type;

      function Draw
        (Self : Array_Strategy_Type;
         Data : Data_Type;
         Dimension_Sizes : out Nat_Array) return Array_Type;

   end Array_Generation_Package;

   package body Array_Generation_Package is

      --------------------
      -- Array_Strategy --
      --------------------

      function Array_Strategy
        (Size_Intervals : Size_Interval_Array) return Array_Strategy_Type
      is
         Result : Array_Strategy_Type (Size_Intervals'Length);
         Average_Sizes : Nat_Array (Size_Intervals'Range);
      begin
         Result.Size_Intervals := Size_Intervals;
         for I in Size_Intervals'Range loop
            declare
               Min_Size : constant Natural := Size_Intervals (I).Min_Size;
               Max_Size : constant Natural := Size_Intervals (I).Max_Size;
            begin
               Average_Sizes (I) :=
                 Natural'Min (Natural'Max (Min_Size * 2, Min_Size + 5),
                              Min_Size + ((Max_Size - Min_Size) / 2));
            end;
         end loop;
         Result.Average_Sizes := Average_Sizes;
         return Result;
      end Array_Strategy;

      function Draw
        (Self : Array_Strategy_Type;
         Data : Data_Type;
         Dimension_Sizes : out Nat_Array) return Array_Type is
         Arr_Length : Natural := 0;
      begin

         --  Determine length of each dimension

         for I in 1 .. Self.Dimensions loop
            declare
               Elements    : Many_Type :=
                 Many
                   (Self.Size_Intervals (I).Min_Size,
                    Self.Size_Intervals (I).Max_Size,
                    Self.Average_Sizes (I));
            begin
               while Elements.More loop
                  null;
               end loop;
               Dimension_Sizes (I) := Elements.Count;
            end;
         end loop;

         --  Now that we have the length of each dimension, fill the array
         --  with elements. TODO: We should probably not generate a random
         --  value for each, but pick a background value and generate a handful
         --  of random values, as done in Hypothesis.

         if Dimension_Sizes'Length > 0 then
            Arr_Length := 1;
            for Dim_Size of Dimension_Sizes loop
               Arr_Length := Arr_Length * Dim_Size;
            end loop;
         end if;

         declare
            Arr : Array_Type (1 .. Arr_Length);
         begin
            for I in 1 .. Arr_Length loop
               Arr (I) := Generate_Value (Data);
            end loop;
            return Arr;
         end;
      end Draw;

   end Array_Generation_Package;

   generic
      type Element_Type is private;
      type Index_Type_1 is (<>);
      type Array_Type is array (Positive range <>) of Element_Type;
      type Reshaped_Array_Type is array (Index_Type_1 range <>)
        of Element_Type;

   function Reshape_1
     (Arr : Array_Type) return Reshaped_Array_Type with Unreferenced;

   ---------------
   -- Reshape_1 --
   ---------------

   function Reshape_1
     (Arr : Array_Type) return Reshaped_Array_Type
   is
      Res : Reshaped_Array_Type
        (Index_Type_1'First .. Index_Type_1'Val (Arr'Last));
      I : Index_Type_1 := Index_Type_1'First;
   begin
      for Orig in Arr'Range loop
         Res (I) := Arr (Orig);
         I := Index_Type_1'Succ (I);
      end loop;
      return Res;
   end Reshape_1;

   generic
      type Element_Type is private;
      type Index_Type_1 is (<>);
      type Index_Type_2 is (<>);
      type Array_Type is array (Positive) of Element_Type;
      type Reshaped_Array_Type is array
        (Index_Type_1 range <>, Index_Type_2 range <>)
        of Element_Type;

   function Reshape_2
     (Arr  : Array_Type;
      Dim1 : Positive) return Reshaped_Array_Type;

   pragma Unreferenced (Reshape_2);

   ---------------
   -- Reshape_2 --
   ---------------

   function Reshape_2
     (Arr  : Array_Type;
      Dim1 : Positive) return Reshaped_Array_Type
   is
      Res : Reshaped_Array_Type
        (Index_Type_1'First .. Index_Type_1'Val (Arr'Last / Dim1),
         Index_Type_2'First .. Index_Type_2'Val (Arr'Last mod Dim1));
      I : Index_Type_1 := Index_Type_1'First;
      J : Index_Type_2 := Index_Type_2'First;
   begin
      for Orig in Arr'Range loop
         Res (I, J) := Arr (Orig);
         J := Index_Type_2'Succ (J);
         if Orig mod Dim1 = 0 then
            I := Index_Type_1'Succ (I);
            J := Index_Type_2'First;
         end if;
      end loop;
      return Res;
   end Reshape_2;

   function Length
     (I_Constraint : TGen.Types.Constraints.Index_Constraint;
      I_Type       : SP.Ref;
      Disc_Context : Disc_Value_Map) return Big_Integer with Unreferenced;
   --  Returns the length of the array from its Index_Constraint

   type Index_Value is record
      Low_Bound, High_Bound : Big_Integer;
   end record;

   type Index_Values_Array is array (Positive range <>) of Index_Value;

   type Index_Strategies_Type is
      record
         Low_Bound_Strat, High_Bound_Strat : Static_Strategy_Acc;
      end record;

   type Index_Static_Strategy_Array is array (Positive range <>)
     of Index_Strategies_Type;

   type Array_Static_Strategy_Type (Num_Dims : Positive) is
     new Static_Strategy_Type with
      record
         T                         : SP.Ref;
         Generate_Element_Strategy : Static_Strategy_Acc;
         Generate_Index_Strategies : Index_Static_Strategy_Array
           (1 .. Num_Dims);
      end record;

   overriding function Generate_Static_Value
     (S            : in out Array_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class;

   function Generate_Static_Common
     (Self                   : Array_Typ'Class;
      Disc_Context           : Disc_Value_Map;
      Generate_Element_Strat : in out Static_Strategy_Type'Class;
      Generate_Index_Strat   : in out Index_Static_Strategy_Array)
      return Static_Value'Class;

   ------------
   -- Length --
   ------------

   function Length
     (I_Constraint : TGen.Types.Constraints.Index_Constraint;
      I_Type       : SP.Ref;
      Disc_Context : Disc_Value_Map) return Big_Integer

   is
      function Constraint_Value
        (Constraint : Discrete_Constraint_Value) return Big_Integer is
        (case Constraint.Kind is
            when Static =>
               Constraint.Int_Val,
            when Discriminant =>
               Disc_Context.Element (Constraint.Disc_Name),
            when others =>
               raise Program_Error with
                 "Dynamic constraint unsupported for static generation");

      use type Big_Integer;
      LB, HB : Big_Integer;
   begin
      if I_Constraint.Present then
         HB := Constraint_Value (I_Constraint.Discrete_Range.High_Bound);
         LB := Constraint_Value (I_Constraint.Discrete_Range.Low_Bound);
      else
         LB := As_Discrete_Typ (I_Type).Low_Bound;
         HB := As_Discrete_Typ (I_Type).High_Bound;
      end if;
      return HB - LB + 1;
   end Length;

   ----------------------------
   -- Generate_Static_Common --
   ----------------------------

   function Generate_Static_Common
     (Self                   : Array_Typ'Class;
      Disc_Context           : Disc_Value_Map;
      Generate_Element_Strat : in out Static_Strategy_Type'Class;
      Generate_Index_Strat   : in out Index_Static_Strategy_Array)
      return Static_Value'Class
   is
      use type Big_Int.Big_Integer;

      function Generate_Component_Wrapper
        (Data : Data_Type) return Unbounded_String;

      function Generate_Component_Wrapper
        (Data : Data_Type) return Unbounded_String is
         pragma Unreferenced (Data);
      begin
         return (+Generate_Element_Strat.Generate_Static_Value
                    (Disc_Context).To_String);
      end Generate_Component_Wrapper;

      Res : Unbounded_String;

      --  Let's use the somewhat generic Array_Strategy capabilities here,
      --  and pick a Min_Size of 0 and a Max_Size of (10 ** Nb_Dim). We will
      --  generate flattened arrays, that we will then reshape to fit the
      --  wanted dimensions.

      Data : Data_Type;

      package Array_Generation_Static is new Array_Generation_Package
        (Element_Type   => Unbounded_String,
         Generate_Value => Generate_Component_Wrapper);

      use Array_Generation_Static;

      Sizes : Size_Interval_Array (1 .. Self.Num_Dims);

      Index_Values : Index_Values_Array (1 .. Self.Num_Dims);

      Strat : Array_Strategy_Type (Self.Num_Dims);

      Dimension_Sizes : Nat_Array (1 .. Self.Num_Dims);

      function Pp_Index_Val
        (Value : Big_Integer; Current_Index : Positive) return String;

      procedure Pp_Arr
        (Arr           : Array_Type;
         Current_Index : in out Positive;
         Indexes       : Index_Values_Array);

      ------------------
      -- Pp_Index_Val --
      ------------------

      function Pp_Index_Val
        (Value : Big_Integer; Current_Index : Positive) return String
      is
      begin
         return As_Discrete_Typ
           (Self.Index_Types (Current_Index)).Lit_Image (Value);
      end Pp_Index_Val;

      ------------
      -- Pp_Arr --
      ------------

      procedure Pp_Arr
        (Arr           : Array_Type;
         Current_Index : in out Positive;
         Indexes       : Index_Values_Array)
      is
         use Big_Int;
      begin
         if Indexes'Length = 0 then
            raise Program_Error with "Array dimension can't be 0";
         end if;

         declare
            Index_Constraint : constant Index_Value :=
              Indexes (Indexes'First);
         begin
            Append (Res, "(");

            if Index_Constraint.High_Bound - Index_Constraint.Low_Bound < 0
            then
               --  1st special case: array is of size 0. We have to generate
               --  an empty aggregate.

               Append
                 (Res,
                  Pp_Index_Val (Index_Constraint.Low_Bound, Indexes'First)
                  & " .. "
                  & Pp_Index_Val (Index_Constraint.High_Bound, Indexes'First)
                  & " => <>");
            end if;

            for I in 0 .. Big_Int.To_Integer
              (Index_Constraint.High_Bound - Index_Constraint.Low_Bound)
            loop

               if Indexes'Length = 1 then
               --  We have reached leafs of a possible multi-dimensional array
               --  type. Time to print values \o/.

                  Append (Res,
                          Pp_Index_Val
                            (Value         => Index_Constraint.Low_Bound
                                              + Big_Int.To_Big_Integer (I),
                             Current_Index => Indexes'First)
                          & " => " & (+Arr (Current_Index)));
                  Current_Index := @ + 1;

               else
                  --  Otherwise, generate the nested array recursively

                  Pp_Arr
                    (Arr,
                     Current_Index,
                     Indexes (Indexes'First + 1 .. Indexes'Last));
               end if;

               Append (Res, ", ");
            end loop;
         end;

         Res := Remove_Trailing_Comma_And_Spaces (Res);
         Append (Res, ")");
      end Pp_Arr;

      procedure Pp_Arr_Wrapper
        (Arr     : Array_Type;
         Indexes : Index_Values_Array);

      --------------------
      -- Pp_Arr_Wrapper --
      --------------------

      procedure Pp_Arr_Wrapper
        (Arr     : Array_Type;
         Indexes : Index_Values_Array)
      is
         Ignore : Positive := 1;
      begin
         Pp_Arr (Arr, Ignore, Indexes);
      end Pp_Arr_Wrapper;

      Disc_Context_With_Low_Bound : Disc_Value_Map := Disc_Context.Copy;
   begin
      --  Draw indexes values. If the array is constrained, we already know
      --  them; it is the LB and the UB of each constraint. Otherwise, we need
      --  to draw them.

      for I in 1 .. Self.Num_Dims loop
         declare
            Index_Strat : constant Index_Strategies_Type :=
              Generate_Index_Strat (I);

            Low_Bound : constant Discrete_Static_Value :=
              Discrete_Static_Value
                (Index_Strat.Low_Bound_Strat.Generate_Static_Value
                   (Disc_Context));
            High_Bound : Discrete_Static_Value;
         begin
            Index_Values (I).Low_Bound := Low_Bound.Value;

            --  After having generated the low bound, we insert it in the
            --  discriminant map. This is a hack to generate index values for
            --  unconstrained array type. TODO: refactor.

            Disc_Context_With_Low_Bound.Include
              (Low_Bound_Disc_Name, Low_Bound.Value);

            High_Bound :=
              Discrete_Static_Value
                (Index_Strat.High_Bound_Strat.Generate_Static_Value
                   (Disc_Context_With_Low_Bound));

            Index_Values (I).High_Bound := High_Bound.Value;
         end;
      end loop;

      --  Now that we have generated both index values, let's compute the sizes

      Sizes :=
        [for I in 1 .. Self.Num_Dims =>
        (Min_Size =>
           Nat_Conversions.From_Big_Integer
             (Index_Values (I).High_Bound - Index_Values (I).Low_Bound + 1),
         Max_Size =>
           Nat_Conversions.From_Big_Integer
             (Index_Values (I).High_Bound - Index_Values (I).Low_Bound + 1)
        )];

      --  Ready to generate our array. Yay! \o/

      Strat := Array_Strategy (Sizes);

      declare
         Random_Arr : constant Array_Type :=
           Strat.Draw (Data, Dimension_Sizes);

      begin
      --  Let's pretty print it. TODO??? we should also print the generated
      --  index values, and not directly an array literal.

         Pp_Arr_Wrapper (Random_Arr, Index_Values);
      end;

      return Base_Static_Value'(Value => Res);
   end Generate_Static_Common;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Array_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      T : constant Array_Typ'Class := As_Array_Typ (S.T);
   begin
      return Generate_Static_Common
        (T,
         Disc_Context,
         S.Generate_Element_Strategy.all,
         S.Generate_Index_Strategies);
   end Generate_Static_Value;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Constrained_Array_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Strat            : Array_Static_Strategy_Type (Self.Num_Dims);
      Element_Strategy : constant Static_Strategy_Type'Class :=
        Self.Component_Type.Get.Generate_Static (Context);
   begin
      Strat.Generate_Element_Strategy :=
        new Static_Strategy_Type'Class'(Element_Strategy);
      SP.From_Element (Strat.T, Self'Unrestricted_Access);

      for I in Self.Index_Types'Range loop
         declare
            Index_Type : constant Discrete_Typ'Class :=
              As_Discrete_Typ (Self.Index_Types (I));

            Index_Constraint : constant Discrete_Range_Constraint :=
              (if Self.Index_Constraints (I).Present
               then Self.Index_Constraints (I).Discrete_Range
               else Discrete_Range_Constraint'
                 (Discrete_Constraint_Value'
                    (Kind => Static, Int_Val => Index_Type.Low_Bound),
                  Discrete_Constraint_Value'
                    (Kind => Static, Int_Val => Index_Type.High_Bound)));
         begin

            --  For the sake of consistency with unconstrained arrays, we will
            --  generate dumb strategies: they will just return the value of
            --  the constraint, be it a discriminant or a literal constraint.

            --  This one is for the lower bound of the array

            Strat.Generate_Index_Strategies (I).Low_Bound_Strat :=
              new Static_Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Identity_Constraint_Strategy
                       (Index_Constraint.Low_Bound));

            --  This one is for the upper bound

            Strat.Generate_Index_Strategies (I).High_Bound_Strat :=
              new Static_Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Identity_Constraint_Strategy
                       (Index_Constraint.High_Bound));
         end;
      end loop;
      return Strat;
   end Generate_Static;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self : Unconstrained_Array_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Strat : Array_Static_Strategy_Type (Self.Num_Dims);
      Element_Strategy : constant Static_Strategy_Type'Class :=
        Self.Component_Type.Get.Generate_Static (Context);
   begin
      Strat.Generate_Element_Strategy :=
        new Static_Strategy_Type'Class'(Element_Strategy);
      SP.From_Element (Strat.T, Self'Unrestricted_Access);

      for I in Self.Index_Types'Range loop

         Strat.Generate_Index_Strategies (I).Low_Bound_Strat :=
           new Static_Strategy_Type'Class'
             (Discrete_Typ'Class (Self.Index_Types (I).Unchecked_Get.all).
                Generate_Static (Context));

         declare
            --  HACK: we generate an artificial discrete constraint so that
            --  we can rely on our generic mechanism for generation of
            --  arrays: we assume that the generated value for the low bound
            --  of each index will be stored as Low_Bound_Disc_Name in
            --  the discriminant context. We will thus generate the
            --  High_Bound from that.

            Artificial_Constraint :
            constant TGen.Types.Constraints.Index_Constraint :=
              TGen.Types.Constraints.Index_Constraint'
                (Present => True,
                 Discrete_Range =>
                   Discrete_Range_Constraint'
                     (Low_Bound =>
                            Discrete_Constraint_Value'
                        (Kind => Discriminant,
                         Disc_Name => Low_Bound_Disc_Name),
                      High_Bound =>
                        Discrete_Constraint_Value'
                          (Kind => Discriminant,
                           Disc_Name => High_Bound_Disc_Name)));
         begin

            Strat.Generate_Index_Strategies (I).High_Bound_Strat :=
              new Static_Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Array_Index_Constraint_Strategy
                      (Var_Name   => High_Bound_Disc_Name,
                       Constraint => Artificial_Constraint,
                       Context    => Context));
         end;
      end loop;
      return Strat;
   end Generate_Static;

end TGen.Types.Array_Types;
