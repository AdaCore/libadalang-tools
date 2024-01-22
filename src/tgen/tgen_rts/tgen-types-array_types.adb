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
with TGen.Types.Int_Types;

package body TGen.Types.Array_Types is

   use TGen.Types.SP;

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   function Get_Diagnostics
     (Self   : Array_Typ;
      Prefix : String := "") return String_Vector
   is
      Res : String_Vector;
   begin
      for Idx_Typ of Self.Index_Types loop
         Res.Append_Vector (Idx_Typ.Get.Get_Diagnostics (Prefix));
      end loop;
      Res.Append_Vector (Self.Component_Type.Get.Get_Diagnostics (Prefix));
      return Res;
   end Get_Diagnostics;

   -----------
   -- Image --
   -----------

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
      Var_Name : Unbounded_String;
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
      Var_Name : Unbounded_String;
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
      Var_Name   : Unbounded_String;
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

   generic
      type Element_Type is private;
      with function Generate_Value (Data : Data_Type) return Element_Type;
   package Array_Generation_Package is
      type Generic_Array_Type is array (Positive range <>) of Element_Type;

      type Generic_Array_Strategy_Type (Dimensions : Natural) is tagged record
         Size_Intervals : Size_Interval_Array (1 .. Dimensions);
         Average_Sizes  : Nat_Array (1 .. Dimensions);
      end record;

      function Generic_Array_Strategy
        (Size_Intervals : Size_Interval_Array)
         return Generic_Array_Strategy_Type;

      function Draw
        (Self            : Generic_Array_Strategy_Type;
         Data            : Data_Type;
         Dimension_Sizes : out Nat_Array) return Generic_Array_Type;

   end Array_Generation_Package;

   package body Array_Generation_Package is

      ----------------------------
      -- Generic_Array_Strategy --
      ----------------------------

      function Generic_Array_Strategy
        (Size_Intervals : Size_Interval_Array)
         return Generic_Array_Strategy_Type
      is
         Result        : Generic_Array_Strategy_Type (Size_Intervals'Length);
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
      end Generic_Array_Strategy;

      function Draw
        (Self : Generic_Array_Strategy_Type;
         Data : Data_Type;
         Dimension_Sizes : out Nat_Array) return Generic_Array_Type is
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
            Arr : Generic_Array_Type (1 .. Arr_Length);
         begin
            for I in 1 .. Arr_Length loop
               Arr (I) := Generate_Value (Data);
            end loop;
            return Arr;
         end;
      end Draw;

   end Array_Generation_Package;

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
         Low_Bound_Strat, High_Bound_Strat : Strategy_Acc;
      end record;

   type Index_Strategy_Array is array (Positive range <>)
     of Index_Strategies_Type;

   type Array_Strategy_Type (Num_Dims : Positive) is
     new Random_Strategy_Type with
      record
         T                         : SP.Ref;
         Generate_Element_Strategy : Strategy_Acc;
         Generate_Index_Strategies : Index_Strategy_Array
           (1 .. Num_Dims);
      end record;

   overriding function Generate
     (S            : in out Array_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   function Generate_Static_Common
     (Self                   : Array_Typ'Class;
      Disc_Context           : Disc_Value_Map;
      Generate_Element_Strat : in out Strategy_Type'Class;
      Generate_Index_Strat   : in out Index_Strategy_Array)
      return JSON_Value;

   function Generate_Enum_Array
     (Arr_T       : Array_Typ'Class;
      Comp_Strat  : Enum_Strategy_Type_Acc;
      Bounds      : Index_Values_Array;
      Disc_Values : Disc_Value_Map) return JSON_Value;
   --  Generate an array value from the strategy defined in Comp_Strat, with as
   --  many elements as defined by Bounds for each dimension. This does not
   --  create a "dimensions" field in the JSON result, it needs to be added by
   --  the caller if the type for which a value is being generated is
   --  unconstrained. The values in the returned array will be already encoded.

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
               Disc_Context.Element (Constraint.Disc_Name).Get,
            when others =>
               raise Program_Error with
                 "Dynamic constraint unsupported for static generation");

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
      Generate_Element_Strat : in out Strategy_Type'Class;
      Generate_Index_Strat   : in out Index_Strategy_Array) return JSON_Value
   is
      pragma Warnings (Off);
      function Generate_Component_Wrapper
        (Data : Data_Type) return JSON_Value;
      pragma Warnings (On);

      function Generate_Component_Wrapper
        (Data : Data_Type) return JSON_Value
      is
         pragma Unreferenced (Data);
      begin
         return Generate_Element_Strat.Generate (Disc_Context);
      end Generate_Component_Wrapper;

      --  Let's use the somewhat generic Array_Strategy capabilities here,
      --  and pick a Min_Size of 0 and a Max_Size of (10 ** Nb_Dim). We will
      --  generate flattened arrays, and put the dimensions in the dimension
      --  field of the generated JSON.

      Data : Data_Type;

      package Array_Generation_Static is new Array_Generation_Package
        (Element_Type   => JSON_Value,
         Generate_Value => Generate_Component_Wrapper);

      use Array_Generation_Static;

      Sizes : Size_Interval_Array (1 .. Self.Num_Dims);

      Index_Values : Index_Values_Array (1 .. Self.Num_Dims);
      --  Generated index values

      Strat : Generic_Array_Strategy_Type (Self.Num_Dims);
      --  Strategy to generate element. This is simply a wrapper around
      --  Generate_Element_Strat.Generate.

      Dimension_Sizes : Nat_Array (1 .. Self.Num_Dims);

      Disc_Context_With_Low_Bound : Disc_Value_Map := Disc_Context.Copy;
      --  Discriminant context enhanced with the generated lower bound

      Result : constant JSON_Value := Create_Object;
      --  Generated array in our JSON format

   begin
      --  Draw indexes values. If the array is constrained, we already know
      --  them; it is the LB and the UB of each constraint. Otherwise, we need
      --  to draw them.

      for I in 1 .. Self.Num_Dims loop
         declare
            Index_Strat : constant Index_Strategies_Type :=
              Generate_Index_Strat (I);

            Low_Bound  : constant Big_Integer :=
              Index_Strat.Low_Bound_Strat.Generate (Disc_Context).Get;
            High_Bound : Big_Integer;
         begin
            Index_Values (I).Low_Bound := Low_Bound;

            --  After having generated the low bound, we insert it in the
            --  discriminant map. This is a hack to generate index values for
            --  unconstrained array types. TODO: refactor.

            Disc_Context_With_Low_Bound.Include
              (Low_Bound_Disc_Name, Create (Low_Bound));
            High_Bound := Index_Strat.High_Bound_Strat.Generate
              (Disc_Context_With_Low_Bound).Get;
            Index_Values (I).High_Bound := High_Bound;
         end;
      end loop;

      --  Add the size of each array dimension as metadata information

      declare
         Sizes_JSON : JSON_Array;
      begin
         for Index_Value of Index_Values loop
            Append
              (Sizes_JSON,
               Create
                 (To_String
                      (Index_Value.High_Bound - Index_Value.Low_Bound + 1)));
         end loop;
         Set_Field (Result, "sizes", Sizes_JSON);
      end;

      --  Add them to the dimensions field of the JSON result

      if Self in Unconstrained_Array_Typ'Class then
         declare
            Dimensions_JSON : JSON_Array;
            Dimension_JSON : JSON_Value;
         begin
            for Index_Value of Index_Values loop
               Dimension_JSON := Create_Object;
               Set_Field (Dimension_JSON, "First", Index_Value.Low_Bound);
               Set_Field (Dimension_JSON, "Last", Index_Value.High_Bound);
               Append (Dimensions_JSON, Dimension_JSON);
            end loop;
            Set_Field (Result, "dimensions", Dimensions_JSON);
         end;
      end if;

      --  Now that we have generated both index values, let's compute the
      --  sizes.

      Sizes :=
        [for I in 1 .. Self.Num_Dims =>
        (Min_Size =>
           Nat_Conversions.From_Big_Integer
             (Index_Values (I).High_Bound - Index_Values (I).Low_Bound + 1),
         Max_Size =>
           Nat_Conversions.From_Big_Integer
             (Index_Values (I).High_Bound - Index_Values (I).Low_Bound + 1)
        )];

      Strat := Generic_Array_Strategy (Sizes);

      declare
         Random_Arr : constant Generic_Array_Type :=
           Strat.Draw (Data, Dimension_Sizes);
         JSON_Arr   : TGen.JSON.JSON_Array;
      begin
         for Elem of Random_Arr loop
            Append (JSON_Arr, Self.Component_Type.Get.Encode (Elem));
         end loop;
         Set_Field (Result, "array",
                    Create (JSON_Arr));
      end;
      return Result;
   end Generate_Static_Common;

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Array_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
   begin
      return Generate_Static_Common
        (As_Array_Typ (S.T),
         Disc_Context,
         S.Generate_Element_Strategy.all,
         S.Generate_Index_Strategies);
   end Generate;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self : Constrained_Array_Typ) return Strategy_Type'Class
   is
      Strat            : Array_Strategy_Type (Self.Num_Dims);
      Element_Strategy : constant Strategy_Type'Class :=
        Self.Component_Type.Get.Default_Strategy;
   begin
      Strat.Generate_Element_Strategy :=
        new Strategy_Type'Class'(Element_Strategy);
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
              new Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Identity_Constraint_Strategy
                       (Index_Constraint.Low_Bound));

            --  This one is for the upper bound

            Strat.Generate_Index_Strategies (I).High_Bound_Strat :=
              new Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Identity_Constraint_Strategy
                       (Index_Constraint.High_Bound));
         end;
      end loop;
      return Strat;
   end Default_Strategy;

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy
     (Self : Unconstrained_Array_Typ) return Strategy_Type'Class
   is
      Strat : Array_Strategy_Type (Self.Num_Dims);
      Element_Strategy : constant Strategy_Type'Class :=
        Self.Component_Type.Get.Default_Strategy;
   begin
      Strat.Generate_Element_Strategy :=
        new Strategy_Type'Class'(Element_Strategy);
      SP.From_Element (Strat.T, Self'Unrestricted_Access);

      for I in Self.Index_Types'Range loop

         Strat.Generate_Index_Strategies (I).Low_Bound_Strat :=
           new Strategy_Type'Class'
             (Discrete_Typ'Class (Self.Index_Types (I).Unchecked_Get.all).
                Default_Strategy);

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
              new Strategy_Type'Class'
                (Discrete_Typ'Class
                   (Self.Index_Types (I).Unchecked_Get.all).
                     Generate_Array_Index_Constraint_Strategy
                      (Var_Name   => High_Bound_Disc_Name,
                       Constraint => Artificial_Constraint));
         end;
      end loop;
      return Strat;
   end Default_Strategy;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : Unconstrained_Array_Typ; Val : JSON_Value) return JSON_Value
   is
      Result : constant JSON_Value := Val;
   begin
      --  Only encode the dimensions. The component values are already
      --  encoded.

      if Val.Has_Field ("dimensions") then
         declare
            Dimensions         : constant JSON_Array :=
              Val.Get ("dimensions");
            Encoded_Dimensions : JSON_Array;
            Dimension_Index    : Positive := 1;
         begin
            for T of Self.Index_Types loop
               declare
                  Dimension         : constant JSON_Value :=
                    Array_Element (Dimensions, Dimension_Index);
                  Encoded_Dimension : constant JSON_Value := Create_Object;
               begin
                  Set_Field
                    (Encoded_Dimension, "First",
                     T.Get.Encode (Dimension.Get ("First")));
                  Set_Field
                    (Encoded_Dimension, "Last",
                     T.Get.Encode (Dimension.Get ("Last")));
                  Dimension_Index := Array_Next (Dimensions, Dimension_Index);
                  Append (Encoded_Dimensions, Encoded_Dimension);
               end;
            end loop;
            Set_Field (Result, "dimensions", Encoded_Dimensions);
         end;
      end if;
      return Result;
   end Encode;

   ----------
   -- Init --
   ----------

   procedure Init (S : in out Unconst_Array_Enum_Strat) is
   begin
      S.Current_Size := 1;
      Init (Constr_Array_Enum_Strat (S));
   end Init;

   --------------
   -- Has_Next --
   --------------

   overriding function Has_Next
     (S : Unconst_Array_Enum_Strat) return Boolean is
     (S.Current_Size < S.Num_Sizes);

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Unconst_Array_Enum_Strat;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Arr_T  : Unconstrained_Array_Typ renames
        Unconstrained_Array_Typ (S.Arr_T.Unchecked_Get.all);
      Bounds : Index_Values_Array (1 .. Arr_T.Num_Dims);
      Len    : Natural renames S.Sizes (S.Current_Size);
      Res    : JSON_Value;
   begin
      --  Generate arrays where each dimension is of size Len, or the whole
      --  range of the index type if it is smaller than what is requested, with
      --  a low bound equal to the first element of the index type.

      --  First compute the bounds

      for I in 1 .. Bounds'Length loop
         declare
            LB : constant Big_Integer :=
              As_Discrete_Typ (Arr_T.Index_Types (I)).Low_Bound;
            HB : constant Big_Integer :=
              As_Discrete_Typ (Arr_T.Index_Types (I)).High_Bound;

            Is_Singleton_Index_Type : constant Boolean :=
              LB + To_Big_Integer (1) > HB;
            --  Whether the index type consists of a single element

            Is_Signed_Int_Typ : constant Boolean :=
              As_Discrete_Typ (Arr_T.Index_Types (I)) in
                TGen.Types.Int_Types.Signed_Int_Typ'Class;
            --  Whether this is a modular or enum type

         begin
            --  Special-case the zero-length array case. Do not generate a
            --  zero-length array if the index type is not a signed int type
            --  as we have no base type to declare to instantiate an empty
            --  array index constraint.

            if Is_Singleton_Index_Type and then not Is_Signed_Int_Typ then
               Len := 1;
            end if;

            if Len = 0 then
               if Is_Signed_Int_Typ then

                  --  Use the (1 .. 0) to generate an empty array when the
                  --  index type is a signed int type as this always in the
                  --  base type range.

                  Bounds (I).Low_Bound := To_Big_Integer (1);
                  Bounds (I).High_Bound := To_Big_Integer (0);
               else
                  Bounds (I).Low_Bound := LB + To_Big_Integer (1);
                  Bounds (I).High_Bound := LB;
               end if;
            else
               Bounds (I).Low_Bound := LB;
               Bounds (I).High_Bound := Min
                 (LB + To_Big_Integer (Len - 1), HB);
            end if;
         end;
      end loop;

      --  Then generate the values

      Res := Generate_Enum_Array (Arr_T, S.Comp_Strat, Bounds, Disc_Context);

      --  Finally generate the "dimension" part of the unconstrained array from
      --  the bounds.

      declare
         Dimension_JSON  : JSON_Value;
         Dimensions_JSON : JSON_Array;
      begin
         for Bound of Bounds loop
            Dimension_JSON := Create_Object;
            Dimension_JSON.Set_Field ("First", Create (Bound.Low_Bound));
            Dimension_JSON.Set_Field ("Last", Create (Bound.High_Bound));
            Append (Dimensions_JSON, Dimension_JSON);
         end loop;
         Res.Set_Field ("dimensions", Dimensions_JSON);
      end;

      S.Current_Size := S.Current_Size + 1;
      return Res;
   end Generate;

   -----------------------------------
   -- Make_Unconst_Array_Enum_Strat --
   -----------------------------------

   function Make_Unconst_Array_Enum_Strat
     (Arr_T : Unconstrained_Array_Typ;
      Sizes : Nat_Array) return Unconst_Array_Enum_Strat'Class
   is
   begin
      return Res : Unconst_Array_Enum_Strat (Num_Sizes => Sizes'Length) do
         Res.Current_Size := 1;
         Res.Arr_T.From_Element (Arr_T'Unrestricted_Access);
         Res.Comp_Strat := new Enum_Strategy_Type'Class'
           (Enum_Strategy_Type'Class
              (Arr_T.Component_Type.Get.Default_Enum_Strategy));
         Res.Has_Generated := False;
         Res.Sizes := Sizes;
      end return;
   end Make_Unconst_Array_Enum_Strat;

   overriding function Default_Enum_Strategy
     (Self : Unconstrained_Array_Typ) return Enum_Strategy_Type'Class is
     (Self.Make_Unconst_Array_Enum_Strat ([0, 1, 10, 1000]));

   -------------------------
   -- Generate_Enum_Array --
   -------------------------

   function Generate_Enum_Array
     (Arr_T       : Array_Typ'Class;
      Comp_Strat  : Enum_Strategy_Type_Acc;
      Bounds      : Index_Values_Array;
      Disc_Values : Disc_Value_Map) return JSON_Value
   is
      Res        : constant JSON_Value := Create_Object;
      Big_1      : constant Big_Integer := Big_Int.To_Big_Integer (1);
      Total_Size : Big_Integer := Big_1;
      Values     : JSON_Array;
      Idx        : Positive := 1;
      --  Idx points to the next item we need to copy in case we have no more
      --  values to generate.

   begin
      --  Start by computing the actual dimensions of the flattened array

      for Bound of Bounds loop
         Total_Size :=
           Total_Size * (Bound.High_Bound - Bound.Low_Bound + Big_1);
      end loop;

      --  Then generate as many values as required. As in the random
      --  generation, pre-encode the values as they can't be used as a
      --  parameter to generate something else.

      Comp_Strat.Init;

      for I in 1 .. To_Integer (Total_Size) loop
         if Comp_Strat.Has_Next then
            Append
              (Values,
               Arr_T.Component_Type.Get.Encode
                 (Comp_Strat.Generate (Disc_Values)));
         else
            Append (Values, Get (Values, Idx));
            Idx := Idx + 1;
         end if;
      end loop;

      --  Encode the sizes as well

      declare
         Sizes_JSON : JSON_Array;
      begin
         for Index_Value of Bounds loop
            Append
              (Sizes_JSON,
               Create
                 (To_String
                      (Index_Value.High_Bound - Index_Value.Low_Bound + 1)));
         end loop;
         Set_Field (Res, "sizes", Sizes_JSON);
      end;

      Res.Set_Field ("array", Values);
      return Res;
   end Generate_Enum_Array;

   ----------
   -- Init --
   ----------

   procedure Init (S : in out Constr_Array_Enum_Strat) is
   begin
      S.Has_Generated := False;
   end Init;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (S : Constr_Array_Enum_Strat) return Boolean is
     (not S.Has_Generated);

   --------------
   -- Generate --
   --------------

   overriding function Generate
     (S            : in out Constr_Array_Enum_Strat;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Arr_T  : Constrained_Array_Typ'Class renames
        Constrained_Array_Typ'Class (S.Arr_T.Unchecked_Get.all);
      Bounds : Index_Values_Array (1 ..  Arr_T.Num_Dims);
   begin
      S.Has_Generated := True;
      for Dim in 1 .. Arr_T.Num_Dims loop
         if not Arr_T.Index_Constraints (Dim).Present then
            Bounds (Dim).Low_Bound :=
              As_Discrete_Typ (Arr_T.Index_Types (Dim)).Low_Bound;
                        Bounds (Dim).High_Bound :=
              As_Discrete_Typ (Arr_T.Index_Types (Dim)).High_Bound;
         else
            Bounds (Dim).Low_Bound :=
              Value
                (Arr_T.Index_Constraints (Dim).Discrete_Range.Low_Bound,
                 Disc_Context);
            Bounds (Dim).High_Bound :=
              Value
                (Arr_T.Index_Constraints (Dim).Discrete_Range.High_Bound,
                 Disc_Context);
         end if;
      end loop;
      return Generate_Enum_Array (Arr_T, S.Comp_Strat, Bounds, Disc_Context);
   end Generate;

   ---------------------------
   -- Default_Enum_Strategy --
   ---------------------------

   function Default_Enum_Strategy
     (Self : Constrained_Array_Typ) return Enum_Strategy_Type'Class
   is
      Res : Constr_Array_Enum_Strat;
   begin
      Res.Arr_T.From_Element (Self'Unrestricted_Access);
      Res.Comp_Strat := new Enum_Strategy_Type'Class'
        (Self.Component_Type.Get.Default_Enum_Strategy);
      Res.Has_Generated := False;
      return Res;
   end Default_Enum_Strategy;

end TGen.Types.Array_Types;
