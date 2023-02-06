with Ada.Containers; use Ada.Containers;
with Ada.Directories;             use Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;   use Ada.Numerics.Float_Random;
with Ada.Streams;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces;                  use Interfaces;
with My_File;                     use My_File;
with My_File.TGen_Values; use My_File.TGen_Values;
with My_File.TGen_Support;        use My_File.TGen_Support;
with TGen.JSON;
with TGen.TGen_Support;           use TGen.TGen_Support;
with Show_Date;                   use Show_Date;
with Show_Date.TGen_Support;      use Show_Date.TGen_Support;

with GNAT.Traceback.Symbolic;
with TGen.Big_Int; use TGen.Big_Int;
with TGen.Big_Reals; use TGen.Big_Reals;
with TGen.Strings; use TGen.Strings;
with TGen.TGen_Values; use TGen.TGen_Values;
with TGen.Types; use TGen.Types;
with TGen.Types.Array_Types; use TGen.Types.Array_Types;
with TGen.Types.Constraints; use TGen.Types.Constraints;
with TGen.Types.Enum_Types; use TGen.Types.Enum_Types;
with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;
with TGen.Types.Int_Types; use TGen.Types.Int_Types;
with TGen.Types.Real_Types; use TGen.Types.Real_Types;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;

procedure Example_Introspection is

   procedure Assert (Cond : Boolean);

   procedure Assert (Cond : Boolean) is
   begin
      if not Cond then
         raise Program_Error;
      end if;
   end;


   --  Testing introspection over the following type:
   --  type T1 is range 1 - 2 ** 31 .. 2 ** 31 - 1;

   -------------
   -- Test_T1 --
   -------------

   procedure Test_T1 is
   begin
      Assert
        (my_file_t1_Typ.Range_Value.Min =
           my_file_t1_Typ_Conversions.To_Big_Integer (1 - 2 ** 31));
      Assert
        (my_file_t1_Typ.Range_Value.Max =
           my_file_t1_Typ_Conversions.To_Big_Integer (2 ** 31 - 1));
   end Test_T1;

   --  Testing introspection over the following type:
   --  type T2 is new Integer range 0 .. 100;

   -------------
   -- Test_T2 --
   -------------

   procedure Test_T2 is
   begin
      Assert
        (my_file_t2_Typ.Range_Value.Min =
           my_file_t2_Typ_Conversions.To_Big_Integer (0));
      Assert
        (my_file_t2_Typ.Range_Value.Max =
           my_file_t2_Typ_Conversions.To_Big_Integer (100));
   end Test_T2;


   --  Testing introspection over the following type:
   --  type T2 is mod 2**16;

   -------------
   -- Test_T3 --
   -------------

   procedure Test_T3 is
   begin
      Assert (To_Integer (my_file_t3_Typ.Mod_Value) = 2**16);
   end Test_T3;

   --  Testing introspection over the following type:
   --  type Constr_Array is array (Positive range 1 .. 10)
   --     of Integer range 0 .. Integer'Last;

   -----------------------
   -- Test_Constr_Array --
   -----------------------

   procedure Test_Constr_Array
   is
      Idx_Constraint : constant Index_Constraint :=
           my_file_constr_array_Typ.Index_Constraints (1);
      Comp_Type : constant Signed_Int_Typ'Class :=
         As_Signed_Int_Typ
             (As_Named_Typ
                (As_Anonymous_Typ (my_file_constr_array_Typ.Component_Type)));
   begin
      Assert (my_file_constr_array_Typ.Num_Dims = 1);
      Assert
        (Comp_Type.Range_Value.Min =
           standard_integer_Typ_Conversions.To_Big_Integer (0));
      Assert
        (Comp_Type.Range_Value.Max =
           standard_integer_Typ_Conversions.To_Big_Integer (Integer'Last));

      --  The bounds are contained in the index constraint and not in the
      --  index type, which is the base type.

      Assert
        (Idx_Constraint.Discrete_Range.Low_Bound.Int_Val =
           standard_positive_Typ_Conversions.To_Big_Integer (1));

      Assert
        (Idx_Constraint.Discrete_Range.High_Bound.Int_Val =
           standard_positive_Typ_Conversions.To_Big_Integer (10));
   end Test_Constr_Array;

   --  Testing introspection over the following type:
   --  type Matrix is array (Natural range <>, Character range <>) of Boolean;

   -----------------
   -- Test_Matrix --
   -----------------

   procedure Test_Matrix
   is
      First_Index_Type : constant Signed_Int_Typ'Class :=
        As_Signed_Int_Typ (my_file_matrix_Typ.Index_Types (1));
      Second_Index_Type : constant Char_Typ'Class :=
        As_Char_Typ (my_file_matrix_Typ.Index_Types (2));
      Comp_Type : constant Bool_Typ'Class :=
        As_Bool_Typ (my_file_matrix_Typ.Component_Type);
   begin
      Assert (my_file_matrix_Typ.Num_Dims = 2);
      Assert
        (First_Index_Type.Range_Value.Min =
           standard_natural_Typ_Conversions.To_Big_Integer (Natural'First));
      Assert
        (First_Index_Type.Range_Value.Max =
           standard_natural_Typ_Conversions.To_Big_Integer (Natural'Last));

      --  TODO: add test for character type second index

   end Test_Matrix;

   --  Testing introspection over the following type:
   --  type Fixed_1 is delta 0.0001 range -0.1 .. 0.1;

   ------------------
   -- Test_Fixed_1 --
   ------------------

   procedure Test_Fixed_1 is
   begin
      Assert
        (my_file_fixed_1_Typ.Delta_Value = From_Universal_Image ("0.0001"));
      Assert
        (my_file_fixed_1_Typ.Range_Value.Min = From_Universal_Image ("-0.1"));
      Assert
        (my_file_fixed_1_Typ.Range_Value.Max = From_Universal_Image ("0.1"));
   end Test_Fixed_1;

   --  Testing introspection over the following type:
   --  type Fixed_2 is delta 1_000_000.0 digits 16
   --     range -100_000_000.0 .. 100_000_000.0;

   ------------------
   -- Test_Fixed_2 --
   ------------------

   procedure Test_Fixed_2 is
   begin
      Assert
        (my_file_fixed_2_Typ.Delta_Value =
           From_Universal_Image ("1_000_000.0"));
      Assert
        (my_file_fixed_2_Typ.Range_Value.Min =
           From_Universal_Image ("-100_000_000.0"));
      Assert
        (my_file_fixed_2_Typ.Range_Value.Max =
           From_Universal_Image ("100_000_000.0"));
      Assert (my_file_fixed_2_Typ.Digits_Value = 16);
   end Test_Fixed_2;

   --  Testing type introspection for the following type:
   --  type Shape_Kind is (Point, Line, Circle, Square, Rectangle, Ellipse);

   ---------------------
   -- Test_Shape_Kind --
   ---------------------

   procedure Test_Shape_Kind is
      use Enum_Literal_Maps;
   begin
      for Cur in my_file_shape_kind_Typ.Literals.Iterate loop
         Assert
           (Shape_Kind'Val (Integer'Value (To_String (Key (Cur)))) =
              Shape_Kind'Value (+Element (Cur)));
      end loop;
   end Test_Shape_Kind;

   --  Testing type introspection for the following type:
   --  subtype Name_Size_Ty is Natural range 0 .. 30;

   -----------------------
   -- Test_Name_Size_Ty --
   -----------------------

   procedure Test_Name_Size_Ty is
   begin
      Assert
        (my_file_name_size_ty_Typ.Range_Value.Min =
           my_file_name_size_ty_Typ_Conversions.To_Big_Integer (0));
      Assert
        (my_file_name_size_ty_Typ.Range_Value.Max =
           my_file_name_size_ty_Typ_Conversions.To_Big_Integer (30));
   end Test_Name_Size_Ty;

   --  Testing type introspection for the following type:
   --  type Shape (K : Shape_Kind := Line; Name_Size : Name_Size_Ty := 30) is record
   --     Name  : String (1 .. Name_Size);
   --     X, Y  : Integer range -100 .. Id (100);
   --     case K is
   --        when Line =>
   --           X_2, Y_2 : Integer range Id (-100) .. 100;
   --        when Circle | Ellipse =>
   --           Radius   : Positive;
   --           case K is
   --              when Ellipse =>
   --                 Radius_2 : Positive;
   --              when others =>
   --                 null;
   --           end case;
   --        when Square .. Rectangle =>
   --           Side     : Positive;
   --           case K is
   --              when Rectangle =>
   --                 Side_2 : Positive;
   --              when others =>
   --                 null;
   --           end case;
   --        when others =>
   --           null;
   --     end case;
   --  end record;

   procedure Test_Shape
   is
      Rec_Typ : constant Discriminated_Record_Typ'Class := my_file_shape_typ;
   begin
      Assert (Rec_Typ.Mutable);

      --  Check discriminant types

      for Cur in Rec_Typ.Discriminant_Types.Iterate loop
         declare
            use Component_Maps;
            Disc_Name : constant String := +Key (Cur);
            Disc_Type : constant SP.Ref := Element (Cur);
         begin
            if Disc_Name = "K" then
               Assert (Disc_Type = my_file_shape_kind_Typ_Ref);
            elsif Disc_Name = "Name_Size" then
               Assert (Disc_Type = my_file_name_size_ty_Typ_Ref);
            else
               raise Program_Error;
            end if;
         end;
      end loop;

      --  Check the components types

      for Cur in Rec_Typ.Component_Types.Iterate loop
         declare
            use Component_Maps;
            Comp_Name : constant String := +Key (Cur);
         begin
            if Comp_Name = "X" or else Comp_Name = "Y" then
               declare
                  Comp_Type : constant Signed_Int_Typ'Class :=
                    As_Signed_Int_Typ
                      (As_Anonymous_Typ
                         (Element (Cur)).As_Named_Typ);
               begin
                  Assert
                    (Comp_Type.Range_Value.Min =
                       From_Universal_Image ("-100"));
                  Assert
                    (Comp_Type.Range_Value.Max =
                       From_Universal_Image (Id (100)'Image));
               end;
            elsif Comp_Name = "Name" then
               declare
                  Comp_Type : constant Constrained_Array_Typ'Class :=
                    As_Constrained_Array_Typ
                      (As_Anonymous_Typ
                         (Element (Cur)).As_Named_Typ);
                  Constr : constant Index_Constraint :=
                    Comp_Type.Index_Constraints (1);
               begin
                  Assert
                    (+Constr.Discrete_Range.Low_Bound.Int_Val =
                       From_Universal_Image ("1"));
                  Assert
                    (+Constr.Discrete_Range.High_Bound.Disc_Name =
                       "Name_Size");
               end;
            else
               raise Program_Error;
            end if;
         end;
      end loop;

      --  Check variants

      Assert (Rec_Typ.Variant /= null);

      --  Top level variant dispatches over K value

      Assert (+Rec_Typ.Variant.Discr_Name = "K");

      --  Check the variant choices

      Assert (Rec_Typ.Variant.Variant_Choices.Length = 4);

      declare
         Variant_Choice_Index : Positive := 1;
         Expected_Alt_Set : Alternatives_Sets.Set;
      begin
         for Choice of Rec_Typ.Variant.Variant_Choices loop
            if Variant_Choice_Index = 1 then
               --  Check first variant choice:
               --        when Line =>
               --           X_2, Y_2 : Integer range Id (-100) .. 100;

               --  Check alternative

               declare
                  Expected_Alt_Set : Alternatives_Set;
               begin
                  Expected_Alt_Set.Insert
                    (Int_Range'
                       (Min =>
                            From_Universal_Image
                          (Shape_Kind'Pos (Line)'Image),
                        Max =>
                          From_Universal_Image
                            (Shape_Kind'Pos (Line)'Image)));

                  Assert
                    (Choice.Alt_Set.Equivalent_Sets (Expected_Alt_Set));
               end;

               --  Check the components for this variant choice

               for Cur in Choice.Components.Iterate loop
                  declare
                     use Component_Maps;
                     Comp_Name : constant String := +Key (Cur);
                  begin
                     if Comp_Name = "X_2" or else Comp_Name = "Y_2" then
                        declare
                           Comp_Type : constant Signed_Int_Typ'Class :=
                             As_Signed_Int_Typ
                               (As_Anonymous_Typ
                                  (Element (Cur)).As_Named_Typ);
                        begin
                           Assert
                             (Comp_Type.Range_Value.Min =
                                From_Universal_Image (Id (-100)'Image));
                           Assert
                             (Comp_Type.Range_Value.Max =
                                From_Universal_Image (Id (100)'Image));
                        end;
                     else
                        raise Program_Error;
                     end if;
                  end;
               end loop;

               --  No nested variant

               Assert (Choice.Variant = null);

            elsif Variant_Choice_Index = 2 then
               --  Check second variant choice:
               --        when Circle | Ellipse =>
               --           Radius   : Positive;
               --           case K is
               --              when Ellipse =>
               --                 Radius_2 : Positive;
               --              when others =>
               --                 null;
               --           end case;

               --  Check alternatives

               declare
                  Expected_Alt_Set : Alternatives_Set;
               begin
                  Expected_Alt_Set.Insert
                    (Int_Range'
                       (Min =>
                            From_Universal_Image
                          (Shape_Kind'Pos (Circle)'Image),
                        Max =>
                          From_Universal_Image
                            (Shape_Kind'Pos (Circle)'Image)));
                  Expected_Alt_Set.Insert
                    (Int_Range'
                       (Min =>
                            From_Universal_Image
                          (Shape_Kind'Pos (Ellipse)'Image),
                        Max =>
                          From_Universal_Image
                            (Shape_Kind'Pos (Ellipse)'Image)));

                  Assert
                    (Choice.Alt_Set.Equivalent_Sets (Expected_Alt_Set));
               end;

               --  Check the components for this variant choice

               for Cur in Choice.Components.Iterate loop
                  declare
                     use Component_Maps;
                     Comp_Name : constant String := +Key (Cur);
                  begin
                     if Comp_Name = "Radius" then
                        Assert (Element (Cur) = standard_positive_Typ_Ref);
                     else
                        raise Program_Error;
                     end if;
                  end;
               end loop;

               --  TODO: check nested variant

               Assert (Choice.Variant /= null);


            --  TODO: check other variant choices

            end if;
            Variant_Choice_Index := Variant_Choice_Index + 1;
         end loop;

         Assert (Variant_Choice_Index = 5);
      end;
   end Test_Shape;

   --  Testing type introspection for the following type:
   --  type Shape_Array is array (T2'Base range <>) of Shape;

   procedure Test_Shape_Array is
   begin
      --  TODO
      null;
   end Test_Shape_Array;

   --  Testing type introspection for the following type:
   --  type Small_Shape_Array (L : T2 := 0) is record
   --     Content : Shape_Array (1 .. L);
   --  end record;

   procedure Test_Small_Shape_Array is
   begin
      --  TODO
      null;
   end Test_Small_Shape_Array;

   --  Testing type introspection for the following type:
   --  type R is record
   --     F1 : T1;
   --     F2 : T2;
   --     F3 : T3;
   --     F4 : Boolean;
   --     F5 : Float;
   --     F6 : Long_Float;
   --     F7 : Long_Float; -- change back to long_long_float?
   --     F8 : Constr_Array;
   --     F9 : Character;
   --     G1 : Fixed_1;
   --     G2 : Fixed_2;
   --     G3 : Matrix (0 .. 9, 'A' .. 'I');
   --     G4 : Shape (Ellipse, 10);
   --     G5 : Shape;
   --     G6 : Small_Shape_Array;
   --  end record;
   --  TODO

   --  Testing type introspection for the following type:
   --  type R2 is record
   --     F1, F2 : Boolean := False;
   --  end record with Predicate => F1 or F2;
   --  TODO

begin
   Test_T1;
   Test_T2;
   Test_T3;
   Test_Constr_Array;
   Test_Matrix;
   Test_Fixed_1;
   Test_Fixed_2;
   Test_Shape_Kind;
   Test_Name_Size_Ty;
   Test_Shape;
exception
   when E : others =>
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end;
