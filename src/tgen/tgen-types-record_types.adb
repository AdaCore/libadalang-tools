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

with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Langkit_Support.Text;

with GNATCOLL.GMP.Integers;

with TGen.Context; use TGen.Context;
with TGen.Gen_Strategies_Utils; use TGen.Gen_Strategies_Utils;
with TGen.Random;

package body TGen.Types.Record_Types is

   package Text renames Langkit_Support.Text;

   package Big_Int renames Ada.Numerics.Big_Numbers.Big_Integers;

   LF : constant String := (1 => ASCII.LF);

   Pad : constant Unbounded_String := 3 * ' ';

   Debug_Variant_Set : constant Boolean := False;
   --  Display the alternatives as intervals instead of source text. For
   --  debug purposes

   function Check_Others
     (Designator : LAL.Others_Designator;
      Val        : Big_Integer) return Boolean;
   --  Check if Val Satisfies this "others" choice. This is done by
   --  checking that Val staisfies all the other choices of the variant.

   function PP_Variant
   (Var     : Variant_Part_Acc;
    Padding : Natural := 0) return Unbounded_String;

   procedure Fill_Components
     (Self        : Variant_Part;
      Constraints : Disc_Value_Map;
      Res         : in out Component_Maps.Map);
   --  Fill Res with the list of components in Self that are present given
   --  a map of discriminant constraints.

   procedure Append_Value_For_Component
     (Component_Name : LAL.Defining_Name;
      Value          : String;
      Rec            : in out Unbounded_String);

   -----------
   -- Image --
   -----------

   function Image (Self : Record_Typ) return String is
      (Image_Internal (Self, 0));

   function Image_Internal
     (Self : Record_Typ; Padding : Natural := 0) return String
   is
      use Component_Maps;
      Str : Unbounded_String := To_Unbounded_String (Typ (Self).Image);
      Current_Component : Cursor;
   begin
      if Self.Component_Types.Is_Empty then
         Str := Str & ": null record";
      else
         Str               := Str & ": record" & LF;
         Current_Component := Self.Component_Types.First;
         while Has_Element (Current_Component) loop
            Str := Str & (Padding + 1) * Pad
                   & Text.Image (Key (Current_Component).Text) & " : ";
            if Element (Current_Component).Get.Kind in Record_Typ_Range then
               Str := Str & String'(As_Record_Typ (Element (Current_Component))
                            .Image_Internal (Padding + 1)) & LF;
            else
               Str := Str & Element (Current_Component).Get.Image & LF;
            end if;
            Next (Current_Component);
         end loop;
         Str := Str & "end record";
      end if;
      return To_String (Str);
   end Image_Internal;

   --------------------------------
   -- Append_Value_For_Component --
   --------------------------------

   procedure Append_Value_For_Component
     (Component_Name : LAL.Defining_Name;
      Value          : String;
      Rec            : in out Unbounded_String) is
   begin
      Append (Rec, +Component_Name.F_Name.Text);
      Append (Rec, " => ");
      Append (Rec, Value);
      Append (Rec, ", ");
   end Append_Value_For_Component;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self         : Record_Typ;
      Disc_Context : Disc_Value_Map) return String
   is
      use Component_Maps;
      Result : Unbounded_String;
   begin
      for C in Self.Component_Types.Iterate loop
         declare
            Component_Name : LAL.Defining_Name := Key (C);
            Component_Type : Typ'Class := Element (C).Get;
         begin
            Append_Value_For_Component
              (Component_Name,
               Component_Type.Generate_Static (Disc_Context),
               Result);
         end;
      end loop;
      return +Result;
   end Generate_Static;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self         : Nondiscriminated_Record_Typ;
      Disc_Context : Disc_Value_Map) return String
   is
      Result : Unbounded_String;
      Generated_Value : String :=
        Record_Typ (Self).Generate_Static (Disc_Context);
   begin
      Append (Result, "(");
      Append (Result, Generated_Value);
      Trim (Result, Right);
      Trim (Result, Null_Set, To_Set (','));
      Append (Result, ")");
      return +Result;
   end Generate_Static;

   ------------------
   -- Free_Variant --
   ------------------

   procedure Free_Variant (Var : in out Variant_Part_Acc) is
      use Variant_Choice_Lists;
      procedure Free is new Ada.Unchecked_Deallocation
         (Variant_Part, Variant_Part_Acc);

      procedure Destroy_Var_Choice (Var_Choice : in out Variant_Choice);

      ------------------------
      -- Destroy_Var_Choice --
      ------------------------

      procedure Destroy_Var_Choice (Var_Choice : in out Variant_Choice)
      is
      begin
         if Var_Choice.Variant /= null then
            Free_Variant (Var_Choice.Variant);
         end if;
      end Destroy_Var_Choice;

      Cur : Cursor := Var.Variant_Choices.First;
   begin
      if Var /= null then
         while Has_Element (Cur) loop
            Var.Variant_Choices.Update_Element
            (Cur, Destroy_Var_Choice'Access);
            Next (Cur);
         end loop;
         Free (Var);
      end if;
   end Free_Variant;

   -------------------
   -- Clone_Variant --
   -------------------

   function Clone (Var : Variant_Part_Acc) return Variant_Part_Acc is
      Res : Variant_Part_Acc;
   begin
      if Var = null then
         return null;
      end if;
      Res := new Variant_Part;
      Res.Discr_Name := Var.Discr_Name;
      for Choice of Var.Variant_Choices loop
         Res.Variant_Choices.Append (Variant_Choice'
           (Alternatives => Choice.Alternatives,
            Alt_Set      => Choice.Alt_Set.Copy,
            Components   => Choice.Components.Copy,
            Variant      => Clone (Choice.Variant)));
      end loop;
      return Res;
   end Clone;

   ------------------
   -- Check_Others --
   ------------------

   function Check_Others
     (Designator : LAL.Others_Designator;
      Val        : Big_Integer) return Boolean
   is
      use LAL;
      use GNATCOLL.GMP.Integers;
      Variant_Root : constant Variant_List :=
         Designator.Parent.Parent.Parent.As_Variant_List;
   begin
      for Variant of Variant_Root loop
         for Choice of Variant.As_Variant.F_Choices loop
            if Choice /= Designator
              and then Choice.P_Choice_Match (Make (Big_Int.To_String (Val)))
            then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Check_Others;

   ---------------------------
   -- Constraints_Respected --
   ---------------------------

   function Constraints_Respected
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Disc_Value_Map) return Boolean
   is
      use Discriminant_Constraint_Maps;
      use Disc_Value_Maps;
      use Ada.Numerics.Big_Numbers.Big_Integers;

      Constraint_Cur : Discriminant_Constraint_Maps.Cursor;
      Value_Cur : Disc_Value_Maps.Cursor;
   begin
      if not Self.Constrained then
         return True;
      end if;

      Constraint_Cur := Self.Discriminant_Constraint.First;
      while Has_Element (Constraint_Cur) loop
         if Element (Constraint_Cur).Kind = Static then
            Value_Cur :=
              Discriminant_Values.Find (Key (Constraint_Cur));
            if Has_Element (Value_Cur)
              and then
                Element (Value_Cur) /= Element (Constraint_Cur).Int_Val
            then
               return False;
            end if;
         end if;
         Next (Constraint_Cur);
      end loop;
      return True;
   end Constraints_Respected;

   ---------------------
   -- Fill_Components --
   ---------------------

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Disc_Value_Map) return Component_Maps.Map
   is
      Res : Component_Maps.Map := Self.Component_Types.Copy;
   begin
      if Self.Variant /= null then
         Fill_Components (Self.Variant.all, Discriminant_Values, Res);
      end if;
      return Res;
   end Components;

   ---------------------
   -- Fill_Components --
   ---------------------

   procedure Fill_Components
     (Self        : Variant_Part;
      Constraints : Disc_Value_Map;
      Res         : in out Component_Maps.Map)
   is
      Disc_Val_Cur : constant Disc_Value_Maps.Cursor :=
        Constraints.Find (Self.Discr_Name);
      Discr_Val : GNATCOLL.GMP.Integers.Big_Integer;
   begin
      if Disc_Value_Maps.Has_Element (Disc_Val_Cur)
      then
         Discr_Val.Set
           (Big_Int.To_String (Disc_Value_Maps.Element (Disc_Val_Cur)));
      end if;
      for Choice of Self.Variant_Choices loop
         declare
            Choice_Matches : constant Boolean :=
              (for some Alt of Choice.Alternatives
                 => Alt.P_Choice_Match (Discr_Val));
            Comp_Cur : Component_Maps.Cursor := Choice.Components.First;
         begin
            if Choice_Matches then
               while Component_Maps.Has_Element (Comp_Cur) loop
                  Res.Insert
                    (Component_Maps.Key (Comp_Cur),
                     Component_Maps.Element (Comp_Cur));
                  Component_Maps.Next (Comp_Cur);
               end loop;
               if Choice.Variant /= null then
                  Fill_Components (Choice.Variant.all, Constraints, Res);
               end if;
            end if;
            exit when Choice_Matches;
         end;
      end loop;
   end Fill_Components;

   function "+" (I : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer is
     (Big_Int.From_String (GNATCOLL.GMP.Integers.Image (I)));

   ----------------
   -- PP_Variant --
   ----------------

   function PP_Variant
     (Var     : Variant_Part_Acc;
      Padding : Natural := 0) return Unbounded_String
   is
      Res : Unbounded_String := (Padding * Pad) & "case ";
      Comp_Cur : Component_Maps.Cursor;
   begin
      Res := Res & Text.Image (Var.Discr_Name.Text) & " is" & LF;
      for Var_Choice of Var.Variant_Choices loop
         Res := Res & (Padding + 2) * Pad & "when ";
         if Debug_Variant_Set then
            Res := Res & "[";
            for Rng of Var_Choice.Alt_Set loop
               Res := Res & "[" & Big_Int.To_String (Rng.Min) & ", "
                     & Big_Int.To_String (Rng.Max) & "],";
            end loop;
            Res := Res & "] => " & LF;
         else
            Res := Res & Text.Image (Var_Choice.Alternatives.Text)
                   & " => " & LF;
         end if;
         Comp_Cur := Var_Choice.Components.First;
         while Component_Maps.Has_Element (Comp_Cur) loop
            Res := Res & (Padding + 3) * Pad &
                   Text.Image (Component_Maps.Key (Comp_Cur).Text)
                   & " : ";
            if Component_Maps.Element (Comp_Cur).Get.Kind in Record_Typ_Range
            then
               Res := Res & String'(As_Record_Typ (
                                    Component_Maps.Element (Comp_Cur))
                                    .Image_Internal (Padding + 3));
            else
               Res := Res & Component_Maps.Element (Comp_Cur).Get.Image;
            end if;
            Component_Maps.Next (Comp_Cur);
            Res := Res & LF;
         end loop;
         if Var_Choice.Variant /= null then
            Res := Res & PP_Variant (Var_Choice.Variant, Padding + 3);
         end if;
      end loop;
      Res := Res & (Padding + 1) * Pad & "end case" & LF;
      return Res;
   end PP_Variant;

   -----------
   -- Image --
   -----------

   function Image (Self : Discriminated_Record_Typ) return String is
      (Image_Internal (Self, 0));

   --------------------
   -- Image_Internal --
   --------------------

   function Image_Internal
     (Self : Discriminated_Record_Typ; Padding : Natural := 0) return String
   is
      use Component_Maps;
      use Discriminant_Constraint_Maps;
      Str : Unbounded_String := To_Unbounded_String (Typ (Self).Image);
      Current_Component : Component_Maps.Cursor;
   begin
      --  First display the discriminants in line
      Str := Str & ": " & (if Self.Mutable then "" else "non ")
             & "mutable record (";
      Current_Component := Self.Discriminant_Types.First;
      loop
         exit when not Has_Element (Current_Component);
         Str := Str & Text.Image (Key (Current_Component).Text) & ": "
                & Element (Current_Component).Get.Image;
         Next (Current_Component);
         exit when not Has_Element (Current_Component);
         Str := Str & "; ";
      end loop;
      Str := Str & ")" & LF;
      if Self.Component_Types.Is_Empty and Self.Variant = null then
         Str := Str & Padding * Pad & " no components" & LF;
      else
         if not Self.Component_Types.Is_Empty then
            Current_Component := Self.Component_Types.First;
            while Has_Element (Current_Component) loop
               Str :=
                 Str & (Padding + 1) * Pad
                 & Text.Image (Key (Current_Component).Text) & " : ";
               if Element (Current_Component).Get.Kind in Record_Typ_Range then
                  Str := Str
                    & String'(As_Record_Typ (Element (Current_Component))
                                       .Image_Internal (Padding + 1));
               else
                  Str := Str & Element (Current_Component).Get.Image & LF;
               end if;
               Next (Current_Component);
            end loop;
         end if;

         if Self.Variant /= null then
            Str := Str & (Padding + 1) * Pad
              & PP_Variant (Self.Variant, Padding + 1);
         end if;
      end if;
      Str := Str & "end record";
      return To_String (Str);
   end Image_Internal;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content (Self : in out Discriminated_Record_Typ) is
   begin
      if Self.Variant /= null then
         Free_Variant (Self.Variant);
      end if;
   end Free_Content;

   Has_Generated_Shapes : Boolean := False;
   Generated_Shapes : String_Set;
   Last_Generated_Shape : Count_Type := 0;

   package Alternative_Constraint_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => LAL.Defining_Name,
      Element_Type => Alternatives_Set,
      "="          => Alternatives_Sets."=");
   subtype Alternative_Constraint_Map is Alternative_Constraint_Maps.Map;

   function Generate_All_Shapes
     (Components       : Component_Map;

      Discr            : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc;
      Disc_Context     : Disc_Value_Map) return String_Set;

   function Generate_All_Shapes_Wrapper
     (Components       : Component_Map;
      Discr            : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc;
      Disc_Context     : Disc_Value_Map) return String_Set;
   --  Function that generates records of all the possible shapes. If this
   --  record has records as component, it will not generate all the possible
   --  shapes for the subrecord. TODO: think about this.

   function Generate_Static
     (Self         : Discriminated_Record_Typ;
      Disc_Context : Disc_Value_Map) return String
   is
      Record_Components : Component_Maps.Map;
      Discriminant_Values : Disc_Value_Map;
      Result : Unbounded_String;
      use Component_Maps;
      use Ada.Numerics.Big_Numbers.Big_Integers;
      use type Ada.Containers.Count_Type;
      Extended_Context : Disc_Value_Map := Disc_Context.Copy;
   begin

      --  If this is a constrained subtype, then we know the shape of the
      --  discriminant according to the constraints in Disc_Context.

      if Self.Constrained then

         for Cursor in Self.Discriminant_Constraint.Iterate loop
            declare
               use Discriminant_Constraint_Maps;
               D_Name : LAL.Defining_Name := Key (Cursor);
               Constraint : Discrete_Constraint_Value := Element (Cursor);
            begin
               case Constraint.Kind is

                  when Static =>

                     --  Static constraint:
                     --  type A (I : Integer) is null record;
                     --  subtype B is A (10);
                     --
                     --  Simply pick the literal constraint value.

                     Discriminant_Values.Insert (D_Name, Constraint.Int_Val);

                  when Discriminant =>

                     --  Discriminant constraint:
                     --  type A (I : Integer) is null record;
                     --  subtype B (J : Integer) is A (J);
                     --
                     --  In this case, we get the constraint value from the
                     --  discriminant context.

                     Discriminant_Values.Insert
                       (D_Name, Disc_Context.Element (D_Name));
                  when others =>
                     raise Program_Error
                       with "Non_Static constrained not supported";
               end case;
            end;
         end loop;

         Append (Result, "(");
         for C in Self.Discriminant_Types.Iterate loop
            declare
               A : Big_Integer;
               Discriminant_Name : LAL.Defining_Name :=  Key (C);
               Discriminant_Typ : Typ'Class := Element (C).Get;
               Generated_Value : String :=
                 To_String (Discriminant_Values.Element (Discriminant_Name));
            begin
               Append_Value_For_Component
                 (Discriminant_Name,
                  Generated_Value,
                  Result);
            end;
         end loop;

         goto Generate_Record;
      end if;

      --  Otherwise, generate the set of possible shapes. TODO: use a generator
      --  to implement it as the current approach (using a global variable) is
      --  not robust at all.

      if not Has_Generated_Shapes then
         Generated_Shapes :=
           Generate_All_Shapes_Wrapper
             (Self.Component_Types,
              Self.Discriminant_Types,
              Alternative_Constraint_Maps.Empty_Map,
              Self.Variant,
              Disc_Context);
      end if;
      if Last_Generated_Shape < Generated_Shapes.Length then
         declare
            I : Count_Type := 0;
         begin
            for Shape of Generated_Shapes loop
               if I = Last_Generated_Shape then
                  Last_Generated_Shape := Last_Generated_Shape + 1;
                  return Shape;
               end if;
               I := I + 1;
            end loop;
         end;
      end if;

      --  If all shapes were generated, generate random values

      Append (Result, "(");
      for C in Self.Discriminant_Types.Iterate loop
         declare
            A : Big_Integer;
            Discriminant_Name : LAL.Defining_Name :=  Key (C);
            Discriminant_Typ : Typ'Class := Element (C).Get;
            Generated_Value : String :=
              Discriminant_Typ.Generate_Static (Disc_Context);
         begin
            Discriminant_Values.Insert
              (Discriminant_Name,
               From_String (Generated_Value));
            Extended_Context.Insert
              (Discriminant_Name, From_String (Generated_Value));
            Append_Value_For_Component
              (Discriminant_Name,
               Generated_Value,
               Result);
         end;
      end loop;

      <<Generate_Record>>
      Record_Components := Components (Self, Discriminant_Values);
      declare
         Record_Shape : Typ'Class :=
           Record_Typ'
             (Name => Self.Name, Component_Types => Record_Components);
         Generated_Value : String :=
           Record_Shape.Generate_Static (Extended_Context);
      begin
         Append (Result, Generated_Value);
      end;

      Trim (Result, Right);
      Trim (Result, Null_Set, To_Set (','));
      Append (Result, ")");

      return +Result;
   end Generate_Static;

   function Generate_Value_In_Alternatives
     (Alt_Set : Alternatives_Set) return Big_Integer;
   --  Pulls a value from a list of sets. TODO: make it random

   function Generate_Value_In_Alternatives
     (Alt_Set : Alternatives_Set) return Big_Integer is
   begin
      return Alt_Set.First_Element.Min;
   end Generate_Value_In_Alternatives;

   function Generate_All_Shapes
     (Components       : Component_Map;
      Discr            : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc;
      Disc_Context     : Disc_Value_Map) return String_Set
   is
      use Alternative_Constraint_Maps;
      use Component_Maps;
      use Ada.Numerics.Big_Numbers.Big_Integers;
      Result : String_Set;
   begin

      --  If the variant is null, we reached a leaf of the record. Use the
      --  discriminant constraints and the list of components to generate the
      --  expected shape.

      if Variant = null then

         declare
            New_Disc_Context : Disc_Value_Map := Disc_Context.Copy;
            Res : Unbounded_String;
         begin

            --  First, let's take care of the discriminants. Simply, we
            --  generate the first value of the first interval for each
            --  discriminant constraint (TODO: pull a random value in the list
            --  of intervals).
            --
            --  Append them to the Disc_Context.

            for Disc_Constraint in Disc_Constraints.Iterate loop
               declare
                  Disc_Name : LAL.Defining_Name :=
                    Key (Disc_Constraint);
                  Alt_Set : Alternatives_Set :=
                    Element (Disc_Constraint);
                  Value : Big_Integer :=
                    Generate_Value_In_Alternatives (Alt_Set);
               begin
                  New_Disc_Context.Insert (Disc_Name, Value);
                  Append_Value_For_Component
                    (Disc_Name,
                     To_String (Value),
                     Res);
               end;
            end loop;

            --  Then, generate all the discriminant that have no constraints,
            --  i.e. no variant associated.

            for Disc in Discr.Iterate loop
               declare
                  Disc_Name : LAL.Defining_Name := Key (Disc);
                  Disc_Typ  : Typ'Class := Element (Disc).Get;
               begin
                  if not Disc_Constraints.Contains (Disc_Name) then
                     declare
                        Value : Big_Integer :=
                          From_String
                            (Disc_Typ.Generate_Static (Disc_Context));
                     begin
                        New_Disc_Context.Insert (Disc_Name, Value);
                        Append_Value_For_Component
                          (Disc_Name, To_String (Value), Res);
                     end;
                  end if;
               end;
            end loop;

            --  Then, generate all the values for the components

            for Comp in Components.Iterate loop
               declare
                  Comp_Name : LAL.Defining_Name := Key (Comp);
                  Comp_Typ  : Typ'Class := Element (Comp).Get;
               begin
                  Append_Value_For_Component
                    (Comp_Name,
                     Comp_Typ.Generate_Static (New_Disc_Context),
                     Res);
               end;
            end loop;

            return String_Sets.To_Set (+Res);
         end;

      end if;

      for Variant_Choice of Variant.Variant_Choices loop

         declare

            Extended_Components : Component_Map := Components.Copy;
            Extended_Constraints : Alternative_Constraint_Map :=
              Disc_Constraints.Copy;
         begin
         --  We have the interval values for this variant choice: put
         --  them in the Disc_Constraints.

            Extended_Constraints.Insert
              (Variant.Discr_Name,
               Variant_Choice.Alt_Set);

            --  Insert the new components

            for Comp in Variant_Choice.Components.Iterate loop
               Extended_Components.Insert (Key (Comp), Element (Comp));
            end loop;

            Result.Union
              (Generate_All_Shapes
                 (Extended_Components,
                  Discr,
                  Extended_Constraints,
                  Variant_Choice.Variant,
                  Disc_Context));
         end;
      end loop;
      return Result;
   end Generate_All_Shapes;

   function Generate_All_Shapes_Wrapper
     (Components       : Component_Map;
      Discr            : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc;
      Disc_Context     : Disc_Value_Map) return String_Set
   is
      Shapes : String_Set :=
        Generate_All_Shapes
          (Components, Discr, Disc_Constraints, Variant, Disc_Context);
      Result : String_Set;
   begin
      for Shape of Shapes loop
         Result.Insert ("("
                        & Trim (Trim (Shape, Right), Null_Set, To_Set (','))
                        & ")");
      end loop;
      return Result;
   end Generate_All_Shapes_Wrapper;

   ------------------------------------------
   -- Generate_Constrained_Random_Strategy --
   ------------------------------------------

   function Generate_Constrained_Random_Strategy
     (Self    : Discriminated_Record_Typ;
      Context : Generation_Context) return Strategy_Type
   is
      use Component_Maps;

      Res : Strategy_Type (Kind => Random_Kind, Constrained => False);
      F_Body : Unbounded_String;
      Indentation : Unbounded_String := +"";
      Rec_Name : constant String := "Rec";

      procedure A (Str : String);
      procedure NL;
      procedure I;
      procedure UI;
      procedure Pp_Components (Components : Component_Maps.Map);
      procedure Pp_Variant
        (Components : Component_Vector;
         Variant    : Variant_Part);

      procedure A (Str : String) is
      begin
         Append (F_Body, Str);
      end A;

      procedure NL is
      begin
         Append (F_Body, Ada.Characters.Latin_1.LF & (+Indentation));
      end NL;

      procedure I is
      begin
         Append (Indentation, "   ");
      end I;

      procedure UI is
      begin
         Indentation := Head (Indentation, Length (Indentation) - 3);
      end UI;

      procedure Pp_Components (Components : Component_Maps.Map) is
      begin
         for Component in Components.Iterate loop
            declare
               Component_Name : LAL.Defining_Name := Key (Component);
               Component_Type : Typ'Class := Element (Component).Get;
            begin
               A ((+Component_Name.F_Name.Text) & " : ");
               A ((+Component_Type.Fully_Qualified_Name) & " :=");
               I;
               NL;
               A (Component_Type.Gen_Random_Function_Name & ";");
               UI;
            end;
         end loop;
      end Pp_Components;

      procedure Pp_Variant
        (Components : Component_Vector;
         Variant    : Variant_Part) is
         use Variant_Choice_Lists;

      begin
         A ("case " & (+Self.Variant.Discr_Name.F_Name.Text) & " is ");
         I;
         NL;
         for Choice of Variant.Variant_Choices loop
            declare
               use Component_Maps;
               use Component_Vectors;

               New_Components : Component_Vector := Components.Copy;
            begin
               for C in Choice.Components.Iterate loop
                  Append (New_Components, Key (C));
               end loop;
               A ("when " & (+Choice.Alternatives.Text) & " => ");
               I;
               NL;
               if Choice.Components.Length /= 0 then
                  A ("declare");
                  I;
                  NL;
                  Pp_Components (Choice.Components);
                  UI;
                  NL;
                  A ("begin");
                  I;
               end if;

               if Choice.Variant /= null then
                  Pp_Variant (New_Components, Choice.Variant.all);

               else
                  if New_Components.Length = 0 then
                     A ("null;");
                  end if;
                  for C of New_Components loop
                     NL;
                     A (Rec_Name & "." & (+C.F_Name.Text)
                        & " := " & (+C.F_Name.Text) & ";");
                  end loop;
               end if;

               if Choice.Components.Length /= 0 then
                  UI;
                  NL;
                  A ("end;");
               end if;
               UI;
               NL;
            end;
         end loop;
         UI;
         NL;
         A ("end case;");
         NL;
      end Pp_Variant;

      Constrained_Function : constant Subprogram_Data :=
        Gen_Constrained_Function (Self);
   begin

      A ("declare");
      I;
      NL;
      A (Rec_Name & " : " & (+Self.Fully_Qualified_Name));

      --  Then add the discriminants to the record type declaration

      if Self.Discriminant_Types.Length > 0 then
         A (" (");
         for Disc in Self.Discriminant_Types.Iterate loop
            A (+Key (Disc).F_Name.Text);
            if Next (Disc) /= No_Element then
               A (", ");
            end if;
         end loop;
         A (");");
      end if;

      --  Now generate a function that is similar to the record declaration:
      --  generate the different variant parts, and when there are no more
      --  variant, assign the list of generated components to the resulting
      --  record.

      UI;
      NL;
      A ("begin");
      I;
      NL;

      Pp_Components (Self.Component_Types);
      if Self.Variant /= null then
         Pp_Variant (Component_Vectors.Empty, Self.Variant.all);
      end if;

      A ("return " & Rec_Name & ";");
      UI;
      NL;
      A ("end;");

      Res.Strategy_Function := Constrained_Function;
      Res.Strategy_Body := +(+F_Body);

      return Res;
   end Generate_Constrained_Random_Strategy;

   ---------------
   -- Nth_Child --
   ---------------

   function Nth_Child
     (Parent : Intervals_Biased_Trees.Cursor;
      N : Positive) return Intervals_Biased_Trees.Cursor
   is
      use Intervals_Biased_Trees;
      Child : Cursor := First_Child (Parent);
      I : Positive := 1;
   begin
      while I < N and then Child /= No_Element loop
         Child := Next_Sibling (Child);
         I := I + 1;
      end loop;

      if I /= N then
         raise Program_Error with "Child " & Positive'Image (N) &
           " not found";
      end if;

      return Child;
   end Nth_Child;

   function Pick_Choice (V : Intervals_With_Bias_Vector) return Positive
     with Pre => V.Length > 0;

   function Pick_Choice (V : Intervals_With_Bias_Vector) return Positive is
      R : Float := TGen.Random.Rand_Float;
      Cpt : Float := Float (0);
      Choice : Positive := 1;
   begin
      for Elem of V loop
         Cpt := Cpt + Elem.Bias;
         if Cpt > R then
            return Choice;
         end if;
         Choice := Choice + 1;
      end loop;
      return Choice;
   end Pick_Choice;

   function Draw (Intervals : Alternatives_Set) return Big_Integer;

   --  TODO: pick a value in the intervals and not the first one

   function Draw (Intervals : Alternatives_Set) return Big_Integer is
   begin
      return Intervals.First_Element.Min;
   end Draw;

   procedure Blll
     (Node              : in out Intervals_Biased_Trees.Cursor;
      Variant_Choices   : in out Positive_Vector;
      Update_Disc_Value : access procedure (V : Big_Integer)) is

      use Intervals_Biased_Trees;

      V : Intervals_With_Bias_Vector := Element (Node).Data;
      Intervals : Alternatives_Set;
      Variant_Choice : constant Positive := Pick_Choice (V);
   begin
      Intervals := V.Element (Variant_Choice).Intervals;
      Update_Disc_Value (Draw (Intervals));
      Variant_Choices.Append (Variant_Choice);
      Node := Nth_Child (Node, Variant_Choice);
   end Blll;

   procedure Fixup_Bias
     (Tr   : in out Intervals_Biased_Trees.Tree;
      Leaf : Intervals_Biased_Trees.Cursor;
      Choices : Positive_Vector)
   is

      use Intervals_Biased_Trees;
      use type Count_Type;

      Current_Node : Cursor := Leaf.Parent;
      Current_Choice_Index : Natural := Natural (Choices.Length);

      procedure Redistribute_Bias
        (V                  : in out Intervals_With_Bias_Vector;
         Redistributed_Bias : Float);

      procedure Process_Node
        (Tr     : in out Tree;
         Node   : in out Cursor;
         Choice : Positive);

      procedure Redistribute_Bias
        (V                  : in out Intervals_With_Bias_Vector;
         Redistributed_Bias : Float) is

         procedure Update_Element
           (IB : in out Intervals_With_Bias);

         procedure Update_Element
           (IB : in out Intervals_With_Bias) is
         begin
            --  Each element takes a chunk of the redistributed bias according
            --  to what its bias was before.
            IB.Bias :=
              @ + (@ / (1.0 - Redistributed_Bias)) * Redistributed_Bias;
         end Update_Element;

         use Intervals_With_Bias_Vectors;
      begin
         for C in V.Iterate loop
            Update_Element (V, C, Update_Element'Access);
         end loop;
      end Redistribute_Bias;


      --  If the node has no subtree of index choice rooted to it, we will
      --  suppress the choice from the list of alternatives. This happens
      --  when a shape or all the shapes in the variant have been generated.
      --  Note that this function should be customizable.

      procedure Process_Node
        (Tr     : in out Tree;
         Node   : in out Cursor;
         Choice : Positive) is
         Variants : Intervals_With_Bias_Vector := Element (Node).Data;
         Bias     : Float := Variants.Element (Choice).Bias;
         Child    : Cursor := Nth_Child (Node, Choice);
      begin
         if Child_Count (Node) = 0 then
            Intervals_With_Bias_Vectors.Delete
              (Variants,
               Intervals_With_Bias_Vectors.Extended_Index'(Choice));
            Delete_Leaf (Tr, Child);

            --  We redistribute all of its bias to its siblings

            Redistribute_Bias (Variants, Bias);
         end if;

         --  Otherwise, let's do nothing. We could make bias modifications in
         --  the uptree as well (see if part of the subtree was deleted, and
         --  disminish the bias if that is the case, left as a TODO for later).

      end Process_Node;

   begin
      while Current_Node /= No_Element loop
         Process_Node
           (Tr, Current_Node, Choices.Element (Current_Choice_Index));
         Current_Node := Parent (Current_Node);
         Current_Choice_Index := Current_Choice_Index - 1;
      end loop;
   end Fixup_Bias;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self    : Discriminated_Record_Typ;
      Context : in out Generation_Context) return Strategy_Type
   is
      Result : Strategy_Type (Kind => Random_Kind, Constrained => True);
      F_Body : Unbounded_String;

      Indentation : Natural := 0;

      Constrained_Function : Subprogram_Data :=
        Gen_Constrained_Function (Self);
   begin

      Write_Line (F_Body, "declare", Indentation);
      Write_Line (F_Body, "begin", Indentation);

      Write_Line
        (F_Body, "return " & (+Constrained_Function.Name), Indentation);

      Indentation := Indentation + 3;

      S_Write (F_Body, "( ", Indentation);

      --  Expected parameters are discriminant values; pass them

      for Disc_Cursor in Self.Discriminant_Types.Iterate loop
         declare
            use Component_Maps;

            Disc_Name : constant String :=
              +Key (Disc_Cursor).F_Name.Text;
            Disc_Type : Typ'Class := Element (Disc_Cursor).Get;

            Suffix : constant String :=
              (if Next (Disc_Cursor) = No_Element then ");" else " ,");
         begin
            Write_Line
              (F_Body,
               Disc_Name & " => "
               & Disc_Type.Gen_Random_Function_Name
               & Suffix,
               Indentation);
         end;
      end loop;

      Indentation := Indentation - 3;
      Write_Line (F_Body, "end;", Indentation);

      Result.Strategy_Body := +(+F_Body);
      Result.Strategy_Function := Self.Random_Strategy_Function;

      --  This uses the constrained strategy

      declare
         Constrained_Strategy : aliased Strategy_Type :=
           Self.Generate_Constrained_Random_Strategy (Context);
      begin
         Context.Strategies.Insert (Constrained_Strategy);
         Result.Constrained_Strategy_Function :=
           Constrained_Strategy'Unchecked_Access;
      end;

      return Result;
   end Generate_Random_Strategy;

end TGen.Types.Record_Types;
