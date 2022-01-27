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

package body TGen.Types.Record_Types is

   package Text renames Langkit_Support.Text;

   package Big_Int renames Ada.Numerics.Big_Numbers.Big_Integers;

   LF : constant String := (1 => ASCII.LF);

   Pad : constant Unbounded_String := 3 * ' ';

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
      Constraints : Discriminant_Constraint_Maps.Map;
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

   function Generate_Static (Self : Record_Typ) return String is
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
               Component_Type.Generate_Static,
               Result);
         end;
      end loop;
      return +Result;
   end Generate_Static;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static (Self : Nondiscriminated_Record_Typ) return String
   is
      Result : Unbounded_String;
      Generated_Value : String := Record_Typ (Self).Generate_Static;
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
      use Variant_Choice_Maps;
      procedure Free is new Ada.Unchecked_Deallocation
         (Variant_Part, Variant_Part_Acc);

      procedure Destroy_Var_Choice
        (Idx : Positive; Var_Choice : in out Variant_Choice);

      ------------------------
      -- Destroy_Var_Choice --
      ------------------------

      procedure Destroy_Var_Choice
        (Idx : Positive; Var_Choice : in out Variant_Choice)
      is
      begin
         if Var_Choice.Variant /= null then
            Free_Variant (Var_Choice.Variant);
         end if;
      end Destroy_Var_Choice;

      Cur : Cursor := Var.Variant_Choices.First;
   begin
      while Has_Element (Cur) loop
         Var.Variant_Choices.Update_Element
           (Cur, Destroy_Var_Choice'Access);
         Next (Cur);
      end loop;
      Free (Var);
   end Free_Variant;

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
      Discriminant_Values : Discriminant_Constraint_Maps.Map)
      return Boolean
   is
      use Discriminant_Constraint_Maps;
      Constraint_Cur, Value_Cur : Cursor;
   begin
      if not Self.Constrained then
         return True;
      end if;

      Constraint_Cur := Self.Discriminant_Constraint.First;
      while Has_Element (Constraint_Cur) loop
         if Element (Constraint_Cur).Kind = Static then
            Value_Cur := Discriminant_Values.Find (Key (Constraint_Cur));
            if Has_Element (Value_Cur)
              and then Element (Value_Cur) /= Element (Constraint_Cur)
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
      Discriminant_Values : Discriminant_Constraint_Maps.Map)
      return Component_Maps.Map
   is
      Res : Component_Maps.Map := Self.Component_Types.Copy;
   begin
      if Self.Variant /= null then
         Fill_Components (Self.Variant.all, Discriminant_Values, Res);
      end if;
      return Res;
   end Components;

   procedure Fill_Components
     (Self        : Variant_Part;
      Constraints : Discriminant_Constraint_Maps.Map;
      Res         : in out Component_Maps.Map)
   is
      Disc_Val_Cur : constant Discriminant_Constraint_Maps.Cursor :=
        Constraints.Find (Self.Discr_Name);
      Discr_Val : GNATCOLL.GMP.Integers.Big_Integer;
   begin
      if Discriminant_Constraint_Maps.Has_Element (Disc_Val_Cur)
        and then Discriminant_Constraint_Maps.Element (Disc_Val_Cur).Kind
                 = Static
      then
         Discr_Val.Set
           (Big_Int.To_String (Discriminant_Constraint_Maps.Element
                               (Disc_Val_Cur).Int_Val));
      else
         raise Discriminant_Value_Error;
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

   type A (I : Integer) is record
      case I is
         when 1 .. 2 | 3 =>
            null;
         when others =>
            null;
      end case;
   end record;

   type B (I : Character) is record
      case I is
         when others =>
            null;
      end case;
   end record;

   function "+" (I : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer is
     (Big_Int.From_String (GNATCOLL.GMP.Integers.Image (I)));

   --  function Interval_Of_Alternative (Alt : Expr) return Interval;
   --
   --  function Interval_Of_Alternative (Alt : Expr) return Interval is
   --     use Libadalang.Common;
   --     L : Big_Integer;
   --     R : Big_Integer;
   --  begin
   --
   --     --  Special cases
   --
   --     case Alt.Kind is
   --        when Ada_Bin_Op =>
   --           case Alt.As_Bin_Op.F_Op is
   --
   --              --  Range
   --
   --              when Ada_Op_Double_Dot =>
   --                 L := +As_Int (Expr_Eval (Alt.As_Bin_Op.F_Left));
   --                 R := +As_Int (Expr_Eval (Alt.As_Bin_Op.F_Right));
   --                 return Interval'(Int, L, R);
   --              when others => null;
   --           end case;
   --        when others => null;
   --     end case;
   --
   --     --  Default: evaluation of the node and singleton interval
   --
   --     L := +As_Int (Expr_Eval (Alt));
   --     return Interval'(Int, L, L);
   --  end Interval_Of_Alternative;
   --
   --  function Intervals_Of_Variant
   --    (Disc_Record       : Discriminated_Record_Typ;
   --     Covered_Intervals : Interval_Set;
   --     Variant_Part      : Variant_Part;
   --     Choice            : Variant_Choice) return Interval_Set;
   --
   --  function Intervals_Of_Variant
   --    (Disc_Record       : Discriminated_Record_Typ;
   --     Covered_Intervals : Interval_Set;
   --     Variant_Part      : Variant_Part;
   --     Choice            : Variant_Choice) return Interval_Set is
   --     Result : Interval_Set;
   --  begin
   --     for Alt of Choice.Alternatives loop
   --        if Alt.Kind = Ada_Others_Designator then
   --           for Interval of Covered_Intervals loop
   --              null;
   --           end loop;
   --        else
   --           Result.Insert (Interval_Of_Alternative (Alt.As_Expr));
   --        end if;
   --     end loop;
   --     return Result;
   --  end Intervals_Of_Variant;

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
         Res := Res & (Padding + 2) * Pad & "when "
                & Text.Image (Var_Choice.Alternatives.Text) & " => " & LF;
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
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc) return String_Set;

   function Generate_All_Shapes_Wrapper
     (Components       : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc) return String_Set;
   --  Function that generates records of all the possible shapes. If this
   --  record has records as component, it will not generate all the possible
   --  shapes for the subrecord. TODO: think about this.

   function Generate_Static
     (Self : Discriminated_Record_Typ) return String
   is
      Record_Components : Component_Maps.Map;
      Discriminant_Values : Discriminant_Constraint_Maps.Map;
      Result : Unbounded_String;
      use Component_Maps;
      use Ada.Numerics.Big_Numbers.Big_Integers;
      use type Ada.Containers.Count_Type;
   begin
      if not Has_Generated_Shapes then
         Generated_Shapes :=
           Generate_All_Shapes_Wrapper
             (Self.Component_Types,
              Alternative_Constraint_Maps.Empty_Map,
              Self.Variant);
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
      Append (Result, "(");
      for C in Self.Discriminant_Types.Iterate loop
         declare
            A : Big_Integer;
            Discriminant_Name : LAL.Defining_Name :=  Key (C);
            Discriminant_Typ : Typ'Class := Element (C).Get;
            Generated_Value : String := Discriminant_Typ.Generate_Static;
         begin
            Discriminant_Values.Insert
              (Discriminant_Name,
               Constraint_Value'
                 (Kind => Static,
                  Int_Val => From_String (Generated_Value)));
            Append_Value_For_Component
              (Discriminant_Name,
               Generated_Value,
               Result);
         end;
      end loop;

      Record_Components := Components (Self, Discriminant_Values);
      declare
         Record_Shape : Typ'Class :=
           Record_Typ'
             (Name => Self.Name, Component_Types => Record_Components);
         Generated_Value : String := Record_Shape.Generate_Static;
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
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc) return String_Set
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
            Res : Unbounded_String;
         begin

            --  First, let's take care of the discriminants. Simply, we
            --  generate the first value of the first interval for each
            --  discriminant constraint (TODO: pull a random value in the list
            --  of intervals).

            for Disc_Constraint in Disc_Constraints.Iterate loop
               declare
                  Disc_Name : LAL.Defining_Name :=
                    Key (Disc_Constraint);
                  Alt_Set : Alternatives_Set :=
                    Element (Disc_Constraint);
               begin
                  Append_Value_For_Component
                    (Disc_Name,
                     To_String (Generate_Value_In_Alternatives (Alt_Set)),
                     Res);
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
                     Comp_Typ.Generate_Static,
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
                  Extended_Constraints,
                  Variant_Choice.Variant));
         end;
      end loop;
      return Result;
   end Generate_All_Shapes;

   function Generate_All_Shapes_Wrapper
     (Components       : Component_Map;
      Disc_Constraints : Alternative_Constraint_Map;
      Variant          : Variant_Part_Acc) return String_Set
   is
      Shapes : String_Set :=
        Generate_All_Shapes (Components, Disc_Constraints, Variant);
      Result : String_Set;
   begin
      for Shape of Shapes loop
         Result.Insert ("("
                        & Trim (Trim (Shape, Right), Null_Set, To_Set (','))
                        & ")");
      end loop;
      return Result;
   end Generate_All_Shapes_Wrapper;

   package body Random_Discriminated_Record_Strategy is

      procedure Gen
        (Strat : Random_Discriminated_Record_Strategy_Type;
         Stream : access Root_Stream_Type'Class) is
      begin
         Discriminated_Record_Type'Output (Stream, Gen (Gen));
      end Gen;

   end Random_Discriminated_Record_Strategy;

end TGen.Types.Record_Types;
