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

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Common;
with Langkit_Support.Text;

with GNATCOLL.GMP.Integers;

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
            Str := (Padding + 1) * Pad
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

   package body Random_Discriminated_Record_Strategy is

      procedure Gen
        (Strat : Random_Discriminated_Record_Strategy_Type;
         Stream : access Root_Stream_Type'Class) is
      begin
         Discriminated_Record_Type'Output (Stream, Gen (Gen));
      end Gen;

   end Random_Discriminated_Record_Strategy;

end TGen.Types.Record_Types;
