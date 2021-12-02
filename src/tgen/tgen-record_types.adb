------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with GNATCOLL.GMP.Integers;

with Libadalang.Common;
with Langkit_Support.Text;

package body TGen.Record_Types is

   package Text renames Langkit_Support.Text;

   LF : constant String := (1 => ASCII.LF);

   function Check_Others
     (Designator : LAL.Others_Designator;
      Val        : GNATCOLL.GMP.Integers.Big_Integer) return Boolean;
   --  Check if Val Satisfies this "others" choice. This is done by
   --  checking that Val staisfies all the other choices of the variant.

   -----------
   -- Image --
   -----------

   function Image (Self : Nondiscriminated_Record_Typ) return String is
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
            Str :=
              Str & "   " & Text.Image (Key (Current_Component).Text) & ": " &
              Element (Current_Component).Get.Image & LF;
            Next (Current_Component);
         end loop;
         Str := Str & "end record" & LF;
      end if;
      return To_String (Str);
   end Image;

   ------------------
   -- Check_Others --
   ------------------

   function Check_Others
     (Designator : LAL.Others_Designator;
      Val        : GNATCOLL.GMP.Integers.Big_Integer) return Boolean
   is
      use LAL;
      Variant_Root : constant Variant_List :=
         Designator.Parent.Parent.Parent.As_Variant_List;
   begin
      for Variant of Variant_Root loop
         for Choice of Variant.As_Variant.F_Choices loop
            if Choice /= Designator and then Choice.P_Choice_Match (Val)
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

   ----------------
   -- Components --
   ----------------

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Discriminant_Constraint_Maps.Map)
      return Component_Maps.Map
   is
      use Discriminant_Choices_Maps;
      use Discriminant_Constraint_Maps;
      use Shape_Maps;
      use GNATCOLL.GMP.Integers;
      use LAL;
   begin
      --  First Check that the values passed satisfy the constraints, if there
      --  are any.

      if Self.Constrained
        and then not Self.Constraints_Respected (Discriminant_Values)
      then
         raise Discriminant_Value_Error;
      end if;

      --  The try to find a shape that matches all of the discriminant values.
      --  If the values are not all static, then the first shape that matches
      --  will be returned.

      for Current_Shape of Self.Shapes loop
         if Shape_Matches (Current_Shape, Discriminant_Values) then
            return Current_Shape.Components;
         end if;
      end loop;

      --  If we reach this point, it means that the values supplied for the
      --  discriminants do not satisfy any of the choice combinations for each
      --  shape, so at least one of the discriminant values is out of bounds
      --  for the type of the corresponding discriminant.

      raise Discriminant_Value_Error with "Discriminant values match no shape";
   end Components;

   -------------------
   -- Shape_Matches --
   -------------------

   function Shape_Matches
     (Shp : Shape;
      Discriminant_Values : Discriminant_Constraint_Maps.Map) return Boolean
   is
      use GNATCOLL.GMP.Integers;
      use Discriminant_Constraint_Maps;
      use LAL;
      package LALCO renames Libadalang.Common;

      Discr_Cur : Cursor;
      Discr_Val : Big_Integer;
   begin
      for Choice of Shp.Discriminant_Choices loop
         Discr_Cur := Discriminant_Values.Find (Choice.Defining_Name);

         --  Non static constrait or constraint based on a discriminant satify
         --  all choices.

         if not Has_Element (Discr_Cur)
           or else Element (Discr_Cur).Kind in Non_Static | Discriminant
         then
            goto Next_Choice;
         end if;

         --  Otherwise, check that the static value we have satisfies at least
         --  one of the alternatives.

         Discr_Val.Set (Element (Discr_Cur).Int_Val'Image);
         for Alternative of Choice.Choices loop
            if (if Kind (Alternative) in LALCO.Ada_Others_Designator_Range
                then Check_Others (Alternative.As_Others_Designator, Discr_Val)
                else Alternative.P_Choice_Match (Discr_Val))
            then
               goto Next_Choice;
            end if;
         end loop;

         --  If we reach here, it means that we have a static discriminant
         --  value that does not satisfy any of the alternatives. This shape
         --  is thus not compatible with the Discriminant_Values.

         return False;

         <<Next_Choice>>
      end loop;
      return True;
   end Shape_Matches;

   -----------
   -- Image --
   -----------

   function Image (Self : Discriminated_Record_Typ) return String is
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
      if Self.Component_Types.Is_Empty then
         Str := Str & ": no components";
      else
         Current_Component := Self.Component_Types.First;
         while Has_Element (Current_Component) loop
            Str :=
              Str & "   " & Text.Image (Key (Current_Component).Text) & ": " &
              Element (Current_Component).Get.Image & LF;
            Next (Current_Component);
         end loop;
         Str := Str & "end record" & LF;
      end if;

      --  If all discriminants are of a discrete and static type, then display
      --  the components associated with all discriminants set the current
      --  constraint, if it exists, or the the lowest bound of the type.

      if (for all Discr of Self.Discriminant_Types =>
           (Discr.Get.Kind in Discrete_Typ_Range)
           and then As_Discrete_Typ (Discr).Is_Static)
      then
         Str := Str & LF & "Components for discriminants (";
         declare
            Discr_Vals : Discriminant_Constraint_Maps.Map;
            Filtered_Components : Component_Maps.Map;
            Current_Lit : Integer;
            Current_Constraint : Discriminant_Constraint_Maps.Cursor;
         begin
            Current_Component := Self.Discriminant_Types.First;
            loop
               exit when not Has_Element (Current_Component);
               declare
                  Current_Typ : constant Discrete_Typ'Class :=
                    As_Discrete_Typ (Element (Current_Component));
               begin
                  if Self.Constrained then
                     Current_Constraint :=
                       Self.Discriminant_Constraint.Find
                         (Key (Current_Component));
                     if Has_Element (Current_Constraint)
                       and then Element (Current_Constraint).Kind = Static
                     then
                        Current_Lit := Element (Current_Constraint).Int_Val;
                     else
                        Current_Lit := Current_Typ.Low_Bound;
                     end if;
                  else
                     Current_Lit := Current_Typ.Low_Bound;
                  end if;
                  Discr_Vals.Insert
                  (Key (Current_Component),
                   (Kind => Static, Int_Val => Current_Lit));
                  Str := Str & Text.Image (Key (Current_Component).Text)
                         & " => "
                         & As_Discrete_Typ (Element (Current_Component))
                           .Lit_Image (Current_Lit);
                  Next (Current_Component);
                  exit when not Has_Element (Current_Component);
                  Str := Str & ", ";
               end;
            end loop;
            Str := Str & ")" & LF;
            Filtered_Components := Self.Components (Discr_Vals);

            Current_Component := Filtered_Components.First;
            while Has_Element (Current_Component) loop
               Str :=
               Str & "   " & Text.Image (Key (Current_Component).Text) & LF;
               Next (Current_Component);
            end loop;
         end;
      end if;

      return To_String (Str);

   end Image;

end TGen.Record_Types;
