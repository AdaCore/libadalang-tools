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

   ----------------
   -- Components --
   ----------------

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Discriminant_Values_Maps.Map)
      return Component_Maps.Map
   is
      use Discriminant_Choices_Maps;
      use Discriminant_Values_Maps;
      use GNATCOLL.GMP.Integers;
      use LAL;

      function Check_Others
        (Designator : Others_Designator; Val : Big_Integer) return Boolean;
      --  Check if Val Satisfies this "others" choice. This is done by
      --  checking that Val staisfies all the other choices of the variant.

      function Check_Others
        (Designator : Others_Designator; Val : Big_Integer) return Boolean
      is
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

      Shape_Index       : Positive := Self.Shapes'First;
      Current_Shape     : Shape    := Self.Shapes (Shape_Index);
      Current_Discr_Val : Big_Integer;
      Current_Choice    : Ada_Node;
   begin
      loop
         exit when Shape_Index > Self.Shapes'Last;
         Current_Shape := Self.Shapes (Shape_Index);
         for Choice_Entry of Current_Shape.Discriminant_Choices loop
            Current_Discr_Val.Set
              (Integer'
                 (Discriminant_Values.Element (Choice_Entry.Defining_Name))'
                 Image);
            Current_Choice := Choice_Entry.Choices.First_Child;
            loop
               if Is_Null (Current_Choice) then
                  goto Next_Shape;
               end if;
               exit when
                 (Kind (Current_Choice) in
                    Libadalang.Common.Ada_Others_Designator_Range)
                 and then Check_Others
                   (Current_Choice.As_Others_Designator, Current_Discr_Val);
               exit when Current_Choice.P_Choice_Match (Current_Discr_Val);

               Current_Choice := Current_Choice.Next_Sibling;
            end loop;
         end loop;
         return Self.Shapes (Shape_Index).Components;

         <<Next_Shape>>
         Shape_Index := Shape_Index + 1;
      end loop;

      --  If we reach this point, it means that the values supplied for the
      --  discriminants do not satisfy any of the choice combinations for each
      --  shape, so at least one of the discriminant values is out of bounds
      --  for the type of the corresponding discriminant.
      --
      --  ??? Raise Constraint_Error for now, but we'll probably need to find a
      --  better failsafe mechanism.

      raise Constraint_Error;
   end Components;

   -----------
   -- Image --
   -----------

   function Image (Self : Discriminated_Record_Typ) return String is
      use Component_Maps;
      Str : Unbounded_String := To_Unbounded_String (Typ (Self).Image);
      Current_Component : Cursor;
   begin
      --  First display the discriminants in line
      Str := Str & ": record (";
      Current_Component := Self.Discriminant_Types.First;
      loop
         Str := Str & Text.Image (Key (Current_Component).Text) & ": "
                & Element (Current_Component).Get.Image;
         Next (Current_Component);
         exit when not Has_Element (Current_Component);
         Str := Str & ", ";
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
      --  the components associated with all discriminants set to their lower
      --  bound.

      if (for all Discr of Self.Discriminant_Types =>
           (Discr.Get.Kind in Discrete_Typ_Range)
           and then As_Discrete_Typ (Discr).Is_Static)
      then
         Str := Str & LF & "Components for discriminants (";
         declare
            Discr_Vals : Discriminant_Values_Maps.Map;
            Filtered_Components : Component_Maps.Map;
            Current_Lit : Integer;
         begin
            Current_Component := Self.Discriminant_Types.First;
            loop
               declare
                  Current_Typ : constant Discrete_Typ'Class :=
                    As_Discrete_Typ (Element (Current_Component));
               begin
                  Current_Lit := Current_Typ.Low_Bound;
                  Discr_Vals.Insert
                  (Key (Current_Component),
                     Current_Lit);
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
