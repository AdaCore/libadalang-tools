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

with TGen.Types.Int_Types;    use TGen.Types.Int_Types;
with TGen.Types.Enum_Types;   use TGen.Types.Enum_Types;
with TGen.Types.Real_Types;   use TGen.Types.Real_Types;
with TGen.Types.Array_Types;  use TGen.Types.Array_Types;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;

package body TGen.Types.Constraints is

   -----------
   -- Image --
   -----------

   function Image (Self : Discrete_Constraint_Value) return String is
     (case Self.Kind is
      when Static => Big_Int.To_String (Self.Int_Val),
      when Discriminant => (+Self.Disc_Name),
      when Non_Static => +Self.Text);

   -----------
   -- Value --
   -----------

   function Value
     (Cst          : Discrete_Constraint_Value;
      Disc_Context : Disc_Value_Map) return Big_Int.Big_Integer
   is
      use Disc_Value_Maps;
      Cur : Cursor;
   begin
      case Cst.Kind is
         when Static =>
            return Cst.Int_Val;
         when Discriminant =>
            Cur := Disc_Context.Find (Cst.Disc_Name);
            if Cur = No_Element then
               raise Program_Error with "unknown discriminant value";
            end if;
            return Get (Element (Cur));
         when others =>
            raise Program_Error with
              "Can't determine value of non static constraint";
      end case;
   end Value;

   -----------
   -- Image --
   -----------

   function Image (Self : Real_Constraint_Value) return String is
     (case Self.Kind is
      when Static => Big_Reals.To_String (Self.Real_Val),
      when Non_Static => +Self.Text);

   -----------
   -- Image --
   -----------

   function Image (Self : Discrete_Range_Constraint) return String is
     ("range " & Image (Self.Low_Bound) & " .. " & Image (Self.High_Bound));

   -----------
   -- Image --
   -----------

   function Image (Self : Real_Range_Constraint) return String is
     ("range " & Image (Self.Low_Bound) & " .. " & Image (Self.High_Bound));

   -----------
   -- Image --
   -----------

   function Image (Self : Digits_Constraint) return String is
     ("digits " & Image (Self.Digits_Value)
      & (if Self.Has_Range then Self.Range_Value.Image else ""));

   -----------
   -- Image --
   -----------

   function Image (Self : Index_Constraints) return String is
      Res : Unbounded_String := To_Unbounded_String ("(");
   begin
      for Index in 1 .. Self.Num_Dims loop
         Res := Res & Self.Constraint_Array (Index).Discrete_Range.Image
                & (if Index = Self.Num_Dims then "" else ", ");
      end loop;
      Res := Res & ")";
      return To_String (Res);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Discriminant_Constraints) return String is
      use Discriminant_Constraint_Maps;
      Res : Unbounded_String := To_Unbounded_String ("(");
      Cur : Discriminant_Constraint_Maps.Cursor := Self.Constraint_Map.First;
   begin
      while Has_Element (Cur) loop
         Res := Res & (+Key (Cur)) & " => " & Image (Element (Cur));
         Next (Cur);
         if Has_Element (Cur) then
            Res := Res & ", ";
         end if;
      end loop;
      Res := Res & ")";
      return To_String (Res);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Self : Anonymous_Typ) return String is
   begin
      return Typ (Self.Named_Ancestor.Unchecked_Get.all).Image
             & " " & Self.Subtype_Constraints.Image;
   end Image;

   ------------------
   -- As_Named_Typ --
   ------------------

   function As_Named_Typ (Self : Anonymous_Typ) return SP.Ref is
      Res : SP.Ref;
      Cst : Constraint'Class renames Self.Subtype_Constraints.all;
   begin
      case Self.Named_Ancestor.Get.Kind is
         when Signed_Int_Kind =>
            if Cst.Static then
               Res.Set (Signed_Int_Typ'
                 (Is_Static   => True,
                  Range_Value =>
                    (Min => Discrete_Range_Constraint
                              (Cst).Low_Bound.Int_Val,
                     Max => Discrete_Range_Constraint
                              (Cst).High_Bound.Int_Val),
                  others      => <>));
            else
               Res.Set (Signed_Int_Typ'
                 (Is_Static => False,
                  others    => <>));
            end if;
         when Mod_Int_Kind =>
            if Cst.Static then
               Res.Set (Mod_Int_Typ'
                 (Is_Static => True,
                  Range_Value =>
                        (Discrete_Range_Constraint (Cst)
                           .High_Bound
                           .Int_Val,
                         Discrete_Range_Constraint (Cst)
                           .Low_Bound
                           .Int_Val),
                  Mod_Value =>
                    Discrete_Range_Constraint
                      (Cst).High_Bound.Int_Val,
                  others => <>));
            else
               Res.Set (Mod_Int_Typ'
                 (Is_Static => False,
                  others    => <>));
            end if;
         when Char_Kind =>
            if Cst.Static then
               Res.Set
                 (Char_Typ'(Is_Static   => True,
                            Has_Range   => True,
                            Range_Value =>
                              Discrete_Range_Constraint (Cst),
                            others      => <>));
            else
               Res.Set
                 (Char_Typ'(Is_Static   => False,
                            Has_Range   => True,
                            Range_Value =>
                              Discrete_Range_Constraint (Cst),
                            others      => <>));
            end if;
         when Enum_Kind =>
            if Cst.Static then
               declare
                  use Enum_Literal_Maps;
                  New_Lit_Set  : Enum_Literal_Maps.Map;
                  Old_Enum_Cur : constant Enum_Literal_Maps.Cursor :=
                    As_Other_Enum_Typ (Self.Named_Ancestor).Literals.First;

                  Min : constant Big_Integer :=
                    Discrete_Range_Constraint (Cst).Low_Bound.Int_Val;
                  Max : constant Big_Integer :=
                    Discrete_Range_Constraint (Cst).High_Bound.Int_Val;
               begin
                  while Has_Element (Old_Enum_Cur) loop
                     if Min <= Key (Old_Enum_Cur)
                       and then Key (Old_Enum_Cur) <= Max
                     then
                        New_Lit_Set.Insert
                          (Key (Old_Enum_Cur), Element (Old_Enum_Cur));
                     end if;
                  end loop;
                  Res.Set (Other_Enum_Typ'(Is_Static => True,
                                           Literals  => New_Lit_Set,
                                           others    => <>));
               end;
            else
               Res.Set (Other_Enum_Typ'(Is_Static => False, others => <>));
            end if;
         when Float_Kind =>
            if Cst.Static then
               if Cst in Real_Range_Constraint then
                  Res.Set (Float_Typ'
                    (Is_Static    => True,
                     Has_Range    => True,
                     Digits_Value =>
                       As_Float_Typ (Self.Named_Ancestor).Digits_Value,
                     Range_Value  =>
                       (Min => Real_Range_Constraint (Cst)
                               .Low_Bound.Real_Val,
                        Max => Real_Range_Constraint (Cst)
                               .High_Bound.Real_Val),
                     others       => <>));
               else
                  if Digits_Constraint (Cst).Has_Range
                  then
                     Res.Set (Float_Typ'
                       (Is_Static    => True,
                        Has_Range    => True,
                        Digits_Value => Big_Int.To_Integer
                          (Digits_Constraint (Cst).Digits_Value.Int_Val),
                        Range_Value  =>
                          (Min => Digits_Constraint (Cst).Range_Value
                                  .Low_Bound.Real_Val,
                           Max => Digits_Constraint (Cst).Range_Value
                                  .High_Bound.Real_Val),
                        others       => <>));
                  else
                     if As_Float_Typ (Self.Named_Ancestor).Is_Static
                       and then As_Float_Typ (Self.Named_Ancestor).Has_Range
                     then
                        Res.Set (Float_Typ'
                          (Is_Static    => True,
                           Has_Range    => True,
                           Digits_Value => Big_Int.To_Integer
                             (Digits_Constraint (Cst).Digits_Value.Int_Val),
                           Range_Value  =>
                             As_Float_Typ (Self.Named_Ancestor).Range_Value,
                           others       => <>));
                     elsif As_Float_Typ (Self.Named_Ancestor).Is_Static then
                        Res.Set (Float_Typ'
                          (Is_Static    => True,
                           Has_Range    => False,
                           Digits_Value => Big_Int.To_Integer
                             (Digits_Constraint (Cst).Digits_Value.Int_Val),
                           others       => <>));
                     else
                        Res := Self.Named_Ancestor;
                     end if;
                  end if;
               end if;
            else
               Res.Set (Float_Typ'
                 (Is_Static => False, Has_Range  => False, others => <>));
            end if;
         when Fixed_Kind =>
            if Cst.Static
              and then Cst in Real_Range_Constraint
              and then As_Ordinary_Fixed_Typ (Self.Named_Ancestor).Is_Static
            then
               Res.Set (Ordinary_Fixed_Typ'
                 (Is_Static   => True,
                  Delta_Value =>
                    As_Ordinary_Fixed_Typ (Self.Named_Ancestor).Delta_Value,
                  Range_Value =>
                    (Min => Real_Range_Constraint (Cst)
                            .Low_Bound.Real_Val,
                     Max => Real_Range_Constraint (Cst)
                            .High_Bound.Real_Val),
                  others => <>));
            else
               Res.Set
                 (Ordinary_Fixed_Typ'(Is_Static  => False, others => <>));
            end if;
         when Array_Typ_Range =>
            Res.Set (Constrained_Array_Typ'
              (Num_Dims          =>
                 As_Unconstrained_Array_Typ (Self.Named_Ancestor).Num_Dims,
               Static_Gen        =>
                 (As_Unconstrained_Array_Typ (Self.Named_Ancestor).Static_Gen
                 and then Self.Subtype_Constraints.Static),
               Component_Type    =>
                 As_Unconstrained_Array_Typ
                   (Self.Named_Ancestor).Component_Type,
               Index_Types       =>
                 As_Unconstrained_Array_Typ (Self.Named_Ancestor).Index_Types,
               Index_Constraints =>
                 Index_Constraints (Cst).Constraint_Array,
               others => <>));
         when Disc_Record_Kind =>
            Res.Set (Discriminated_Record_Typ'
              (Constrained             => True,
               Component_Types         =>
                 As_Discriminated_Record_Typ (Self.Named_Ancestor)
                 .Component_Types.Copy,
               Mutable                 =>
                 As_Discriminated_Record_Typ (Self.Named_Ancestor).Mutable,
               Discriminant_Types      =>
                 As_Discriminated_Record_Typ (Self.Named_Ancestor)
                 .Discriminant_Types.Copy,
               Variant                 => Clone
                 (As_Discriminated_Record_Typ (Self.Named_Ancestor).Variant),
               Discriminant_Constraint =>
                 Discriminant_Constraints (Cst).Constraint_Map.Copy,
               Static_Gen              =>
                 As_Record_Typ (Self.Named_Ancestor).Static_Gen
                 and then Self.Subtype_Constraints.Static,
               others                  => <>));
         when others =>
            Res.Set (Unsupported_Typ'
              (Reason => +"Unknown named ancestor type kind", others => <>));
      end case;
      Res.Get.Name := Self.Named_Ancestor.Get.Name;
      Res.Get.Last_Comp_Unit_Idx := Self.Last_Comp_Unit_Idx;
      Res.Get.Fully_Private := Self.Named_Ancestor.Get.Fully_Private;
      return Res;
   end As_Named_Typ;

   ------------
   -- Encode --
   ------------

   function Encode
     (Self : Anonymous_Typ; Val : JSON_Value) return JSON_Value is
     (Self.Named_Ancestor.Get.Encode (Val));

   ----------------------
   -- Default_Strategy --
   ----------------------

   function Default_Strategy (Self : Anonymous_Typ) return Strategy_Type'Class
   is
   begin
      return Self.As_Named_Typ.Get.Default_Strategy;
   end Default_Strategy;

   overriding function Default_Enum_Strategy
     (Self : Anonymous_Typ) return Enum_Strategy_Type'Class
   is
   begin
      return Self.As_Named_Typ.Get.Default_Enum_Strategy;
   end Default_Enum_Strategy;

   ------------------
   -- Free_Content --
   ------------------

   procedure Free_Content (Self : in out Anonymous_Typ) is
   begin
      Free (Self.Subtype_Constraints);
   end Free_Content;

   -----------
   -- Image --
   -----------

   function Image (Self : Instance_Typ) return String is
   begin
      return Self.Orig_Typ.Unchecked_Get.Image;
   end Image;
end TGen.Types.Constraints;
