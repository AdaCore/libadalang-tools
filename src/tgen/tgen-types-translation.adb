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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;

with Langkit_Support.Text;

with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

with TGen.Int_Types;    use TGen.Int_Types;
with TGen.Real_Types;   use TGen.Real_Types;
with TGen.Enum_Types;   use TGen.Enum_Types;
with TGen.Array_Types;  use TGen.Array_Types;
with TGen.Record_Types; use TGen.Record_Types;

package body TGen.Types.Translation is
   use LAL;

   Translation_Error : exception;

   Verbose_Diag : Boolean := False;
   package Text renames Langkit_Support.Text;

   package Translation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada_Node,
      Element_Type    => TGen.Types.SP.Ref,
      Hash            => LAL.Hash,
      Equivalent_Keys => LAL."=",
      "="             => TGen.Types.SP."=");

   Translation_Cache : Translation_Maps.Map;
   --  Cache used for the memoization of Translate.

   Cache_Hits : Natural := 0;
   Cache_Miss : Natural := 0;
   --  Stats for the cache

   function Translate_Internal
     (N       : LAL.Base_Type_Decl;
      Verbose : Boolean := False) return Translation_Result;
   --  Actually translates the Base_Type_Decl. Translate is simply a
   --  memoization wrapper.

   function "+" (Str : String) return Unbounded_String is
     (To_Unbounded_String (Str));

   function Translate_Int_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Int_Type;

   function Translate_Enum_Decl
     (Decl           : Base_Type_Decl;
      Root_Enum_Decl : Base_Type_Decl;
      Type_Name      : Defining_Name)
      return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Enum_Type;

   function Translate_Float_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Float_Type;

   function Translate_Ordinary_Fixed_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Fixed_Point;

   function Translate_Decimal_Fixed_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Fixed_Point;

   procedure Translate_Float_Range
     (Decl     :     Base_Type_Decl; Has_Range : out Boolean;
      Min, Max : out Long_Float);

   function Translate_Array_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Array_Type;

   function Translate_Record_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result with
      Pre => Decl.P_Is_Record_Type;

   procedure Apply_Record_Subtype_Decl
     (Decl : Subtype_Indication;
      Res  : in out Discriminated_Record_Typ) with
     Pre => Kind (Decl.Parent) in Ada_Subtype_Decl_Range
           and then Res.Constrained;
   --  Record the discriminant constraints of Decl in Res. For this, the type
   --  on which you want to apply constraints must be able to accept them

   function Apply_Record_Derived_Type_Decl
     (Decl : Type_Decl'Class;
      From : Discriminated_Record_Typ) return Discriminated_Record_Typ with
     Pre => Kind (Decl.F_Type_Def) in Ada_Derived_Type_Def_Range
           and then From.Constrained;
   --  Apply the effects of the record type derivation defined in Decl.
   --  If any discriminant constraints are present, this filters out the
   --  incompatible shapes, and renames discriminant which correspond
   --  between the ancestor type and the child type.

   function Record_Constrained
   (Decl : Base_Type_Decl;
    Root : Base_Type_Decl) return Boolean;
   --  Returns True if Decl has discriminants constraints at some stage in the
   --  chain of subtype definitions / type derivations.

   ------------------------
   -- Translate_Int_Decl --
   ------------------------

   function Translate_Int_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is
      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max : constant Integer :=
        Integer'Value (High_Bound (Rang).P_Eval_As_Int.Image);
      --  Since Decl is a static subtype, its bounds are also static
      --  expressions according to RM 4.9(26/2).
      Res : Translation_Result (Success => True);
   begin
      if Is_Null (Low_Bound (Rang)) then
         Res.Res.Set
         (Mod_Int_Typ'(Is_Static => True,
                       Name      => Type_Name,
                       Mod_Value => Max));
      else
         declare
            Min : constant Integer :=
              Integer'Value (Low_Bound (Rang).P_Eval_As_Int.Image);
         begin
            Res.Res.Set
              (Signed_Int_Typ'(Is_Static   => True,
                               Name        => Type_Name,
                               Range_Value => (Min => Min, Max => Max)));
         end;
      end if;
      return Res;
   end Translate_Int_Decl;

   -------------------------
   -- Translate_Enum_Decl --
   -------------------------

   function Translate_Enum_Decl
     (Decl           : Base_Type_Decl;
      Root_Enum_Decl : Base_Type_Decl;
      Type_Name      : Defining_Name)
      return Translation_Result
   is
      Enum_Lits : Enum_Literal_Maps.Map;

      Index : Natural := 0;

      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max, Min : Natural;

   begin
      for Literal of Root_Enum_Decl.As_Type_Decl.F_Type_Def.As_Enum_Type_Def
        .F_Enum_Literals
      loop
         Enum_Lits.Insert (Index, Literal.F_Name);
         Index := Index + 1;
      end loop;

      if (not Is_Null (High_Bound (Rang)))
        and then not Is_Null (Low_Bound (Rang))
      then
         Max := Natural'Value (High_Bound (Rang).P_Eval_As_Int.Image);
         Min := Natural'Value (Low_Bound (Rang).P_Eval_As_Int.Image);
         for Pos in Enum_Lits.First_Key .. Min - 1 loop
            Enum_Lits.Delete (Pos);
         end loop;

         for Pos in Max + 1 .. Enum_Lits.Last_Key loop
            Enum_Lits.Delete (Pos);
         end loop;
      end if;

      return Res : Translation_Result (Success => True) do
         Res.Res.Set
           (Other_Enum_Typ'(Is_Static => True,
                            Name      => Type_Name,
                            Literals  => Enum_Lits));
      end return;
   end Translate_Enum_Decl;

   --------------------------
   -- Translate_Float_Decl --
   --------------------------

   function Translate_Float_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is

      procedure Find_Digits
        (Decl : Base_Type_Decl; Digits_Value : out Natural);
      --  Determine the digits value of Decl.

      procedure Find_Digits (Decl : Base_Type_Decl; Digits_Value : out Natural)
      is
         Parent_Type : Subtype_Indication;
         Constraints : Digits_Constraint;
      begin
         if Decl = Decl.P_Root_Type then

            --  Decl is the root type decl, so we only need to translate the
            --  type definition.

            Digits_Value :=
              Natural'Value
                (Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def
                   .F_Num_Digits
                   .P_Eval_As_Int
                   .Image);
            return;
         end if;

         --  Decl is either a subtype decl or a derived type decl. Check if
         --  there are constraints associated with this decl.

         if Kind (Decl) in Ada_Subtype_Decl_Range then
            Parent_Type := Decl.As_Subtype_Decl.F_Subtype;

         elsif Kind (Decl.As_Type_Decl.F_Type_Def) in
                 Ada_Derived_Type_Def_Range
         then
            Parent_Type :=
              Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                .F_Subtype_Indication;
         else
            raise Translation_Error with
              "Unexpected type decl for a floating point type declaration: "
              & Decl.Image;
         end if;
         if Is_Null (Parent_Type.F_Constraint) then

            --  If there aren't any constraints in the subtype indication,
            --  try to find the type properites on the referenced type.

            Find_Digits (Parent_Type.P_Designated_Type_Decl, Digits_Value);
         else

            --  Otherwise, analyze the type constraint
            case Kind (Parent_Type.F_Constraint) is
               when Ada_Range_Constraint_Range =>
                  Find_Digits
                    (Parent_Type.P_Designated_Type_Decl, Digits_Value);
                  return;
               when Ada_Digits_Constraint_Range =>
                  Constraints := Parent_Type.F_Constraint.As_Digits_Constraint;
                  Digits_Value :=
                    Natural'Value (Constraints.F_Digits.P_Eval_As_Int.Image);
               when others =>
                  raise Translation_Error
                    with "Unexpected kind of" &
                    " constraint for float subtype indication: " &
                    Kind_Name (Constraints);
            end case;
         end if;
      end Find_Digits;

      Digits_Value : Natural := 0;
      Has_Range    : Boolean;
      Min, Max     : Long_Float;

      Res : Translation_Result (Success => True);

   --  Start processing for Translate_Float_Decl

   begin
      Find_Digits (Decl, Digits_Value);
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      if Has_Range then
         Res.Res.Set
           (Float_Typ'(Is_Static    => True,
                       Has_Range    => True,
                       Name         => Type_Name,
                       Digits_Value => Digits_Value,
                       Range_Value  => (Min => Min, Max => Max)));
      else
         Res.Res.Set
           (Float_Typ'(Is_Static    => True,
                       Has_Range    => False,
                       Name         => Type_Name,
                       Digits_Value => Digits_Value));
      end if;
      return Res;
   exception
      when Exc : Translation_Error =>
         if Verbose_Diag then
            Put_Line
              ("Warning: could not determine static properties of" & " type" &
               Decl.Image & " : " & Ada.Exceptions.Exception_Message (Exc));
         end if;
         Res.Res.Set
           (Float_Typ'(Is_Static => False,
                       Has_Range => False,
                       Name      => Type_Name));
         return Res;
   end Translate_Float_Decl;

   -----------------------------------
   -- Translate_Ordinary_Fixed_Decl --
   -----------------------------------

   function Translate_Ordinary_Fixed_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is
      Min, Max    : Long_Float;
      Delta_Value : Long_Float;
      Has_Range   : Boolean;

      procedure Find_Delta
        (Decl : Base_Type_Decl; Delta_Value : out Long_Float);
      --  Travese the type hierachy from the bottom to find the inner most
      --  delta value of Decl.

      procedure Find_Delta
        (Decl : Base_Type_Decl; Delta_Value : out Long_Float)
      is
         Delta_Expr : Expr;
         --  Expr corresponding to the delta value, which will later be
         --  statically evaluated.

         Subtype_Ind : Subtype_Indication;
         --  Convininece variable to hold constraints to shorten then length of
         --  chained Libadalang dot calls.

         Subtype_Constraint : Constraint;
         --  Convininece variable to hold constraints to shorten then length of
         --  chained Libadalang dot calls.
      begin
         if Decl = Decl.P_Root_Type then

            --  First, the case where Decl is the root type, and thus we have a
            --  Ordinary_Fixed_Point_Def.

            pragma Assert
              (Kind (Decl.As_Type_Decl.F_Type_Def) in
               Ada_Ordinary_Fixed_Point_Def_Range);
            Delta_Expr :=
              Decl.As_Type_Decl.F_Type_Def.As_Ordinary_Fixed_Point_Def.F_Delta;

         elsif Kind (Decl) in Ada_Subtype_Decl_Range
           or else
           (Kind (Decl) in Ada_Type_Decl_Range
            and then Kind (Decl.As_Type_Decl.F_Type_Def) =
              Ada_Derived_Type_Def)
         then

            --  Case of a subtype decl or derived type decl, look at the
            --  subtype indication for a constraint, or look at the delta of
            --  the parent subtype.

            if Kind (Decl) in Ada_Subtype_Decl_Range then
               Subtype_Ind := Decl.As_Subtype_Decl.F_Subtype;
            else
               Subtype_Ind :=
                 Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                   .F_Subtype_Indication;
            end if;

            if Is_Null (Subtype_Ind.F_Constraint) then
               Find_Delta
                 (Subtype_Ind.F_Name.P_Name_Designated_Type, Delta_Value);
               return;
            end if;
            Subtype_Constraint := Subtype_Ind.F_Constraint;
            case Kind (Subtype_Constraint) is
               when Ada_Delta_Constraint_Range =>
                  Delta_Expr :=
                    Subtype_Constraint.As_Delta_Constraint.F_Digits;

               when Ada_Range_Constraint_Range =>

                  --  If we only have range constraints then look for the delta
                  --  value on the subtype designated by the subtype
                  --  indication.

                  Find_Delta
                    (Subtype_Ind.F_Name.P_Name_Designated_Type, Delta_Value);
                  return;

               when others =>
                  raise Translation_Error with
                    "Unexpected constraint kind for a ordinary fixed point"
                    & " subtype declaration: "
                    & Kind_Name (Subtype_Constraint);
            end case;
         else
            raise Translation_Error with
              "Unexpected base type decl for a ordinary fixed point decl: "
              & Image (Decl);
         end if;

         declare
            Delta_Eval : constant Eval_Result := Expr_Eval (Delta_Expr);
         begin
            if Delta_Eval.Kind /= Real then
               raise Translation_Error with "wrong eval type for delta value";
            end if;
            Delta_Value := Delta_Eval.Real_Result;
         end;
      end Find_Delta;

   --  Start of processing for Translate_Ordinary_Fixed_Decl
   begin
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      pragma Assert (Has_Range);
      Find_Delta (Decl, Delta_Value);
      return Res : Translation_Result (Success => True) do
         Res.Res.Set
           (Ordinary_Fixed_Typ'(Is_Static   => True,
                                Name        => Type_Name,
                                Delta_Value => Delta_Value,
                                Range_Value => (Min => Min, Max => Max)));
      end return;
   exception
      when Exc : Translation_Error =>

         --  In case of translation error, return a non-static type,
         --  but print the information if verbose diagnostics are required.
         if Verbose_Diag then
            Put_Line
              ("Warning: could not determine static properties of" & " type" &
               Decl.Image & " : " & Ada.Exceptions.Exception_Message (Exc));
         end if;
         return Res : Translation_Result (Success => True) do
            Res.Res.Set
              (Ordinary_Fixed_Typ'(Is_Static => False,
                                   Name      => Type_Name));
         end return;
   end Translate_Ordinary_Fixed_Decl;

   ----------------------------------
   -- Translate_Decimal_Fixed_Decl --
   ----------------------------------

   function Translate_Decimal_Fixed_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is
   begin
      --  ??? Actually implement this, should be similar to Ordinary
      --  fixed points
      return Res : Translation_Result (Success => True) do
         Res.Res.Set
           (Decimal_Fixed_Typ'(Is_Static => False,
                               Has_Range => False,
                               Name      => Type_Name));
      end return;
   end Translate_Decimal_Fixed_Decl;

   ---------------------------
   -- Translate_Float_Range --
   ---------------------------

   procedure Translate_Float_Range
     (Decl     :     Base_Type_Decl; Has_Range : out Boolean;
      Min, Max : out Long_Float)
   is
      Root           : constant Type_Decl := Decl.P_Root_Type.As_Type_Decl;
      Parent_Type    : Subtype_Indication;
      Range_Spec_Val : Range_Spec         := No_Range_Spec;
   begin
      if Decl = Root then

         --  Decl is the root type, it is a type decl

         case Kind (Decl.As_Type_Decl.F_Type_Def) is
            when Ada_Floating_Point_Def_Range =>
               Range_Spec_Val :=
                 Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def.F_Range;
            when Ada_Ordinary_Fixed_Point_Def_Range =>
               Range_Spec_Val :=
                 Decl.As_Type_Decl.F_Type_Def.As_Ordinary_Fixed_Point_Def
                   .F_Range;
            when others =>
               raise Translation_Error
                 with "Expected Real type def for decl but got" &
                 Kind_Name (Decl.As_Type_Decl.F_Type_Def);
         end case;

         if Is_Null (Range_Spec_Val) then
            Has_Range := False;
            Min       := 0.0;
            Max       := 0.0;
            return;
         end if;

      else
         if Kind (Decl) in Ada_Type_Decl_Range
           and then Kind (Decl.As_Type_Decl.F_Type_Def) in
                      Ada_Derived_Type_Def_Range
         then

            --  Decl is a derived type decl, look at the constraints in the
            --  subtype indication

            Parent_Type :=
              Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                .F_Subtype_Indication;
         elsif Kind (Decl) in Ada_Subtype_Decl_Range then

            --  Same but for a subtype declaration

            Parent_Type := Decl.As_Subtype_Decl.F_Subtype;
         else
            raise Translation_Error
              with "Unexpected base type decl for a float type" &
              Kind_Name (Decl);
         end if;
         if Is_Null (Parent_Type.F_Constraint) then
            Translate_Float_Range
              (Parent_Type.P_Designated_Type_Decl, Has_Range, Min, Max);
            return;
         end if;

         --  Here we know the subtype indication had constraints.
         --  Now see if it has Range constraints and get it's range spec.
         --  Otherwise inspect the parent type to see if it has a range
         --  constraint defined.

         case Kind (Parent_Type.F_Constraint) is
            when Ada_Range_Constraint_Range =>
               Range_Spec_Val :=
                 Parent_Type.F_Constraint.As_Range_Constraint.F_Range;
            when Ada_Digits_Constraint_Range =>
               if Is_Null
                   (Parent_Type.F_Constraint.As_Digits_Constraint.F_Range)
               then
                  Translate_Float_Range
                    (Parent_Type.P_Designated_Type_Decl, Has_Range, Min, Max);
                  return;
               end if;
               Range_Spec_Val :=
                 Parent_Type.F_Constraint.As_Digits_Constraint.F_Range;
            when Ada_Delta_Constraint_Range =>
               if Is_Null
                   (Parent_Type.F_Constraint.As_Delta_Constraint.F_Range)
               then
                  Translate_Float_Range
                    (Parent_Type.P_Designated_Type_Decl, Has_Range, Min, Max);
                  return;
               end if;
               Range_Spec_Val :=
                 Parent_Type.F_Constraint.As_Delta_Constraint.F_Range;
            when others =>
               raise Translation_Error
                 with "Unexpected kind of constraint for a real type " &
                 Kind_Name (Parent_Type.F_Constraint);
         end case;
      end if;

      --  At this point we should have a non null range constraint spec, lets
      --  analyze it.

      pragma Assert (not Is_Null (Range_Spec_Val));
      case Kind (Range_Spec_Val.F_Range) is

         --  According to RM 3.5 (3) a range constraint can only be of the form
         --  "Min .. Max" or "Name'Range", and assume we are analyzing a well
         --  formed AST.

         when Ada_Attribute_Ref_Range =>
            Translate_Float_Range
              (Range_Spec_Val.F_Range.As_Attribute_Ref.F_Prefix
                 .P_Referenced_Decl
                 .As_Base_Type_Decl,
               Has_Range, Min, Max);

         when Ada_Bin_Op_Range =>
            pragma Assert
              (Range_Spec_Val.F_Range.As_Bin_Op.F_Left.P_Is_Static_Expr);
            pragma Assert
              (Range_Spec_Val.F_Range.As_Bin_Op.F_Right.P_Is_Static_Expr);
            declare
               Min_Eval : constant Eval_Result :=
                 Expr_Eval (Range_Spec_Val.F_Range.As_Bin_Op.F_Left);
               Max_Eval : constant Eval_Result :=
                 Expr_Eval (Range_Spec_Val.F_Range.As_Bin_Op.F_Right);
            begin
               if Min_Eval.Kind /= Real or else Max_Eval.Kind /= Real then
                  raise Translation_Error with
                    "Wrong type of static eval for real range constraint.";
               end if;
               Has_Range := True;
               Min       := Min_Eval.Real_Result;
               Max       := Max_Eval.Real_Result;
            end;
         when others =>
            raise Translation_Error
              with "Unexpected expression kind for real range constraint: " &
              Kind_Name (Range_Spec_Val.F_Range);
      end case;
   end Translate_Float_Range;

   --------------------------
   -- Translate_Array_Decl --
   --------------------------

   function Translate_Array_Decl
    (Decl       : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is
      function Translate_Constrained
        (Decl      : Base_Type_Decl;
         Type_Name : Defining_Name) return Translation_Result;

      function Translate_Unconstrained
        (Def : Array_Type_Def;
         Type_Name : Defining_Name) return Translation_Result;

      ---------------------------
      -- Translate_Constrained --
      ---------------------------

      function Translate_Constrained
         (Decl      : Base_Type_Decl;
          Type_Name : Defining_Name) return Translation_Result
      is
         Cmp_Typ_Def         : Component_Def;

         type Local_Ada_Node_List is array (Positive range <>) of Ada_Node;

         Num_Indices         : Natural := 0;
      begin
         --  Here we either have a constrained array type decl, or a subtype
         --  decl that constrains a previous unconstrained array type decl.

         while not Is_Null (Decl.P_Index_Type (Num_Indices)) loop
            Num_Indices := Num_Indices + 1;
         end loop;

         declare

            Constraint_Nodes : Local_Ada_Node_List (1 .. Num_Indices);
            Current_Index : Positive := 1;

         begin

            if Kind (Decl) in Ada_Type_Decl_Range then
               Cmp_Typ_Def :=
               Decl.As_Type_Decl.F_Type_Def.As_Array_Type_Def.F_Component_Type;

               for Constraint of Decl.As_Type_Decl.F_Type_Def.As_Array_Type_Def
                                .F_Indices.As_Constrained_Array_Indices.F_List
               loop
                  Constraint_Nodes (Current_Index) := Constraint.As_Ada_Node;
                  Current_Index := Current_Index + 1;
               end loop;
            else
               Cmp_Typ_Def :=
                 Decl.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl
                 .As_Type_Decl.F_Type_Def.As_Array_Type_Def.F_Component_Type;
               if Kind (Decl.As_Subtype_Decl.F_Subtype.F_Constraint)
                  in Ada_Index_Constraint_Range
               then
                  for Constraint of Decl.As_Subtype_Decl.F_Subtype.F_Constraint
                                    .As_Index_Constraint.F_Constraints
                  loop
                     Constraint_Nodes (Current_Index) :=
                       Constraint.As_Ada_Node;
                     Current_Index := Current_Index + 1;
                  end loop;
               else
                  pragma Assert
                  (Kind (Decl.As_Subtype_Decl.F_Subtype.F_Constraint)
                     in Ada_Discriminant_Constraint_Range);
                  for Assoc of Decl.As_Subtype_Decl.F_Subtype.F_Constraint
                               .As_Discriminant_Constraint.F_Constraints
                  loop
                     Constraint_Nodes (Current_Index) :=
                       Assoc.As_Discriminant_Assoc.F_Discr_Expr.As_Ada_Node;
                     Current_Index := Current_Index + 1;
                  end loop;
               end if;
            end if;

            declare
               Res_Typ : Constrained_Array_Typ (Num_Indices);

               Component_Typ : constant Translation_Result :=
               Translate (Cmp_Typ_Def.F_Type_Expr, Verbose_Diag);
               --  This ignores any constraints on the element type that may
               --  appear in the component definition.

               Index_Typ                      : Base_Type_Decl;
               Has_Constraints                : Boolean;
               Constraints_Static             : Boolean;
               Range_Exp                      : Expr;
               Constraint_Min, Constraint_Max : Integer;

               Failure_Reason : Unbounded_String;
            begin

               Current_Index := 1;
               if not Component_Typ.Success then
                  return (Success     => False,
                        Diagnostics => "Failed to translate component type of"
                                       & " array decl : "
                                       & Component_Typ.Diagnostics);
               end if;
               Res_Typ.Component_Type := Component_Typ.Res;

               for Constraint of Constraint_Nodes loop
                  case Kind (Constraint) is
                     when Ada_Subtype_Indication_Range =>
                        Index_Typ := Constraint.As_Subtype_Indication
                                     .P_Designated_Type_Decl;

                        if Is_Null
                             (Constraint.As_Subtype_Indication.F_Constraint)
                        then
                           Has_Constraints := False;
                        elsif not Constraint.As_Subtype_Indication.F_Constraint
                                 .As_Range_Constraint.F_Range.F_Range
                                 .P_Is_Static_Expr
                        then
                           Has_Constraints := True;
                           Constraints_Static := False;
                        elsif Kind
                                (Constraint.As_Subtype_Indication.F_Constraint
                                 .As_Range_Constraint.F_Range.F_Range)
                                 in Ada_Attribute_Ref_Range | Ada_Bin_Op_Range
                        then
                           Range_Exp :=
                           Constraint.As_Subtype_Indication.F_Constraint
                                       .As_Range_Constraint.F_Range.F_Range;

                           Has_Constraints := True;
                           Constraints_Static := True;

                        end if;
                     when Ada_Bin_Op_Range =>
                        Index_Typ :=
                        Constraint.As_Bin_Op.F_Left.P_Expression_Type;
                        Has_Constraints := True;
                        if Constraint.As_Expr.P_Is_Static_Expr then
                           Constraints_Static := True;
                           Range_Exp := Constraint.As_Expr;
                        else
                           Constraints_Static := False;
                        end if;

                     when Ada_Attribute_Ref_Range =>
                        Index_Typ :=
                        Constraint.As_Attribute_Ref.F_Prefix
                        .P_Name_Designated_Type;
                        Has_Constraints := True;
                        if Constraint.As_Expr.P_Is_Static_Expr then
                           Constraints_Static := True;
                           Range_Exp := Constraint.As_Expr;
                        else
                           Constraints_Static := False;
                        end if;
                     when Ada_Identifier_Range =>
                        Index_Typ := Constraint.As_Identifier.P_Referenced_Decl
                                     .As_Base_Type_Decl;
                        Has_Constraints := False;
                     when others =>
                        Failure_Reason :=
                        +"Unexpected constraint kind for a constrained array: "
                        & Kind_Name (Constraint);
                        goto Failed_UC_Translation;
                  end case;

                  declare
                     Index_Trans : constant Translation_Result :=
                     Translate (Index_Typ, Verbose_Diag);
                  begin
                     if not Index_Trans.Success then
                        Failure_Reason :=
                        "Failed to translate type of the index dimention"
                        & Current_Index'Image & ": " & Index_Trans.Diagnostics;
                        goto Failed_UC_Translation;
                     end if;
                     Res_Typ.Index_Types (Current_Index) := Index_Trans.Res;
                  end;

                  if Has_Constraints and then Constraints_Static then
                     --  We should only encouter either a Bin Op (A .. B) or a
                     --  range attribute reference according to RM 3.5 (2).

                     if Kind (Range_Exp) in Ada_Bin_Op_Range then
                        Constraint_Min := Integer'Value
                        (Range_Exp.As_Bin_Op.F_Left.P_Eval_As_Int.Image);
                        Constraint_Max := Integer'Value
                        (Range_Exp.As_Bin_Op.F_Right.P_Eval_As_Int.Image);
                     else
                        Constraint_Min := Integer'Value
                        (Low_Bound (Range_Exp.As_Attribute_Ref.F_Prefix
                           .P_Name_Designated_Type.P_Discrete_Range)
                           .P_Eval_As_Int.Image);
                        Constraint_Min := Integer'Value
                        (High_Bound (Range_Exp.As_Attribute_Ref.F_Prefix
                           .P_Name_Designated_Type.P_Discrete_Range)
                           .P_Eval_As_Int.Image);
                     end if;
                  end if;

                  if not Has_Constraints then
                     Res_Typ.Index_Constraints (Current_Index) :=
                     (Present => False);
                  elsif not Constraints_Static then
                     Res_Typ.Index_Constraints (Current_Index) :=
                     (Present        => True,
                        Discrete_range =>
                        (Low_Bound  => (Kind => Non_Static),
                           High_Bound => (Kind => Non_Static)));
                  else
                     Res_Typ.Index_Constraints (Current_Index) :=
                     (Present        => True,
                        Discrete_Range =>
                        (Low_Bound  => (Kind    => Static,
                                          Int_Val => Constraint_Min),
                           High_Bound => (Kind    => Static,
                                          Int_Val => Constraint_Max)));
                  end if;
                  Current_Index := Current_Index + 1;
               end loop;

               Res_Typ.Name := Type_Name;

               return Res : Translation_Result (Success => True) do
                  Res.Res.Set (Res_Typ);
               end return;

               <<Failed_UC_Translation>>
               return (Success => False, Diagnostics => Failure_Reason);
            end;
         end;
      end Translate_Constrained;

      -----------------------------
      -- Translate_Unconstrained --
      -----------------------------

      function Translate_Unconstrained
        (Def       : Array_Type_Def;
         Type_Name : Defining_Name) return Translation_Result
      is
         Indices_List : constant Unconstrained_Array_Index_List :=
           Def.F_Indices.As_Unconstrained_Array_Indices.F_Types;
         Num_Indices  : constant Positive := Indices_List.Last_Child_Index;

         Failure_Reason : Unbounded_String;

         Element_Type : constant Translation_Result :=
           Translate (Def.F_Component_Type.F_Type_Expr, Verbose_Diag);
           --  This ignores any constraints on the element type that may appear
           --  in the component definition.

         Current_Index_Type : Positive := 1;

         Res_Typ : Unconstrained_Array_Typ (Num_Indices);

      begin
         if not Element_Type.Success then
            return
              (Success     => False,
               Diagnostics => "Could not translate element type for array: "
                              & Element_Type.Diagnostics);
         end if;
         Res_Typ.Component_Type := Element_Type.Res;
         for Index of Indices_List loop
            declare
               Index_Type : constant Translation_Result :=
                 Translate
                   (Index.F_Subtype_Indication.As_Type_Expr, Verbose_Diag);
            begin
               if Index_Type.Success then
                  Res_Typ.Index_Types (Current_Index_Type) := Index_Type.Res;
                  Current_Index_Type := Current_Index_Type + 1;
               else
                  Failure_Reason := Index_Type.Diagnostics;
                  goto Failed_Translation;
               end if;
            end;
         end loop;

         Res_Typ.Name := Type_Name;
         return Res : Translation_Result (Success => True) do
            Res.Res.Set (Res_Typ);
         end return;

         <<Failed_Translation>>
         return (Success => False,
                 Diagnostics => "Failed to translate the type of the"
                                & Current_Index_Type'Image & "index dimention"
                                & ": " & Failure_Reason);

      end Translate_Unconstrained;

   --  Start of processing for Translate_Array_Decl

   begin
      case Kind (Decl) is
         when Ada_Subtype_Decl_Range =>
            return Translate_Constrained (Decl, Type_Name);

         when Ada_Type_Decl_Range =>
            case Kind (Decl.As_Type_Decl.F_Type_Def
                       .As_Array_Type_Def.F_Indices) is
               when Ada_Constrained_Array_Indices_Range =>
                  return Translate_Constrained (Decl, Type_Name);

               when Ada_Unconstrained_Array_Indices_Range =>
                  return Translate_Unconstrained
                    (Decl.As_Type_Decl.F_Type_Def.As_Array_Type_Def,
                     Type_Name);

               when others =>
                  return
                    (Success     => False,
                     Diagnostics =>
                       +"Unexpected array indices for array type def:"
                        & Kind_Name (Decl.As_Type_Decl.F_Type_Def
                                     .As_Array_Type_Def.F_Indices));
            end case;

         when others =>
            return
              (Success     => False,
               Diagnostics => +"Unexpected base type decl kind for an array:"
                              & Kind_Name (Decl));
      end case;
   end Translate_Array_Decl;

   ------------------------
   -- Record_Constrained --
   ------------------------

   function Record_Constrained
      (Decl : Base_Type_Decl;
      Root : Base_Type_Decl) return Boolean
   is
      Ancestor_Type : Subtype_Indication;
   begin
      --  The original Decl of a record is not constrained.

      if Decl = Root then
         return False;
      end if;
      case Kind (Decl) is
         when Ada_Subtype_Decl_Range =>
            Ancestor_Type := Decl.As_Subtype_Decl.F_Subtype;
         when Ada_Type_Decl =>
            pragma Assert (Kind (Decl.As_Type_Decl.F_Type_Def)
                           in Ada_Derived_Type_Def_Range);
            Ancestor_Type := Decl.As_Type_Decl.F_Type_Def
                              .As_Derived_Type_Def.F_Subtype_Indication;
         when others =>
            return False;
            --  we should not be able to end up in here, but if we do,
            --  simply ignore the constraints.
      end case;

      if Is_Null (Ancestor_Type.F_Constraint) then
         return Record_Constrained
            (Ancestor_Type.P_Designated_Type_Decl, Root);
      else
         pragma Assert (Kind (Ancestor_Type.F_Constraint)
                        in Ada_Discriminant_Constraint_Range);
         return True;
      end if;
   end Record_Constrained;

   -------------------------------
   -- Apply_Record_Subtype_Decl --
   -------------------------------

   procedure Apply_Record_Subtype_Decl
      (Decl : Subtype_Indication;
      Res  : in out Discriminated_Record_Typ)
   is
      Constraints : Assoc_List;
   begin
      if Is_Null (Decl.F_Constraint) then
         return;
      end if;

      Constraints := Decl.F_Constraint.As_Discriminant_Constraint
                     .F_Constraints;

      for Assoc of Constraints loop
         if Assoc.As_Discriminant_Assoc.F_Discr_Expr.P_Is_Static_Expr then
            for Id of Assoc.As_Discriminant_Assoc.F_Ids loop
               Res.Discriminant_Constraint.Include
                  (Id.P_Referenced_Defining_Name,
                  (Kind    => Static,
                     Int_Val => Integer'Value (Assoc.As_Discriminant_Assoc
                                             .F_Discr_Expr.P_Eval_As_Int
                                             .Image)));
            end loop;
         else
            for Id of Assoc.As_Discriminant_Assoc.F_Ids loop
               Res.Discriminant_Constraint.Include
                 (Id.P_Referenced_Defining_Name,
                  (Kind => Non_Static));
            end loop;
         end if;
      end loop;
   end Apply_Record_Subtype_Decl;

   ------------------------------------
   -- Apply_Record_Derived_Type_Decl --
   ------------------------------------

   function Apply_Record_Derived_Type_Decl
      (Decl  : Type_Decl'Class;
      From  : Discriminated_Record_Typ) return Discriminated_Record_Typ
   is

      use Discriminant_Constraint_Maps;

      Constraints_Map : Discriminant_Constraint_Maps.Map;
      Discr_Renaming_Map : Discriminant_Constraint_Maps.Map;
      Constraint_Cur : Cursor;
   begin
      --  There are three cases here:
      --  1. There is no known discriminant part, and no discriminant
      --     constraints. In that case, simply forward the type as is, with
      --     all of its discriminants and constraints
      --
      --  2. There is no known discriminant part, but we have a set of
      --     discriminant constraints. In that case, The type should become
      --     a non discriminated record type. We don't do this here, as we
      --     may have non-static constraints which prevent us from
      --     determining the actual components of the record, so we keep
      --     a Discriminated record type, but we'll prune the incompatible
      --     shapes as best as we can.
      --
      --  3. We have a known discriminant part and discriminant constraints,
      --     so the resulting type is a non constrained discriminated record
      --     type, but as with the previous case, some of the constraints
      --     may not be static, so we'll prune the incompatible shapes as
      --     best as possible.
      --
      --  Case 2 and 3 will be handled together given that we do not
      --  change the type to undiscriminated/nonconstrained_record_typ.
      --
      --  There cannot be a case where we have a known discriminant part but
      --  no discriminant constraints as we do not deal with tagged types.

      --  Case 1:

      if Is_Null (Decl.F_Type_Def.As_Derived_Type_Def.F_Subtype_Indication
                  .F_Constraint)
      then
         return From;
      end if;

      --  Case 2 & 3:

      --  First build a discriminant constraint map to filter out
      --  the unachievable shapes.

      return New_Typ : Discriminated_Record_Typ (Constrained => True) do
         for Assoc of Decl.F_Type_Def.As_Derived_Type_Def
                        .F_Subtype_Indication.F_Constraint
                        .As_Discriminant_Constraint.F_Constraints
         loop
            if Kind (Assoc.As_Discriminant_Assoc.F_Discr_Expr) in Ada_Name
               and then not Is_Null (Assoc.As_Discriminant_Assoc.F_Discr_Expr
                                    .As_Name.P_Referenced_Defining_Name)
               and then Kind (Assoc.As_Discriminant_Assoc.F_Discr_Expr
                                    .As_Name.P_Referenced_Defining_Name
                                    .Parent.Parent) in
                        Ada_Discriminant_Spec_Range
            then
               --  Case of a Discriminant correspondance
               for Id of Assoc.As_Discriminant_Assoc.F_Ids loop
                  Discr_Renaming_Map.Insert
                     (Key      => Id.P_Referenced_Defining_Name,
                      New_Item =>
                       (Kind          => Discriminant,
                        Disc_Name     => Assoc.As_Discriminant_Assoc
                                          .F_Discr_Expr.As_Name
                                          .P_Referenced_Defining_Name,
                        Enclosing_Typ => SP.Null_Ref));
               end loop;
            elsif Assoc.As_Discriminant_Assoc.F_Discr_Expr.P_Is_Static_Expr
            then
               --  Static value in the discriminant constraint

               for Id of Assoc.As_Discriminant_Assoc.F_Ids loop
                  Constraints_Map.Insert
                     (Key      => Id.P_Referenced_Defining_Name,
                     New_Item =>
                        (Kind    => Static,
                        Int_Val => Integer'Value
                           (Assoc.As_Discriminant_Assoc.F_Discr_Expr
                           .P_Eval_As_Int.Image)));
               end loop;
            else
               --  Non static value

               for Id of Assoc.As_Discriminant_Assoc.F_Ids loop
                  Constraints_Map.Insert
                     (Key => Id.P_Referenced_Defining_Name,
                      New_Item => (Kind => Non_Static));
               end loop;
            end if;
         end loop;

         --  Then copy the shapes, discriminant types and non static
         --  discriminant constraints to the new type
         --  Also rebuild the list of components while copying the shapes

         declare
            use Shape_Maps;
            use Component_Maps;
            New_Shape_Index : Positive := 1;
            Shape_Cur       : Shape_Maps.Cursor := From.Shapes.First;
            Component_Cur   : Component_Maps.Cursor;
            Inserted        : Boolean;

            procedure Update_Choice_Discriminant
              (Index : Positive; Choice : in out Discriminant_Choice_Entry);
            --  Replace the discriminant name in Choice if it is present in
            --  Discr_Renaming_Map.

            procedure Update_Shape_Discriminants
              (Index : Positive; Current_Shape : in out Record_Types.Shape);
            --  Go through all the choices of the shape to rename the
            --  associated discriminant according to Discr_Renaming_Map.

            --------------------------------
            -- Update_Shape_Discriminants --
            --------------------------------

            procedure Update_Shape_Discriminants
              (Index : Positive; Current_Shape : in out Record_Types.Shape)
            is
               use Discriminant_Choices_Maps;
               Choice_Cur : Discriminant_Choices_Maps.Cursor;
            begin
               Choice_Cur := Current_Shape.Discriminant_Choices.First;
               while Has_Element (Choice_Cur) loop
                  Current_Shape.Discriminant_Choices.Update_Element
                     (Choice_Cur, Update_Choice_Discriminant'Access);
                  Next (Choice_Cur);
               end loop;
            end Update_Shape_Discriminants;

            --------------------------------
            -- Update_Choice_Discriminant --
            --------------------------------

            procedure Update_Choice_Discriminant
              (Index : Positive; Choice : in out Discriminant_Choice_Entry)
            is
               use Discriminant_Constraint_Maps;
               Renaming_Cur : Discriminant_Constraint_Maps.Cursor;
            begin
               Renaming_Cur := Discr_Renaming_Map.Find (Choice.Defining_Name);
               if Has_Element (Renaming_Cur) then
                  Choice.defining_Name := Element (Renaming_Cur).Disc_Name;
               end if;
            end Update_Choice_Discriminant;

         begin
            for Current_Shape of From.Shapes loop
               if Shape_Matches (Element (Shape_Cur), Constraints_Map) then

                  --  Insert the new shape in the new type

                  New_Typ.Shapes.Insert
                    (Key      => New_Shape_Index,
                     New_Item => Current_Shape,
                     Position => Shape_Cur,
                     Inserted => Inserted);
                  New_Shape_Index := New_Shape_Index + 1;

                  --  Replace all discriminants that have correspondance to the
                  --  new name in the shape we just inserted

                  New_Typ.Shapes.Update_Element
                    (Shape_Cur, Update_Shape_Discriminants'Access);

                  --  and register all of the components in the global map.

                  Component_Cur := Current_Shape.Components.First;
                  while Has_Element (Component_Cur) loop
                     New_Typ.Component_Types.Include
                       (Key (Component_Cur), Element (Component_Cur));
                     Next (Component_Cur);
                  end loop;
               end if;
            end loop;
         end;

         --  Fill out discriminant types

         Constraint_Cur := Discr_Renaming_Map.First;
         while Has_Element (Constraint_Cur) loop
            if Element (Constraint_Cur).Kind = Discriminant then
               New_Typ.Discriminant_Types.Insert
                  (Key     => Element (Constraint_Cur).Disc_Name,
                   New_Item => From.Discriminant_Types.Element
                                 (Key (Constraint_Cur)));
               --  TODO: propagate new discriminant name in the components
               Next (Constraint_Cur);
            end if;
         end loop;

         --  Then the non static constraints
         --  We also need to copy the corresponding discriminant type.

         Constraint_Cur := Constraints_Map.First;
         while Has_Element (Constraint_Cur) loop
            if Element (Constraint_Cur).Kind = Non_Static then
               New_Typ.Discriminant_Constraint.Insert
                  (Key (Constraint_Cur), Element (Constraint_Cur));
               New_Typ.Discriminant_Types.Insert
                  (Key     =>  Key (Constraint_Cur),
                   New_Item => From.Discriminant_Types.Element
                                 (Key (Constraint_Cur)));
            end if;
            Next (Constraint_Cur);
         end loop;

      end return;
   end Apply_Record_Derived_Type_Decl;

   ---------------------------
   -- Translate_Record_Decl --
   ---------------------------

   function Translate_Record_Decl
     (Decl      : Base_Type_Decl;
      Type_Name : Defining_Name) return Translation_Result
   is

      function Translate_Component_Decl_List
        (Decl_List : Ada_Node_List;
         Res       : in out Component_Maps.Map) return Unbounded_String;
      --  Translate the list of components of Decl into Res

      procedure Apply_Constraints
        (Decl, Root : Base_Type_Decl; Res : in out Discriminated_Record_Typ);
      --  Modify Res to include all the discriminant constraints present in
      --  the type derivation / subtype decl chain.

      -----------------------------------
      -- Transalte_Component_Decl_List --
      -----------------------------------

      function Translate_Component_Decl_List
        (Decl_List : Ada_Node_List;
         Res       : in out Component_Maps.Map) return Unbounded_String
      is
         Current_Typ : Translation_Result;
         Comp_Decl : Component_Decl;
      begin
         for Decl of Decl_List loop
            Comp_Decl := Decl.As_Component_Decl;
            Current_Typ := Translate (Comp_Decl.F_Component_Def.F_Type_Expr);
            if not Current_Typ.Success then
               return "Failed to translate type of component"
                      & Comp_Decl.Image & ": " & Current_Typ.Diagnostics;
            end if;
            for Id of Comp_Decl.F_Ids loop
               Res.Insert (Key => Id.As_Defining_Name,
                           New_Item => Current_Typ.Res);
            end loop;
         end loop;
         return Null_Unbounded_String;
      end Translate_Component_Decl_List;

      -----------------------
      -- Apply_Constraints --
      -----------------------

      procedure Apply_Constraints
        (Decl, Root : Base_Type_Decl; Res : in out Discriminated_Record_Typ)
      is
      begin
         --  The original Decl of a record is not constrained.

         if Decl = Root then
            return;
         end if;

         case Kind (Decl) is
            when Ada_Type_Decl_Range =>

               --  First apply constraints of the ancestor type

               Apply_Constraints
                 (Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                  .F_Subtype_Indication.F_Name.P_Name_Designated_Type,
                  Root,
                  Res);

               --  Then apply the effects of the type derivation

               Res := Apply_Record_Derived_Type_Decl (Decl.As_Type_Decl, Res);

            when Ada_Subtype_Decl_Range =>

               --  First apply the constraints of the ancestor type

               Apply_Constraints
                 (Decl.As_Subtype_Decl.F_Subtype.F_Name.P_Name_Designated_Type,
                  Root,
                  Res);

               --  The register the eventual constraints imposed by the subtype
               --  definition

               Apply_Record_Subtype_Decl (Decl.As_Subtype_Decl.F_Subtype, Res);

            when others =>
               --  This should not be rechable
               null;
         end case;
      end Apply_Constraints;

      Actual_Decl : Type_Decl;
      --  The type decl where the components of the array are actually defined.
      --  For now we don't support tagged types, and thus record extension, so
      --  the whole list of components is available in a single type
      --  declaration. Other subtypes or derived types may only add
      --  discriminant constraints or rebind discriminants.

      Failure_Reason : Unbounded_String;

   --  Start of processing for Translate_Record_Decl;

   begin

      --  First the simple case of an undiscriminated record

      if Kind (Decl.P_Root_Type) in Ada_Type_Decl_Range
        and then Kind (Decl.P_Root_Type.As_Type_Decl.F_Type_Def)
                   in Ada_Record_Type_Def_Range
        and then Is_Null (Decl.P_Root_Type.As_Type_Decl.F_Discriminants)
      then
         Actual_Decl := Decl.P_Root_Type.As_Type_Decl;

         declare
            Trans_Res : Nondiscriminated_Record_Typ;
            Comp_List : constant Ada_Node_List :=
              Actual_Decl.F_Type_Def.As_Record_Type_Def.F_Record_Def
              .F_Components.F_Components;
         begin

            Failure_Reason := Translate_Component_Decl_List
               (Comp_List, Trans_Res.Component_Types);

            if Failure_Reason = Null_Unbounded_String then
               Trans_Res.Name := Type_Name;

               return Res : Translation_Result (Success => True) do
                  Res.Res.Set (Trans_Res);
               end return;

            else
               return (Success => False,
                 Diagnostics => Failure_Reason);
            end if;
         end;

      else
         --  Now the rest. The constraints in the subtype definitions in the
         --  component declarations are not handled yet.

         Actual_Decl := Decl.P_Root_Type.As_Type_Decl;

         declare

            Record_Shapes : constant Shape_Array :=
              Decl.P_Shapes (Include_Discriminants => False);

            Trans_Res : Discriminated_Record_Typ
              (Constrained => Record_Constrained
                                (Decl, Actual_Decl.As_Base_Type_Decl));

            Discriminant_List : constant Discriminant_Spec_List :=
              Actual_Decl.F_Discriminants.As_Known_Discriminant_Part
              .F_Discr_Specs;
            --  ??? We assume that we only have known discriminants for the
            --  moment as we are supposed to be translating the full view of
            --  the type, will need to revisit this to double check.

            Current_Type  : Translation_Result;
            Current_Shape : LAL.Shape;
            Translated_Shape : Record_Types.Shape;
            Cur_Discr_Val : Discriminant_Values;
            Comp_Decl     : Component_Decl;
         begin

            --  First translate the list of discriminants

            for Spec of Discriminant_List loop
               if not Is_Null (Spec.F_Default_Expr) then
                  Trans_Res.Mutable := True;
               end if;
               Current_Type := Translate (Spec.F_Type_Expr, Verbose_Diag);
               if not Current_Type.Success then
                  Failure_Reason := "Failed to translate discriminant spec "
                                    & Spec.Image & ": "
                                    & Current_Type.Diagnostics;
                  goto Failed_Discr_Rec_Translation;
               end if;
               for Def_Name of Spec.F_Ids loop
                  Trans_Res.Discriminant_Types.Insert
                    (Key      => Def_Name.As_Defining_Name,
                     New_Item => Current_Type.Res);
               end loop;
            end loop;

            --  Then process the shapes.
            --  For each shape, translate each component decl, and add it in
            --  the corresponding shape component map. Also add it to the
            --  global list of components.

            for J in Record_Shapes'Range loop
               Current_Shape := Record_Shapes (J);

               --  Reset the current shape variable.
               Translated_Shape.Components.Clear;
               Translated_Shape.Discriminant_Choices.Clear;

               --  First translate the component types and include them in the
               --  global component map.

               for Decl of Components (Current_Shape) loop
                  Comp_Decl := Decl.As_Component_Decl;
                  Current_Type := Translate
                    (Comp_Decl.F_Component_Def.F_Type_Expr);
                  if not Current_Type.Success then
                     Failure_Reason := "Failed to translate type of component"
                                       & Comp_Decl.Image & ": "
                                       & Current_Type.Diagnostics;
                     goto Failed_Discr_Rec_Translation;
                  end if;
                  for Id of Comp_Decl.F_Ids loop
                     Translated_Shape.Components.Insert
                     (Key => Id.As_Defining_Name,
                      New_Item => Current_Type.Res);
                  end loop;
               end loop;

               declare
                  use Component_Maps;
                  Cur : Cursor := Translated_Shape.Components.First;
               begin
                  while Has_Element (Cur) loop

                     --  We use Include instead of Insert here because a
                     --  component may appear in multiple shapes, so it is
                     --  legitimate to attempt to insert the same component
                     --  multiple times.

                     Trans_Res.Component_Types.Include
                       (Key      => Component_Maps.Key (Cur),
                        New_Item => Component_Maps.Element (Cur));
                     Next (Cur);
                  end loop;
               end;

               --  Then process the discriminant values. We store the "raw"
               --  LAL choice nodes to be able to call P_Choice_Matches on them
               --  later once we have actual values for the discriminants.

               for Discriminant_Index in
               Discriminants_Values (Current_Shape)'Range
               loop
                  Cur_Discr_Val :=
                  Discriminants_Values (Current_Shape) (Discriminant_Index);

                  Translated_Shape.Discriminant_Choices.Insert
                  (Discriminant_Index,
                     (Defining_Name =>
                       Discriminant (Cur_Discr_Val).P_Referenced_Defining_Name,
                     Choices        =>
                       Values (Cur_Discr_Val).As_Alternatives_List));
               end loop;
               Trans_Res.Shapes.Insert (J, Translated_Shape);
            end loop;

            --  If the record is actually a constrained type, reccord the
            --  constraints now.

            if Trans_Res.Constrained then
               Apply_Constraints
                 (Decl, Actual_Decl.As_Base_Type_Decl, Trans_Res);
            end if;

            Trans_Res.Name := Type_Name;

            --  Apply_Constraints can actully return a type that isn't
            --  discriminated or that isn't constrained, so lets try to
            --  convert Trans_Res to the correct kind depending on the
            --  attributes.

            if Trans_Res.Discriminant_Types.Is_Empty then
               if Trans_Res.Discriminant_Constraint.Is_Empty then

                  --  Normally only checking for the discriminant is sufficient
                  --  to check if Trans_Res will is actually a non
                  --  discriminated type, but we may have some lingering non
                  --  static cosntraints that don't allow us to determine
                  --  what the final list of components is.

                  null;
               end if;
            end if;

            return Res : Translation_Result (Success => True) do
               Res.Res.Set (Trans_Res);
            end return;

            <<Failed_Discr_Rec_Translation>>
            return (Success => False,
                    Diagnostics => Failure_Reason);

         end;
      end if;
   end Translate_Record_Decl;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N : LAL.Type_Expr; Verbose : Boolean := False) return Translation_Result
   is
      Type_Decl_Node : Base_Type_Decl;
      Intermediate_Result : Translation_Result;
   begin
      if Kind (N) in Ada_Anonymous_Type_Range then
         Type_Decl_Node := N.As_Anonymous_Type.F_Type_Decl.As_Base_Type_Decl;
      else

         --  For now, work on the full view of the type that we are trying to
         --  translate. If this proves useless/problematic this can be
         --  revisited.

         Type_Decl_Node :=
           N.As_Subtype_Indication.P_Designated_Type_Decl.P_Full_View;
      end if;

      Intermediate_Result := translate (Type_Decl_Node, Verbose);

      if not Intermediate_Result.Success
        or else Kind (N) in Ada_Anonymous_Type
        or else Is_Null (N.As_Subtype_Indication.F_Constraint)
      then
         return Intermediate_Result;
      else
         --  TODO: Implement constraint translation for anonymous subtypes
         return Intermediate_Result;
      end if;
   exception
      when Exc : Property_Error =>
         return (Success     => False,
                 Diagnostics => +"Error translating " & N.Image & " : "
                                 & Ada.Exceptions.Exception_Message (Exc));
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N       : LAL.Base_Type_Decl;
      Verbose : Boolean := False) return Translation_Result
   is
      use Translation_Maps;

      Cache_Cur : constant Cursor := Translation_Cache.Find (N.As_Ada_Node);
   begin
      --  If we still have the base_type_decl in the cache, return it

      if Cache_Cur /= No_Element then
         Cache_Hits := Cache_Hits + 1;
         return Res : Translation_Result (Success => True) do
            Res.Res := Element (Cache_Cur);
         end return;
      end if;

      Cache_Miss := Cache_Miss + 1;

      --  Otherwise, recompute it and store it in the cache

      declare
         Trans_Res : constant Translation_Result :=
           Translate_Internal (N, Verbose);
      begin
         if Trans_Res.Success then
            Translation_Cache.Insert (N.As_Ada_Node, Trans_Res.Res);
         end if;
         return Trans_Res;
      end;

   end Translate;

   ------------------------
   -- Translate_Internal --
   ------------------------

   function Translate_Internal
     (N       : LAL.Base_Type_Decl;
      Verbose : Boolean := False) return Translation_Result
   is
      Root_Type : constant Base_Type_Decl := N.P_Root_Type;
      Is_Static : Boolean;
      --  Relevant only for Scalar types / array bounds

      Type_Name : constant Defining_Name :=
        (if not (Kind (N) in Ada_Anonymous_Type_Decl_Range)
         then N.P_Defining_Name
         else No_Defining_Name);

   begin
      Verbose_Diag := Verbose;
      Is_Static := N.P_Is_Static_Decl;

      if N.P_Is_Int_Type then
         if Is_Static then
            return Translate_Int_Decl (N, Type_Name);
         else
            return Res : Translation_Result (Success => True) do
               Res.Res.Set (Int_Typ'(Is_Static => False,
                                     Name      => Type_Name));
            end return;
         end if;

      elsif P_Is_Derived_Type
          (Node       => N,
           Other_Type => N.P_Bool_Type.As_Base_Type_Decl)
      then
         return Res : Translation_Result (Success => True) do
            Res.Res.Set (Bool_Typ'(Is_Static => True,
                                   Name      => Type_Name));
         end return;
      elsif N.P_Is_Enum_Type then

         if not Is_Static then
            return Res : Translation_Result (Success => True) do
               Res.Res.Set (Other_Enum_Typ'(Is_Static => False,
                                            Name      => Type_Name));
            end return;
         end if;
         declare
            Root_Type_Name : constant String :=
              Text.Image (Root_Type.P_Unique_Identifying_Name);
         begin
            if Root_Type_Name = "standard.character"
              or else Root_Type_Name = "standard.wide_character"
              or else Root_Type_Name = "standard.wide_wide_character"
            then
               return Res : Translation_Result (Success => True) do
                  Res.Res.Set (Char_Typ'
                      (Is_Static => True,
                       Name      => Type_Name));
               end return;
            else
               return Translate_Enum_Decl (N, Root_Type, Type_Name);
            end if;
         end;

      elsif N.P_Is_Float_Type then
         if Is_Static then
            return Translate_Float_Decl (N, Type_Name);
         else
            return Res : Translation_Result (Success => True) do
               Res.Res.Set
                 (Float_Typ'(Is_Static => False,
                             Has_Range => False,
                             Name      => Type_Name));
            end return;
         end if;

      elsif N.P_Is_Fixed_Point then
         if Kind (Root_Type.As_Type_Decl.F_Type_Def) in
             Ada_Ordinary_Fixed_Point_Def_Range
         then
            if Is_Static then
               return Translate_Ordinary_Fixed_Decl (N, Type_Name);
            else
               return Res : Translation_Result (Success => True) do
                  Res.Res.Set
                    (Ordinary_Fixed_Typ'(Is_Static => False,
                                         Name      => Type_Name));
               end return;
            end if;
         else
            if Is_Static then
               return Translate_Decimal_Fixed_Decl (N, Type_Name);
            else
               return Res : Translation_Result (Success => True) do
                  Res.Res.Set
                    (Decimal_Fixed_Typ'(Is_Static => False,
                                        Has_Range => False,
                                        Name      => Type_Name));
               end return;
            end if;
         end if;

      elsif N.P_Is_Array_Type then
         return Translate_Array_Decl (N, Type_Name);

      elsif N.P_Is_Record_Type then
         return Translate_Record_Decl (N, Type_Name);

      elsif N.P_Is_Access_Type then
         return Res : Translation_Result (Success => True) do
            Res.Res.Set (Access_Typ'(Name => Type_Name));
         end return;
      end if;

      return (Success => False, Diagnostics => +"Unknown type kind");

   exception
      when Exc : Property_Error =>
         return
           (Success     => False,
            Diagnostics =>
              +"Error translating " & N.Image & " : " &
              Ada.Exceptions.Exception_Information (Exc));

   end Translate_Internal;

   procedure Print_Cache_Stats is
   begin
      New_Line;
      Put_Line ("Items in cache :" & Translation_Cache.Length'Image);
      Put_Line ("Cache hits  :" & Cache_Hits'Image);
      Put_Line ("Cache misses:" & Cache_Miss'Image);
   end Print_Cache_Stats;

end TGen.Types.Translation;
