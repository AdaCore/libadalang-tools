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

with Langkit_Support.Text;

with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

with TGen.Int_Types;   use TGen.Int_Types;
with TGen.Real_Types;  use TGen.Real_Types;
with TGen.Enum_Types;  use TGen.Enum_Types;

package body TGen.Types.Translation is
   use LAL;

   Translation_Error : exception;

   Verbose_Diag : Boolean := False;
   package Text renames Langkit_Support.Text;

   function "+" (Str : String) return Unbounded_String is
     (To_Unbounded_String (Str));

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Int_Type;

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
      return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Enum_Type;

   function Translate_Float_Decl
     (Decl : Base_Type_Decl) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Float_Type;

   function Translate_Ordinary_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Fixed_Point;

   function Translate_Decimal_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result with
      Pre => Decl.P_Is_Static_Decl and then Decl.P_Is_Fixed_Point;

   procedure Translate_Float_Range
     (Decl     :     Base_Type_Decl; Has_Range : out Boolean;
      Min, Max : out Long_Float);

   ------------------------
   -- Translate_Int_Decl --
   ------------------------

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max : constant Integer :=
        Integer'Value (High_Bound (Rang).P_Eval_As_Int.Image);
      --  Since Decl is a static subtype, its bounds are also static
      --  expressions according to RM 4.9(26/2).
   begin
      if Is_Null (Low_Bound (Rang)) then
         return
           (Success => True,
            Res     =>
              new Mod_Int_Typ'
                (Is_Static => True, Name => Decl.P_Defining_Name,
                 Mod_Value => Max));
      else
         declare
            Min : constant Integer :=
              Integer'Value (Low_Bound (Rang).P_Eval_As_Int.Image);
         begin
            return
              (Success => True,
               Res     =>
                 new Signed_Int_Typ'
                   (Is_Static   => True, Name => Decl.P_Defining_Name,
                    Range_Value => (Min => Min, Max => Max)));
         end;
      end if;
   end Translate_Int_Decl;

   -------------------------
   -- Translate_Enum_Decl --
   -------------------------

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
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

      return
        (Success => True,
         Res     =>
           new Other_Enum_Typ'
             (Is_Static => True, Name => Decl.P_Defining_Name,
              Literals  => Enum_Lits));
   end Translate_Enum_Decl;

   --------------------------
   -- Translate_Float_Decl --
   --------------------------

   function Translate_Float_Decl
     (Decl : Base_Type_Decl) return Translation_Result
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

         elsif Kind (Decl.As_Type_Decl.F_Type_Def) in Ada_Derived_Type_Def then
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
               when Ada_Digits_Constraint =>
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
   begin
      Find_Digits (Decl, Digits_Value);
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      if Has_Range then
         return
           (Success => True,
            Res     =>
              new Float_Typ'
                (Is_Static   => True, Has_Range => True,
                 Name => Decl.P_Defining_Name, Digits_Value => Digits_Value,
                 Range_Value => (Min => Min, Max => Max)));
      else
         return
           (Success => True,
            Res     =>
              new Float_Typ'
                (Is_Static => True, Has_Range => False,
                 Name => Decl.P_Defining_Name, Digits_Value => Digits_Value));
      end if;
   exception
      when Exc : Translation_Error =>
         if Verbose_Diag then
            Put_Line
              ("Warning: could not determine static properties of" & " type" &
               Decl.Image & " : " & Ada.Exceptions.Exception_Message (Exc));
         end if;
         return
           (Success => True,
            Res     =>
              new Float_Typ'
                (Is_Static => False, Has_Range => False,
                 Name      => Decl.P_Defining_Name));
   end Translate_Float_Decl;

   -----------------------------------
   -- Translate_Ordinary_Fixed_Decl --
   -----------------------------------

   function Translate_Ordinary_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result
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
              (Kind (Decl.As_Type_Decl.F_Type_Def) =
               Ada_Ordinary_Fixed_Point_Def);
            Delta_Expr :=
              Decl.As_Type_Decl.F_Type_Def.As_Ordinary_Fixed_Point_Def.F_Delta;

         elsif Kind (Decl) = Ada_Subtype_Decl
           or else
           (Kind (Decl) = Ada_Type_Decl
            and then Kind (Decl.As_Type_Decl.F_Type_Def) =
              Ada_Derived_Type_Def)
         then

            --  Case of a subtype decl or derived type decl, look at the
            --  subtype indication for a constraint, or look at the delta of
            --  the parent subtype.

            if Kind (Decl) = Ada_Subtype_Decl then
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
               when Ada_Delta_Constraint =>
                  Delta_Expr :=
                    Subtype_Constraint.As_Delta_Constraint.F_Digits;

               when Ada_Range_Constraint =>

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

   begin
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      pragma Assert (Has_Range);
      Find_Delta (Decl, Delta_Value);
      return
        (Success => True,
         Res     =>
           new Ordinary_Fixed_Typ'
             (Is_Static   => True, Name => Decl.P_Defining_Name,
              Delta_Value => Delta_Value,
              Range_Value => (Min => Min, Max => Max)));
   exception
      when Exc : Translation_Error =>

         --  In case of translation error, return a non-static type,
         --  but print the information if verbose diagnostics are required.
         if Verbose_Diag then
            Put_Line
              ("Warning: could not determine static properties of" & " type" &
               Decl.Image & " : " & Ada.Exceptions.Exception_Message (Exc));
         end if;
         return
           (Success => True,
            Res     =>
              new Ordinary_Fixed_Typ'
                (Is_Static => False, Name => Decl.P_Defining_Name));
   end Translate_Ordinary_Fixed_Decl;

   ----------------------------------
   -- Translate_Decimal_Fixed_Decl --
   ----------------------------------

   function Translate_Decimal_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
   begin
      --  ??? Actually implement this, should be similar to Ordinary
      --  fixed points

      return (Success => True,
              Res     => new Decimal_Fixed_Typ'
                (Is_Static => False,
                 Has_Range => False,
                 Name      => Decl.P_Defining_Name));
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

         --  Decl is the root, type, so it is a type decl

         case Kind (Decl.As_Type_Decl.F_Type_Def) is
            when Ada_Floating_Point_Def =>
               Range_Spec_Val :=
                 Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def.F_Range;
            when Ada_Ordinary_Fixed_Point_Def =>
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
         if Kind (Decl) = Ada_Type_Decl
           and then Kind (Decl.As_Type_Decl.F_Type_Def) in Ada_Derived_Type_Def
         then

            --  Decl is a derived type decl, look at the constraints in the
            --  subtype indication

            Parent_Type :=
              Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                .F_Subtype_Indication;
         elsif Kind (Decl) = Ada_Subtype_Decl then

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
            when Ada_Range_Constraint =>
               Range_Spec_Val :=
                 Parent_Type.F_Constraint.As_Range_Constraint.F_Range;
            when Ada_Digits_Constraint =>
               if Is_Null
                   (Parent_Type.F_Constraint.As_Digits_Constraint.F_Range)
               then
                  Translate_Float_Range
                    (Parent_Type.P_Designated_Type_Decl, Has_Range, Min, Max);
                  return;
               end if;
               Range_Spec_Val :=
                 Parent_Type.F_Constraint.As_Digits_Constraint.F_Range;
            when Ada_Delta_Constraint =>
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

         when Ada_Attribute_Ref =>
            Translate_Float_Range
              (Range_Spec_Val.F_Range.As_Attribute_Ref.F_Prefix
                 .P_Referenced_Decl
                 .As_Base_Type_Decl,
               Has_Range, Min, Max);

         when Ada_Bin_Op =>
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

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N : LAL.Type_Expr; Verbose : Boolean := False) return Translation_Result
   is
      Type_Decl_Node : Base_Type_Decl;
   begin
      if Kind (N) in Ada_Anonymous_Type then
         return
           (Success     => False,
            Diagnostics => +"Anonymous types not supported yet");
      end if;

      --  For now, work on the full view of the type that we are trying to
      --  translate. If this proves useless/problematic this can be revisited.

      Type_Decl_Node :=
        N.As_Subtype_Indication.P_Designated_Type_Decl.P_Full_View;

      return Translate (Type_Decl_Node, Verbose);
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
      Root_Type : constant Base_Type_Decl := N.P_Root_Type;
      Is_Static : Boolean;
      --  Relevant only for Scalar types / array bounds

   begin
      Verbose_Diag := Verbose;

      Is_Static := N.P_Is_Static_Decl;

      if N.P_Is_Int_Type then
         if Is_Static then
            return Translate_Int_Decl (N);
         else
            return
              (Success => True,
               Res     =>
                 new Int_Typ'
                   (Is_Static => False,
                    Name      => N.P_Defining_Name));
         end if;

      elsif P_Is_Derived_Type
          (Node       => N,
           Other_Type => N.P_Bool_Type.As_Base_Type_Decl)
      then
         return
           (Success => True,
            Res     =>
              new Bool_Typ'
                (Is_Static => True, Name => N.P_Defining_Name));
      elsif N.P_Is_Enum_Type then

         if not Is_Static then
            return
              (Success => True,
               Res     =>
                 new Other_Enum_Typ'
                   (Is_Static => False,
                    Name      => N.P_Defining_Name));
         end if;
         declare
            Root_Type_Name : constant String :=
              Text.Image (Root_Type.P_Unique_Identifying_Name);
         begin
            if Root_Type_Name = "standard.character"
              or else Root_Type_Name = "standard.wide_character"
              or else Root_Type_Name = "standard.wide_wide_character"
            then
               return
                 (Success => True,
                  Res     =>
                    new Char_Typ'
                      (Is_Static => True,
                       Name      => N.P_Defining_Name));
            else
               return Translate_Enum_Decl (N, Root_Type);
            end if;
         end;

      elsif N.P_Is_Float_Type then
         if Is_Static then
            return Translate_Float_Decl (N);
         else
            return
              (Success => True,
               Res     =>
                 new Float_Typ'
                   (Is_Static => False, Has_Range => False,
                    Name      => N.P_Defining_Name));
         end if;

      elsif N.P_Is_Fixed_Point then
         if Kind (Root_Type.As_Type_Decl.F_Type_Def) in
             Ada_Ordinary_Fixed_Point_Def_Range
         then
            if Is_Static then
               return Translate_Ordinary_Fixed_Decl (N);
            else
               return
                 (Success => True,
                  Res     =>
                    new Ordinary_Fixed_Typ'
                      (Is_Static => False,
                       Name      => N.P_Defining_Name));
            end if;
         else
            if Is_Static then
               return Translate_Decimal_Fixed_Decl (N);
            else
               return
                 (Success => True,
                  Res     =>
                    new Decimal_Fixed_Typ'
                      (Is_Static => False, Has_Range => False,
                       Name      => N.P_Defining_Name));
            end if;
         end if;
      end if;

      return (Success => False, Diagnostics => +"Unknown type kind");

   exception
      when Exc : Property_Error =>
         return
           (Success     => False,
            Diagnostics =>
              +"Error translating " & N.Image & " : " &
              Ada.Exceptions.Exception_Message (Exc));

   end Translate;

end TGen.Types.Translation;
