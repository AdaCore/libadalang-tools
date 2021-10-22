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
--  with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text;

with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

package body TGen.Types.Translation is
   use LAL;

   Translation_Error : exception;
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
                (Name => Decl.P_Defining_Name, Mod_Value => Max));
      else
         declare
            Min : constant Integer :=
              Integer'Value (Low_Bound (Rang).P_Eval_As_Int.Image);
         begin
            return
              (Success => True,
               Res     =>
                 new Signed_Int_Typ'
                   (Name        => Decl.P_Defining_Name,
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
             (Name => Decl.P_Defining_Name, Literals => Enum_Lits));
   end Translate_Enum_Decl;

   --------------------------
   -- Translate_Float_Decl --
   --------------------------

   function Translate_Float_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is

      procedure Translate_Float_Decl_Internal
        (Decl         : Base_Type_Decl;
         Find_Digits  : Boolean;
         Find_Range   : Boolean;
         Digits_Value : in out Natural;
         Has_Range    : in out Boolean;
         Range_Value  : in out Float_Range);
      --  Determine the characteristics of Decl. Find_Digits and Find_Range are
      --  used to indicate whether this call should atempt to evaluate the
      --  digits value and range bounds of Decl or not. As such, Digits_Value
      --  will only be modified if Find_Digits was set to True, and Range_Value
      --  and Has_Range will only be modified if Find_Range is set to True.

      procedure Translate_Float_Decl_Internal
        (Decl         : Base_Type_Decl;
         Find_Digits  : Boolean;
         Find_Range   : Boolean;
         Digits_Value : in out Natural;
         Has_Range    : in out Boolean;
         Range_Value  : in out Float_Range)
      is
         Parent_Type    : Subtype_Indication;
         Constraints    : Constraint;
         Range_Spec_Val : Range_Spec := No_Range_Spec;
      begin
         if Decl = Decl.P_Root_Type then

            --  Decl is the root type decl, so we only need to translate the
            --  type definition.

            declare
               Float_Def : constant Floating_Point_Def :=
                 Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def;
            begin

               if Find_Digits then
                  Digits_Value :=
                    Natural'Value (Float_Def.F_Num_Digits.P_Eval_As_Int.Image);
               end if;

               if Find_Range and then not Is_Null (Float_Def.F_Range) then
                  Has_Range := True;
                  declare
                     Max_Eval : constant Eval_Result :=
                       Expr_Eval (Float_Def.F_Range.F_Range.As_Bin_Op.F_Right);
                     Min_Eval : constant Eval_Result :=
                       Expr_Eval (Float_Def.F_Range.F_Range.As_Bin_Op.F_Left);
                     --  RM 3.5.7 (3) indicates that a floating point def range
                     --  spec should be of the form "range A .. B" so the
                     --  conversion to Bin_Op should be safe here.
                  begin
                     if Max_Eval.Kind /= Real or else Min_Eval.Kind /= Real
                     then
                        raise Translation_Error
                          with "Wrong eval type for float range bounds";
                     end if;
                     Range_Value.Max := Max_Eval.Real_Result;
                     Range_Value.Min := Min_Eval.Real_Result;
                  end;
               elsif Find_Range then
                  Has_Range := False;
               end if;
            end;
         else
            --  Decl is either a subtype decl or a derived type decl. Check if
            --  there are constraints associated with this decl.

            if Kind (Decl) in Ada_Subtype_Decl_Range then
               Parent_Type := Decl.As_Subtype_Decl.F_Subtype;
            elsif Kind (Decl.As_Type_Decl.F_Type_Def) in Ada_Derived_Type_Def
            then
               Parent_Type :=
                 Decl.As_Type_Decl.F_Type_Def.As_Derived_Type_Def
                   .F_Subtype_Indication;
            end if;
            if Is_Null (Parent_Type.F_Constraint) then

               --  If there aren't any constraints in the subtype indication,
               --  try to find the type properites on the referenced type.

               Translate_Float_Decl_Internal
                 (Parent_Type.P_Designated_Type_Decl, Find_Digits, Find_Range,
                  Digits_Value, Has_Range, Range_Value);
            else

               --  Otherwise, analyze the type constraint

               Constraints := Parent_Type.F_Constraint;
               case Kind (Constraints) is
                  when Ada_Range_Constraint_Range =>
                     Range_Spec_Val := Constraints.As_Range_Constraint.F_Range;
                  when Ada_Digits_Constraint =>
                     Range_Spec_Val :=
                       Constraints.As_Digits_Constraint.F_Range;
                  when others =>
                     raise Translation_Error with "Unexpected kind of"
                       & " constraint for float subtype indication";
               end case;

               --  First comes the digits constraint, which can be direcly
               --  statically evaluated.

               if Find_Digits
                  and then Kind (Constraints) in Ada_Digits_Constraint_Range
                  and then not Is_Null
                                 (Constraints.As_Digits_Constraint.F_Digits)
               then
                  Digits_Value :=
                    Natural'Value (Constraints.As_Digits_Constraint
                                   .F_Digits.P_Eval_As_Int.Image);
               elsif Find_Digits then
                  Translate_Float_Decl_Internal
                    (Parent_Type.P_Designated_Type_Decl, Find_Digits, False,
                     Digits_Value, Has_Range, Range_Value);
               end if;

               --  Then come the range constraint. Here, we can either have
               --  no range constraint (in that case, look them up in the type
               --  referenced in the subtype indication), a "range Low .. High"
               --  expression (in which case we can statically evaluate the
               --  bounds) or a 'Range attribute ref, in which case we will
               --  determine the range constraints from the type refernced in
               --  the prefix of the range attribute.

               if Find_Range and then not Is_Null (Range_Spec_Val) then
                  if Kind (Range_Spec_Val.F_Range) in Ada_Bin_Op_Range
                  then
                     declare
                        Min_Eval : constant Eval_Result :=
                          Expr_Eval
                            (Range_Spec_Val.F_Range.As_Bin_Op.F_Left);
                        Max_Eval : constant Eval_Result :=
                          Expr_Eval
                            (Range_Spec_Val.F_Range.As_Bin_Op.F_Right);
                     begin
                        if Min_Eval.Kind /= Real
                          or else Max_Eval.Kind /= Real
                        then
                           raise Translation_Error
                             with "Wrong eval type for float range bounds";
                        end if;
                        Has_Range       := True;
                        Range_Value.Min := Min_Eval.Real_Result;
                        Range_Value.Max := Max_Eval.Real_Result;
                     end;
                  else
                     if
                       (not (Kind (Range_Spec_Val.F_Range) in
                          Ada_Attribute_Ref_Range))
                       or else
                         Range_Spec_Val.F_Range.As_Attribute_Ref
                           .As_Attribute_Ref
                           .Text /= "Range"
                     then
                        raise Translation_Error
                          with "Did not find range attribute ref or bin op in"
                           & " float range constraint";
                     end if;
                     declare
                        Range_Constraint_Type : constant Base_Type_Decl :=
                          Range_Spec_Val.F_Range.As_Attribute_Ref.F_Prefix
                            .P_Referenced_Decl
                            .As_Base_Type_Decl;
                     begin
                        Translate_Float_Decl_Internal
                          (Range_Constraint_Type, False, Find_Range,
                           Digits_Value, Has_Range, Range_Value);
                     end;
                  end if;

               elsif Find_Range then
                  Translate_Float_Decl_Internal
                    (Parent_Type.P_Designated_Type_Decl, Find_Digits, False,
                     Digits_Value, Has_Range, Range_Value);
               end if;
            end if;
         end if;
      end Translate_Float_Decl_Internal;

      Digits_Value : Natural     := 0;
      Has_Range    : Boolean     := False;
      Range_Value  : Float_Range := (Min => 0.0, Max => 0.0);
   begin
      Translate_Float_Decl_Internal
        (Decl, Find_Digits => True, Find_Range => True,
         Digits_Value      => Digits_Value, Has_Range => Has_Range,
         Range_Value       => Range_Value);
      if Has_Range then
         return
           (Success => True,
            Res     =>
              new Float_Typ'
                (Has_Range    => True, Name => Decl.P_Defining_Name,
                 Digits_Value => Digits_Value, Range_Value => Range_Value));
      else
         return
           (Success => True,
            Res     =>
              new Float_Typ'
                (Has_Range    => False, Name => Decl.P_Defining_Name,
                 Digits_Value => Digits_Value));
      end if;
   exception
      when Exc : Translation_Error =>
         return
           (Success     => False,
            Diagnostics => +Ada.Exceptions.Exception_Message (Exc));
   end Translate_Float_Decl;

   function Translate (N : LAL.Type_Expr) return Translation_Result is
      Type_Decl_Node : Base_Type_Decl;
      Root_Type      : Base_Type_Decl;
   begin
      if Kind (N) in Ada_Anonymous_Type then
         return
           (Success     => False,
            Diagnostics => +"Anonymous types not supported yet");
      end if;

      Type_Decl_Node :=
        N.As_Subtype_Indication.P_Designated_Type_Decl.P_Full_View;
      Root_Type := Type_Decl_Node.P_Root_Type;

      if not Type_Decl_Node.P_Is_Static_Decl then
         return
           (Success     => False,
            Diagnostics => +"Non static subtypes not supported yet");
      end if;

      if Type_Decl_Node.P_Is_Int_Type then
         return Translate_Int_Decl (Type_Decl_Node);
      elsif P_Is_Derived_Type
          (Node       => Type_Decl_Node,
           Other_Type => Type_Decl_Node.P_Bool_Type.As_Base_Type_Decl)
      then
         return
           (Success => True,
            Res     => new Bool_Typ'(Name => Type_Decl_Node.P_Defining_Name));
      elsif Type_Decl_Node.P_Is_Enum_Type then
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
                    new Char_Typ'(Name => Type_Decl_Node.P_Defining_Name));
            else
               return Translate_Enum_Decl (Type_Decl_Node, Root_Type);
            end if;
         end;

      elsif Type_Decl_Node.P_Is_Float_Type then
         return Translate_Float_Decl (Type_Decl_Node);
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
