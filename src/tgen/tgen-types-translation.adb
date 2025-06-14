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

with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.GMP.Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;

with Test.Common;

with TGen.LAL_Utils;            use TGen.LAL_Utils;
with TGen.Types.Array_Types;    use TGen.Types.Array_Types;
with TGen.Types.Constraints;    use TGen.Types.Constraints;
with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;
with TGen.Types.Enum_Types;     use TGen.Types.Enum_Types;
with TGen.Types.Int_Types;      use TGen.Types.Int_Types;
with TGen.Types.Real_Types;     use TGen.Types.Real_Types;
with TGen.Types.Record_Types;   use TGen.Types.Record_Types;
with TGen.Marshalling;
with TGen.Numerics;
with TGen.Strings;

package body TGen.Types.Translation is

   Translation_Error : exception;

   Non_Static_Error : exception;
   --  Exception raised when the translation of a type that should be static
   --  ends up not being static, due to missing bits in the static evaluator
   --  in LAL.

   package LALCO renames Libadalang.Common;

   type Nondiscriminated_Record_Typ_Access is
     access Nondiscriminated_Record_Typ;
   type Discriminated_Record_Typ_Access is access Discriminated_Record_Typ;
   type Function_Typ_Access is access Function_Typ;

   function New_Eval_As_Int
     (Node : Expr'Class) return GNATCOLL.GMP.Integers.Big_Integer;
   --  Wrapper arround P_Eval_As_Int which raises Non_Static_Error when
   --  something that should be static turns out not to be due to a LAL
   --  limitation.

   Verbose_Diag : Boolean := False;
   package Text renames Langkit_Support.Text;

   function Get_From_Cache
     (FQN : Ada_Qualified_Name; T : out Typ_Access) return Boolean;
   --  Try to get a type named FQN from the cache. If the lookup is
   --  succesful, return True and set T to the cached translation.

   Cache_Hits : Natural := 0;
   Cache_Miss : Natural := 0;
   --  Stats for the cache

   type Local_Ada_Node_Arr is array (Positive range <>) of Ada_Node;
   --  Like Ada_Node_List, but that we can build ourselves

   function Translate_Internal
     (N                 : LAL.Base_Type_Decl;
      Verbose           : Boolean := False;
      Assume_Non_Static : Boolean := False) return Translation_Result;
   --  Actually translates the Base_Type_Decl. Translate is simply a
   --  memoization wrapper.
   --  If Assume_Non_Static is true, the the translated type will always be
   --  flaged as non static.

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result;

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
      return Translation_Result;

   function Translate_Char_Decl
     (Decl : Base_Type_Decl) return Translation_Result;

   function Translate_Float_Decl
     (Decl : Base_Type_Decl) return Translation_Result;

   function Translate_Ordinary_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result;

   function Translate_Decimal_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result;

   procedure Translate_Float_Range
     (Decl      : Base_Type_Decl;
      Has_Range : out Boolean;
      Min, Max  : out Big_Reals.Big_Real);

   function Extract_Real_Range_Spec
     (Node : LAL.Constraint) return LAL.Range_Spec;
   --  Analyze contraint to determine if there are range constraints in Node,
   --  and if so, return the associated Range_Spec.

   function Translate_Real_Range_Spec
     (Node : LAL.Range_Spec) return Real_Range_Constraint;
   --  Translate a Range_Spec (Assumed to be of a real type)
   --  For practical reasons, the the range spec is an attribute reference to
   --  a real type for which no range is defined, this will default to
   --  Long_Float'First .. Long_Float'Last

   function Translate_Array_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   with Pre => Decl.P_Root_Type.P_Full_View.P_Is_Array_Type;

   function Translate_Component_Decl_List
     (Decl_List : Ada_Node_List; Res : in out Component_Maps.Map)
      return Unbounded_String;
   --  Translate the list of components of Decl into Res.
   --  If the returned string is empty then the results are valid, otherwise
   --  and error occured during translation and the contents of Res should
   --  not be used. The returned string contains the diagnostics of the
   --  translation

   function Translate_Variant_Part
     (Node : LAL.Variant_Part; Discriminants : Component_Maps.Map)
      return Record_Types.Variant_Part
   with Pre => (not Node.Is_Null);

   procedure Subtract_Choice_From_Other
     (Others_Cur : Variant_Choice_Lists.Cursor;
      Choice     : Variant_Choice;
      List       : in out Variant_Choice_Lists.List);
   --  Subtract the Integer ranges that correspond to the matching alternatives
   --  in Choice.Alt_Set from the corresponding set in the variant
   --  choice denoted by Others_Cur.

   function Gather_Index_Constraint_Nodes
     (Decl_Or_Constraint : Ada_Node'Class; Num_Dims : Positive)
      return Local_Ada_Node_Arr;
   --  Collect all the constraints on the indexes that are present in
   --  Decl_Or_Constraint, for which the kind should be one of Base_Type_Decl
   --  or Constraint. Num_Dims should match the number of constraints defined
   --  in Decl_Or_Constraint.

   function Translate_Record_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   with Pre => Decl.P_Root_Type.P_Full_View.P_Is_Record_Type;

   procedure Apply_Record_Subtype_Decl
     (Decl : Subtype_Indication; Res : in out Discriminated_Record_Typ)
   with Pre => Res.Constrained;
   --  Record the discriminant constraints of Decl in Res. For this, the
   --  type on which you want to apply constraints must be able to accept
   --  them.

   function Apply_Record_Derived_Type_Decl
     (Decl : Type_Decl'Class; From : in out Discriminated_Record_Typ)
      return Discriminated_Record_Typ
   with
     Pre =>
       Kind (Decl.F_Type_Def) in Ada_Derived_Type_Def_Range
       and then From.Constrained;
   --  Apply the effects of the record type derivation defined in Decl.
   --  If any discriminant constraints are present, this filters out the
   --  incompatible shapes, and renames discriminant which correspond
   --  between the ancestor type and the child type.

   procedure Filter_Variant_Part
     (Variant       : in out Variant_Part_Acc;
      TL_Components : in out Component_Maps.Map;
      Constraints   : Discriminant_Constraint_Maps.Map;
      Renaming      : Discriminant_Constraint_Maps.Map);
   --  Filter the unreachable shapes of Variant based on the constraints in
   --  Constraints, and rename the variant part discriminant based on the
   --  mapping in Renaming.

   function Record_Constrained
     (Decl : Base_Type_Decl; Root : Base_Type_Decl) return Boolean;
   --  Returns True if Decl has discriminants constraints at some stage in the
   --  chain of subtype definitions / type derivations.

   function Eval_Discrete_Range
     (Rng : Discrete_Range)
      return TGen.Types.Constraints.Discrete_Range_Constraint;

   function Translate_Discrete_Range_Constraint
     (Node : LAL.Range_Constraint) return Discrete_Range_Constraint;
   --  Translate a range constraint that applies to a discrete type

   function Translate_Real_Constraints
     (Node : LAL.Constraint) return TGen.Types.Constraints.Constraint'Class;
   --  Translate constraints that apply to a real type. This can return
   --  either a Real_Range_Constraint or a Digits_Constraint.
   --  ??  Implement delta constraints and digits constraints for decimal fixed
   --  points.

   function Translate_Index_Constraints
     (Node : LAL.Constraint; Num_Dims : Positive)
      return TGen.Types.Constraints.Index_Constraints;

   function Translate_Discriminant_Constraints
     (Node : LAL.Composite_Constraint)
      return TGen.Types.Constraints.Discriminant_Constraints;

   function Variant_Support_Static_Gen (Var : Variant_Part_Acc) return Boolean;

   function Var_Choice_Supports_Static_Gen
     (Choice : Variant_Choice) return Boolean;

   function "+" (Text : Text_Type) return Unbounded_String
   is (+(+Text));

   function "+" (Text : Unbounded_Text_Type) return Unbounded_String
   is (TGen.Types.Translation."+" (+Text));

   function Decl_Is_Fully_Private (N : Basic_Decl'Class) return Boolean;
   --  Return whether N is fully private, i.e. whether the first declaration of
   --  N is in a private part, and can't thus be used outside the private parts
   --  of its declaration unit or child units.

   --------------
   -- PP_Cache --
   --------------

   procedure PP_Cache is
      use Translation_Maps;
      Cache_Cur : Cursor := Translation_Cache.First;
   begin
      while Has_Element (Cache_Cur) loop
         Put_Line
           (To_Ada (Key (Cache_Cur)) & " => " & Element (Cache_Cur).all.Image);
         Next (Cache_Cur);
      end loop;
   end PP_Cache;

   --------------------
   -- Get_From_Cache --
   --------------------

   function Get_From_Cache
     (FQN : Ada_Qualified_Name; T : out Typ_Access) return Boolean
   is
      use Translation_Maps;
      Cache_Cur : constant Cursor := Translation_Cache.Find (FQN);
   begin
      --  If we have the type name in the cache, return it

      if Cache_Cur /= No_Element then
         Cache_Hits := Cache_Hits + 1;
         T := Element (Cache_Cur);
         return True;
      end if;

      Cache_Miss := Cache_Miss + 1;
      return False;
   end Get_From_Cache;

   ------------------------------------
   -- Var_Choice_Supports_Static_Gen --
   ------------------------------------

   function Var_Choice_Supports_Static_Gen
     (Choice : Variant_Choice) return Boolean
   is ((for all Comp_Ref of Choice.Components =>
          Comp_Ref.all.Supports_Static_Gen)
       and then Variant_Support_Static_Gen (Choice.Variant));

   --------------------------------
   -- Variant_Support_Static_Gen --
   --------------------------------

   function Variant_Support_Static_Gen (Var : Variant_Part_Acc) return Boolean
   is (Var = null
       or else (for all Choice of Var.all.Variant_Choices =>
                  Var_Choice_Supports_Static_Gen (Choice)));

   ----------------------
   -- New_Eval_As_Int --
   ----------------------

   function New_Eval_As_Int
     (Node : Expr'Class) return GNATCOLL.GMP.Integers.Big_Integer is
   begin
      return Node.P_Eval_As_Int;
   exception
      when Exc : Property_Error =>
         declare
            Error_Msg : constant String :=
              Ada.Exceptions.Exception_Message (Exc);
         begin
            if Error_Msg'Length >= 9 and then Error_Msg (1 .. 9) = "Unhandled"
            then

               --  Quick and dirty heuristic: cases where LAL should be able
               --  to statically evaluate the expression but isn't able to have
               --  an exception message that starts with "Unhandled"

               raise Non_Static_Error with Error_Msg;
            else

               --  We still want the Property_Error to propagate when we are
               --  not using the static evaluator correctly

               raise;
            end if;
         end;
   end New_Eval_As_Int;

   ---------------------------
   -- Decl_Is_Fully_Private --
   ---------------------------

   function Decl_Is_Fully_Private (N : Basic_Decl'Class) return Boolean is
      First_Part : constant Basic_Decl := N.P_All_Parts (1);
      Sem_Parent : Ada_Node := First_Part.P_Semantic_Parent;
   begin
      --  Consider that N is fully private if there is a private part node
      --  among the chain of semantic parents of the first part of N, until we
      --  reach a library level package declaration.

      while not Sem_Parent.Is_Null
        and then not (Sem_Parent.Kind in Ada_Package_Decl_Range
                      and then Sem_Parent.Parent.Kind
                               in Ada_Library_Item_Range)
      loop
         if Sem_Parent.Kind in Ada_Private_Part_Range then
            return True;
         end if;
         Sem_Parent := Sem_Parent.P_Semantic_Parent;
      end loop;
      return False;
   end Decl_Is_Fully_Private;

   ------------------------
   -- Translate_Int_Decl --
   ------------------------

   function Translate_Int_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max, Min : Big_Integer;
      --  Static evaluations of the bounds, if available

      Is_Actually_Static : Boolean := Decl.P_Is_Static_Decl;
      --  Sometimes LAL reports a declaration as static, but isn't able to
      --  evaluate the bounds of the type, we thus have to consider the type as
      --  non static. To do so we have no choice but to try to evaluate the
      --  bounds, and see if we get an exception.

      Is_Mod_Typ : constant Boolean :=
        Decl.P_Root_Type.P_Full_View.As_Concrete_Type_Decl.F_Type_Def.Kind
        in Ada_Mod_Int_Type_Def;
   begin
      if High_Bound (Rang).Is_Null then
         Is_Actually_Static := False;
      end if;
      if Is_Actually_Static then
         begin
            Max :=
              Big_Int.From_String (New_Eval_As_Int (High_Bound (Rang)).Image);
         exception
            when Non_Static_Error =>
               Max := Big_Int.To_Big_Integer (0);
               Is_Actually_Static := False;
         end;
      end if;

      if Is_Mod_Typ then
         --  ???modular subtypes can actually have a lower bound different than
         --  zero. We need to change the type representation to account for
         --  this.

         if Is_Actually_Static then
            return Res : Translation_Result (Success => True) do
               Res.Res :=
                 new Mod_Int_Typ'
                   (Is_Static   => True,
                    Mod_Value   =>
                      Big_Int.From_String
                        (New_Eval_As_Int
                           (Decl
                              .P_Root_Type
                              .P_Full_View
                              .As_Concrete_Type_Decl
                              .F_Type_Def
                              .As_Mod_Int_Type_Def
                              .F_Expr)
                           .Image),
                    Range_Value => (0, Max),
                    others      => <>);
            end return;
         else
            return Res : Translation_Result (Success => True) do
               Res.Res := new Mod_Int_Typ'(Is_Static => False, others => <>);
            end return;
         end if;
      end if;
      --  We are not dealing with a mod type, let's evaluate the low bound

      if Low_Bound (Rang).Is_Null then
         Is_Actually_Static := False;
      end if;
      if Is_Actually_Static then
         begin
            Min :=
              Big_Int.From_String (New_Eval_As_Int (Low_Bound (Rang)).Image);
         exception
            when Non_Static_Error =>
               Min := Big_Int.To_Big_Integer (0);
               Is_Actually_Static := False;
         end;
      end if;
      if Is_Actually_Static then
         return Res : Translation_Result (Success => True) do
            Res.Res :=
              new Signed_Int_Typ'
                (Is_Static   => True,
                 Range_Value => (Min => Min, Max => Max),
                 others      => <>);
         end return;
      else
         return Res : Translation_Result (Success => True) do
            Res.Res := new Signed_Int_Typ'(Is_Static => False, others => <>);
         end return;
      end if;
   end Translate_Int_Decl;

   -------------------------
   -- Translate_Char_Decl --
   -------------------------

   function Translate_Char_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Rang : constant Discrete_Range := Decl.P_Discrete_Range;
   begin

      if Is_Null (Low_Bound (Rang)) then
         return Res : Translation_Result (Success => True) do
            Res.Res :=
              new Char_Typ'
                (Is_Static => True, Has_Range => False, others => <>);
         end return;
      else
         declare
            LB, HB : Discrete_Constraint_Value;
         begin
            if Low_Bound (Rang).P_Is_Static_Expr then
               LB :=
                 (Kind    => Static,
                  Int_Val =>
                    Big_Int.From_String
                      (New_Eval_As_Int (Low_Bound (Rang)).Image));
            else
               LB := (Kind => Non_Static, Text => +Low_Bound (Rang).Text);
            end if;
            if High_Bound (Rang).P_Is_Static_Expr then
               HB :=
                 (Kind    => Static,
                  Int_Val =>
                    Big_Int.From_String
                      (New_Eval_As_Int (High_Bound (Rang)).Image));
            else
               HB := (Kind => Non_Static, Text => +High_Bound (Rang).Text);
            end if;
            if LB.Kind = Static and then HB.Kind = Static then
               return Res : Translation_Result (Success => True) do
                  Res.Res :=
                    new Char_Typ'
                      (Is_Static   => True,
                       Has_Range   => True,
                       Range_Value => (Low_Bound => LB, High_Bound => HB),
                       others      => <>);

               end return;
            else
               return Res : Translation_Result (Success => True) do
                  Res.Res :=
                    new Char_Typ'
                      (Is_Static   => False,
                       Has_Range   => True,
                       Range_Value => (Low_Bound => LB, High_Bound => HB),
                       others      => <>);

               end return;
            end if;
         end;
      end if;
   end Translate_Char_Decl;

   -------------------------
   -- Translate_Enum_Decl --
   -------------------------

   function Translate_Enum_Decl
     (Decl : Base_Type_Decl; Root_Enum_Decl : Base_Type_Decl)
      return Translation_Result
   is
      package Long_Long_Conversion is new
        Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      use Long_Long_Conversion;

      Enum_Lits : Enum_Literal_Maps.Map;

      Index : Long_Long_Integer := 0;

      Rang : constant Discrete_Range := Decl.P_Discrete_Range;

      Max, Min : Long_Long_Integer;

   begin
      for Literal of
        Root_Enum_Decl.As_Type_Decl.F_Type_Def.As_Enum_Type_Def.F_Enum_Literals
      loop
         Enum_Lits.Insert (To_Big_Integer (Index), +Literal.F_Name.Text);
         Index := Index + 1;
      end loop;

      if not Is_Null (High_Bound (Rang))
        and then not Is_Null (Low_Bound (Rang))
      then
         Max :=
           Long_Long_Integer'Value (New_Eval_As_Int (High_Bound (Rang)).Image);
         Min :=
           Long_Long_Integer'Value (New_Eval_As_Int (Low_Bound (Rang)).Image);
         for Pos in From_Big_Integer (Enum_Lits.First_Key) .. Min - 1 loop
            Enum_Lits.Delete (To_Big_Integer (Pos));
         end loop;

         for Pos in Max + 1 .. From_Big_Integer (Enum_Lits.Last_Key) loop
            Enum_Lits.Delete (To_Big_Integer (Pos));
         end loop;
      end if;

      return Res : Translation_Result (Success => True) do
         Res.Res :=
           new Other_Enum_Typ'
             (Is_Static => True, Literals => Enum_Lits, others => <>);
      end return;
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
         Constraints : LAL.Digits_Constraint;
      begin
         if Decl = Decl.P_Root_Type then

            --  Decl is the root type decl, so we only need to translate the
            --  type definition.

            Digits_Value :=
              Natural'Value
                (New_Eval_As_Int
                   (Decl
                      .As_Type_Decl
                      .F_Type_Def
                      .As_Floating_Point_Def
                      .F_Num_Digits)
                   .Image);
            return;
         end if;

         --  Decl is either a subtype decl or a derived type decl. Check if
         --  there are constraints associated with this decl.

         if Kind (Decl) in Ada_Subtype_Decl_Range then
            Parent_Type := Decl.As_Subtype_Decl.F_Subtype;

         elsif Kind (Decl.As_Type_Decl.F_Type_Def)
               in Ada_Derived_Type_Def_Range
         then
            Parent_Type :=
              Decl
                .As_Type_Decl
                .F_Type_Def
                .As_Derived_Type_Def
                .F_Subtype_Indication;
         else
            raise Translation_Error
              with
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
                    Natural'Value
                      (New_Eval_As_Int (Constraints.F_Digits).Image);

               when others =>
                  raise Translation_Error
                    with
                      "Unexpected kind of"
                      & " constraint for float subtype indication: "
                      & Kind_Name (Constraints);
            end case;
         end if;
      end Find_Digits;

      Digits_Value : Natural := 0;
      Has_Range    : Boolean;
      Min, Max     : Big_Reals.Big_Real;

      Res : Translation_Result (Success => True);

      --  Start processing for Translate_Float_Decl

   begin
      Find_Digits (Decl, Digits_Value);
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      if Has_Range then
         Res.Res :=
           new Float_Typ'
             (Is_Static    => True,
              Has_Range    => True,
              Digits_Value => Digits_Value,
              Range_Value  => (Min => Min, Max => Max),
              others       => <>);
      else
         Res.Res :=
           new Float_Typ'
             (Is_Static    => True,
              Has_Range    => False,
              Digits_Value => Digits_Value,
              others       => <>);
      end if;
      return Res;
   end Translate_Float_Decl;

   -----------------------------------
   -- Translate_Ordinary_Fixed_Decl --
   -----------------------------------

   function Translate_Ordinary_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Min, Max    : Big_Real;
      Delta_Value : Big_Real;
      Has_Range   : Boolean;

      procedure Find_Delta (Decl : Base_Type_Decl; Delta_Value : out Big_Real);
      --  Travese the type hierachy from the bottom to find the inner most
      --  delta value of Decl.

      procedure Find_Delta (Decl : Base_Type_Decl; Delta_Value : out Big_Real)
      is
         Delta_Expr : Expr;
         --  Expr corresponding to the delta value, which will later be
         --  statically evaluated.

         Subtype_Ind : Subtype_Indication;
         --  Convininece variable to hold constraints to shorten then length of
         --  chained Libadalang dot calls.

         Subtype_Constraint : LAL.Constraint;
         --  Convininece variable to hold constraints to shorten then length of
         --  chained Libadalang dot calls.
      begin
         if Decl = Decl.P_Root_Type then

            --  First, the case where Decl is the root type, and thus we have a
            --  Ordinary_Fixed_Point_Def.

            pragma
              Assert
                (Kind (Decl.As_Type_Decl.F_Type_Def)
                 in Ada_Ordinary_Fixed_Point_Def_Range);
            Delta_Expr :=
              Decl.As_Type_Decl.F_Type_Def.As_Ordinary_Fixed_Point_Def.F_Delta;

         elsif Kind (Decl) in Ada_Subtype_Decl_Range
           or else (Kind (Decl) in Ada_Type_Decl
                    and then Kind (Decl.As_Type_Decl.F_Type_Def)
                             = Ada_Derived_Type_Def)
         then

            --  Case of a subtype decl or derived type decl, look at the
            --  subtype indication for a constraint, or look at the delta of
            --  the parent subtype.

            if Kind (Decl) in Ada_Subtype_Decl_Range then
               Subtype_Ind := Decl.As_Subtype_Decl.F_Subtype;
            else
               Subtype_Ind :=
                 Decl
                   .As_Type_Decl
                   .F_Type_Def
                   .As_Derived_Type_Def
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
                  Delta_Expr := Subtype_Constraint.As_Delta_Constraint.F_Delta;

               when Ada_Range_Constraint_Range =>

                  --  If we only have range constraints then look for the delta
                  --  value on the subtype designated by the subtype
                  --  indication.

                  Find_Delta
                    (Subtype_Ind.F_Name.P_Name_Designated_Type, Delta_Value);
                  return;

               when others =>
                  raise Translation_Error
                    with
                      "Unexpected constraint kind for a ordinary fixed point"
                      & " subtype declaration: "
                      & Kind_Name (Subtype_Constraint);
            end case;
         else
            raise Translation_Error
              with
                "Unexpected base type decl for a ordinary fixed point decl: "
                & Image (Decl);
         end if;

         declare
            Delta_Eval : constant Eval_Result := Expr_Eval (Delta_Expr);
         begin
            if Delta_Eval.Kind /= Real then
               raise Translation_Error with "wrong eval type for delta value";
            end if;
            Delta_Value :=
              TGen.Numerics.From_Universal_Image
                (Num => Delta_Eval.Real_Result.Numerator.Image,
                 Den => Delta_Eval.Real_Result.Denominator.Image);
         end;
      end Find_Delta;

      --  Start of processing for Translate_Ordinary_Fixed_Decl

   begin
      Translate_Float_Range (Decl, Has_Range, Min, Max);
      pragma Assert (Has_Range);
      Find_Delta (Decl, Delta_Value);
      return Res : Translation_Result (Success => True) do
         Res.Res :=
           new Ordinary_Fixed_Typ'
             (Is_Static   => True,
              Delta_Value => Delta_Value,
              Range_Value => (Min => Min, Max => Max),
              others      => <>);
      end return;
   exception
      when Exc : Translation_Error =>

         --  In case of translation error, return a non-static type,
         --  but print the information if verbose diagnostics are required.
         if Verbose_Diag then
            Put_Line
              ("Warning: could not determine static properties of"
               & " type"
               & Decl.Image
               & " : "
               & Ada.Exceptions.Exception_Message (Exc));
         end if;
         return Res : Translation_Result (Success => True) do
            Res.Res :=
              new Ordinary_Fixed_Typ'(Is_Static => False, others => <>);
         end return;
   end Translate_Ordinary_Fixed_Decl;

   ----------------------------------
   -- Translate_Decimal_Fixed_Decl --
   ----------------------------------

   function Translate_Decimal_Fixed_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      Delta_Val  : Big_Real;
      Digits_Val : Natural;
      Has_Range  : Boolean;

      Range_Min, Range_Max : Big_Real;

      procedure Find_Digits (Decl : Base_Type_Decl; Digits_Val : out Natural);

      procedure Find_Delta (Decl : Base_Type_Decl; Delta_Val : out Big_Real);

      -----------------
      -- Find_Digits --
      -----------------

      procedure Find_Digits (Decl : Base_Type_Decl; Digits_Val : out Natural)
      is
         Parent_Subtype : Subtype_Indication;
      begin
         case Kind (Decl) is
            when Ada_Type_Decl =>
               if Kind (Decl.As_Type_Decl.F_Type_Def)
                  in Ada_Decimal_Fixed_Point_Def_Range
               then
                  --  Simply translate the Digits value
                  Digits_Val :=
                    Natural'Value
                      (New_Eval_As_Int
                         (Decl
                            .As_Type_Decl
                            .F_Type_Def
                            .As_Decimal_Fixed_Point_Def
                            .F_Digits)
                         .Image);
                  return;
               elsif Kind (Decl.As_Type_Decl.F_Type_Def)
                     in Ada_Derived_Type_Def_Range
               then
                  Parent_Subtype :=
                    Decl
                      .As_Type_Decl
                      .F_Type_Def
                      .As_Derived_Type_Def
                      .F_Subtype_Indication;
               else
                  raise Translation_Error
                    with
                      "Unexpected kind for a type def translating a decimal"
                      & " fixed point type: "
                      & Kind_Name (Decl.As_Type_Decl.F_Type_Def);
               end if;

            when Ada_Subtype_Decl_Range =>
               Parent_Subtype := Decl.As_Subtype_Decl.F_Subtype;

            when others =>
               raise Translation_Error
                 with
                   "unexpected kind for a decimal fixed point declaration:"
                   & Kind_Name (Decl);
         end case;
         if Is_Null (Parent_Subtype.F_Constraint)
           or else not (Kind (Parent_Subtype.F_Constraint)
                        in Ada_Digits_Constraint_Range)
         then
            Find_Digits (Parent_Subtype.P_Designated_Type_Decl, Digits_Val);
            return;
         end if;

         --  Constraints are a digits constraint from this point on
         Digits_Val :=
           Natural'Value
             (New_Eval_As_Int
                (Parent_Subtype.F_Constraint.As_Digits_Constraint.F_Digits)
                .Image);
      end Find_Digits;

      ----------------
      -- Find_Delta --
      ----------------

      procedure Find_Delta (Decl : Base_Type_Decl; Delta_Val : out Big_Real) is
         --  There can be no delta constraints on a decimal fixed point type
         --  as per RM J.3 (5) so lets work on the type definition directly.

         Root_Typ : constant Type_Decl :=
           Decl.P_Root_Type.P_Full_View.As_Type_Decl;
         Eval_Res : constant Eval_Result :=
           Expr_Eval (Root_Typ.F_Type_Def.As_Decimal_Fixed_Point_Def.F_Delta);
      begin
         if Eval_Res.Kind /= Real then
            raise Translation_Error
              with
                "Evaluation of delta value for a decimal fixed point did not"
                & " return a real type";
         end if;
         Delta_Val :=
           TGen.Numerics.From_Universal_Image
             (Num => Eval_Res.Real_Result.Numerator.Image,
              Den => Eval_Res.Real_Result.Denominator.Image);
      end Find_Delta;

   begin
      Find_Delta (Decl, Delta_Val);
      Find_Digits (Decl, Digits_Val);
      Translate_Float_Range (Decl, Has_Range, Range_Min, Range_Max);
      if Has_Range then
         return Res : Translation_Result (Success => True) do
            Res.Res :=
              new Decimal_Fixed_Typ'
                (Is_Static    => True,
                 Has_Range    => True,
                 Digits_Value => Digits_Val,
                 Delta_Value  => Delta_Val,
                 Range_Value  => (Min => Range_Min, Max => Range_Max),
                 others       => <>);
         end return;
      else
         return Res : Translation_Result (Success => True) do
            Res.Res :=
              new Decimal_Fixed_Typ'
                (Is_Static    => True,
                 Has_Range    => False,
                 Digits_Value => Digits_Val,
                 Delta_Value  => Delta_Val,
                 others       => <>);
         end return;
      end if;
   end Translate_Decimal_Fixed_Decl;

   ---------------------------
   -- Translate_Float_Range --
   ---------------------------

   procedure Translate_Float_Range
     (Decl      : Base_Type_Decl;
      Has_Range : out Boolean;
      Min, Max  : out Big_Reals.Big_Real)
   is
      Root           : constant Type_Decl :=
        Decl.P_Root_Type.P_Full_View.As_Type_Decl;
      Parent_Type    : Subtype_Indication := No_Subtype_Indication;
      Range_Spec_Val : Range_Spec := No_Range_Spec;
   begin
      if Decl = Root then

         --  Decl is the root type, it is a type decl

         case Kind (Decl.As_Type_Decl.F_Type_Def) is
            when Ada_Floating_Point_Def_Range =>
               Range_Spec_Val :=
                 Decl.As_Type_Decl.F_Type_Def.As_Floating_Point_Def.F_Range;

            when Ada_Ordinary_Fixed_Point_Def_Range =>
               Range_Spec_Val :=
                 Decl
                   .As_Type_Decl
                   .F_Type_Def
                   .As_Ordinary_Fixed_Point_Def
                   .F_Range;

            when Ada_Decimal_Fixed_Point_Def_Range =>
               Range_Spec_Val :=
                 Decl
                   .As_Type_Decl
                   .F_Type_Def
                   .As_Decimal_Fixed_Point_Def
                   .F_Range;

            when others =>
               raise Translation_Error
                 with
                   "Expected Real type def for decl but got"
                   & Kind_Name (Decl.As_Type_Decl.F_Type_Def);
         end case;

         if Is_Null (Range_Spec_Val) then

            Has_Range := False;
            Min := TGen.Types.Big_Zero_F;
            Max := TGen.Types.Big_Zero_F;
            return;
         end if;

      else
         if Kind (Decl) in Ada_Type_Decl
           and then Kind (Decl.As_Type_Decl.F_Type_Def)
                    in Ada_Derived_Type_Def_Range
         then

            --  Decl is a derived type decl, look at the constraints in the
            --  subtype indication

            Parent_Type :=
              Decl
                .As_Type_Decl
                .F_Type_Def
                .As_Derived_Type_Def
                .F_Subtype_Indication;

         elsif Kind (Decl) in Ada_Subtype_Decl_Range then

            --  Same but for a subtype declaration

            Parent_Type := Decl.As_Subtype_Decl.F_Subtype;
         else
            raise Translation_Error
              with
                "Unexpected base type decl for a float type"
                & Kind_Name (Decl);
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

         Range_Spec_Val := Extract_Real_Range_Spec (Parent_Type.F_Constraint);
         if Is_Null (Range_Spec_Val) then
            Translate_Float_Range
              (Parent_Type.P_Designated_Type_Decl, Has_Range, Min, Max);
            return;
         end if;
      end if;

      declare
         Real_Rng : constant Real_Range_Constraint :=
           Translate_Real_Range_Spec (Range_Spec_Val);
      begin
         pragma
           Assert
             (Real_Rng.Low_Bound.Kind = Static
                and then Real_Rng.High_Bound.Kind = Static);
         Has_Range := True;
         Min := Real_Rng.Low_Bound.Real_Val;
         Max := Real_Rng.High_Bound.Real_Val;
      end;
   end Translate_Float_Range;

   function Extract_Real_Range_Spec
     (Node : LAL.Constraint) return LAL.Range_Spec is
   begin
      case Kind (Node) is
         when Ada_Range_Constraint_Range =>
            return Node.As_Range_Constraint.F_Range;

         when Ada_Digits_Constraint_Range =>
            return Node.As_Digits_Constraint.F_Range;

         when Ada_Delta_Constraint_Range =>
            return Node.As_Delta_Constraint.F_Range;

         when others =>
            raise Translation_Error
              with
                "Unexpected kind of constraint for a real type "
                & Kind_Name (Node);
      end case;
   end Extract_Real_Range_Spec;

   function Translate_Real_Range_Spec
     (Node : LAL.Range_Spec) return Real_Range_Constraint
   is
      Min, Max               : Big_Reals.Big_Real;
      Min_Text, Max_Text     : Unbounded_Text_Type;
      Has_Range              : Boolean;
      Min_Static, Max_Static : Boolean;
   begin
      case Kind (Node.F_Range) is

         --  According to RM 3.5 (3) a range constraint can only be of the form
         --  "Min .. Max" or "Name'Range", and assume we are analyzing a well
         --  formed AST.

         when Ada_Attribute_Ref_Range =>
            if Node.F_Range.P_Is_Static_Expr then
               Translate_Float_Range
                 (Node
                    .F_Range
                    .As_Attribute_Ref
                    .F_Prefix
                    .P_Referenced_Decl
                    .As_Base_Type_Decl,
                  Has_Range,
                  Min,
                  Max);
               if not Has_Range then
                  Min := LF_Conversions.To_Big_Real (Long_Float'First);
                  Max := LF_Conversions.To_Big_Real (Long_Float'Last);
               end if;
               Min_Static := True;
               Max_Static := True;
            else
               Max_Text := +Node.Text;
               Min_Static := False;
               Max_Static := False;
            end if;

         when Ada_Bin_Op_Range =>
            if Node.F_Range.As_Bin_Op.F_Left.P_Is_Static_Expr then
               declare
                  Min_Eval : constant Eval_Result :=
                    Expr_Eval (Node.F_Range.As_Bin_Op.F_Left);
               begin
                  if Min_Eval.Kind /= Real then
                     raise Translation_Error
                       with
                         "Wrong type of static eval for real range"
                         & " constraint.";
                  end if;
                  --  Put_Line (Node.F_Range.As_Bin_Op.F_Left.Text'Image);
                  --  Put_Line (Min_Eval.Real_Result.Numerator'Image);
                  --  Put_Line (Min_Eval.Real_Result.Denominator'Image);
                  Min :=
                    TGen.Numerics.From_Universal_Image
                      (Num => Min_Eval.Real_Result.Numerator.Image,
                       Den => Min_Eval.Real_Result.Denominator.Image);
                  Min_Static := True;
               exception
                  when Exc : Storage_Error =>
                     raise Translation_Error
                       with
                         "Technical limitation: "
                         & Ada.Exceptions.Exception_Message (Exc);
               end;
            else
               Min_Text := +Node.F_Range.As_Bin_Op.F_Left.Text;
               Min_Static := False;
            end if;
            if Node.F_Range.As_Bin_Op.F_Right.P_Is_Static_Expr then
               declare
                  Max_Eval : constant Eval_Result :=
                    Expr_Eval (Node.F_Range.As_Bin_Op.F_Right);
               begin
                  if Max_Eval.Kind /= Real then
                     raise Translation_Error
                       with
                         "Wrong type of static eval for real range"
                         & " constraint.";
                  end if;
                  Max :=
                    TGen.Numerics.From_Universal_Image
                      (Num => Max_Eval.Real_Result.Numerator.Image,
                       Den => Max_Eval.Real_Result.Denominator.Image);
                  Max_Static := True;
               exception
                  when Exc : Storage_Error =>
                     raise Translation_Error
                       with
                         "Technical limitation: "
                         & Ada.Exceptions.Exception_Message (Exc);
               end;
            else
               Max_Text := +Node.F_Range.As_Bin_Op.F_Right.Text;
               Max_Static := False;
            end if;

         when others =>
            raise Translation_Error
              with
                "Unexpected expression kind for real range constraint: "
                & Kind_Name (Node.F_Range);
      end case;

      if Min_Static and then Max_Static then
         return
           Real_Range_Constraint'
             (Low_Bound  => (Kind => Static, Real_Val => Min),
              High_Bound => (Kind => Static, Real_Val => Max));
      elsif Min_Static then
         return
           Real_Range_Constraint'
             (Low_Bound  => (Kind => Static, Real_Val => Min),
              High_Bound => (Kind => Non_Static, Text => +Max_Text));
      elsif Max_Static then
         return
           Real_Range_Constraint'
             (Low_Bound  => (Kind => Non_Static, Text => +Min_Text),
              High_Bound => (Kind => Static, Real_Val => Max));
      else
         return
           Real_Range_Constraint'
             (Low_Bound  => (Kind => Non_Static, Text => +Min_Text),
              High_Bound => (Kind => Non_Static, Text => +Max_Text));
      end if;

   end Translate_Real_Range_Spec;

   -----------------------------------
   -- Gather_Index_Constraint_Nodes --
   -----------------------------------

   function Gather_Index_Constraint_Nodes
     (Decl_Or_Constraint : Ada_Node'Class; Num_Dims : Positive)
      return Local_Ada_Node_Arr
   is
      Res           : Local_Ada_Node_Arr (1 .. Num_Dims);
      Current_Index : Positive := 1;
      Constraints   : LAL.Constraint;
   begin
      case Kind (Decl_Or_Constraint) is
         when Ada_Type_Decl =>
            case Kind (Decl_Or_Constraint.As_Type_Decl.F_Type_Def) is
               when Ada_Array_Type_Def_Range =>
                  for Node of
                    Decl_Or_Constraint
                      .As_Type_Decl
                      .F_Type_Def
                      .As_Array_Type_Def
                      .F_Indices
                      .As_Constrained_Array_Indices
                      .F_List
                  loop
                     Res (Current_Index) := Node.As_Ada_Node;
                     Current_Index := Current_Index + 1;
                  end loop;
                  return Res;

               when Ada_Derived_Type_Def_Range =>
                  Constraints :=
                    Decl_Or_Constraint
                      .As_Type_Decl
                      .F_Type_Def
                      .As_Derived_Type_Def
                      .F_Subtype_Indication
                      .F_Constraint;

               when others =>
                  raise Translation_Error
                    with
                      "unexpected kind for index constraints in constrained"
                      & " array type declaration: "
                      & Kind_Name (Decl_Or_Constraint);
            end case;

         when Ada_Subtype_Decl_Range =>
            Constraints :=
              Decl_Or_Constraint.As_Subtype_Decl.F_Subtype.F_Constraint;

         when Ada_Constraint =>
            Constraints := Decl_Or_Constraint.As_Constraint;

         when others =>
            raise Translation_Error
              with
                "unexpected kind for index constraints: "
                & Kind_Name (Decl_Or_Constraint);
      end case;

      case Kind (Constraints) is
         when Ada_Composite_Constraint_Range =>
            for Node of Constraints.As_Composite_Constraint.F_Constraints loop
               Res (Current_Index) :=
                 Node.As_Composite_Constraint_Assoc.F_Constraint_Expr;
               Current_Index := Current_Index + 1;
            end loop;

         when others =>
            raise Translation_Error
              with
                "unexpected kind for index constraints: "
                & Kind_Name (Constraints);
      end case;
      return Res;
   end Gather_Index_Constraint_Nodes;

   --------------------------
   -- Translate_Array_Decl --
   --------------------------

   function Translate_Array_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is
      function Translate_Constrained
        (Decl : Base_Type_Decl) return Translation_Result;

      function Translate_Unconstrained
        (Def : Array_Type_Def) return Translation_Result;

      ---------------------------
      -- Translate_Constrained --
      ---------------------------

      function Translate_Constrained
        (Decl : Base_Type_Decl) return Translation_Result
      is

         Cmp_Typ_Def : constant Component_Def :=
           Decl
             .P_Root_Type
             .P_Full_View
             .As_Type_Decl
             .F_Type_Def
             .As_Array_Type_Def
             .F_Component_Type;
         Num_Indices : Natural := 0;
         Total_Size  : Big_Integer;
      begin
         --  Compute the number of indices

         while not Is_Null (Decl.P_Index_Type (Num_Indices)) loop
            Num_Indices := Num_Indices + 1;
         end loop;

         declare
            Constraint_Nodes : constant Local_Ada_Node_Arr :=
              Gather_Index_Constraint_Nodes (Decl.As_Ada_Node, Num_Indices);

            type Constrained_Array_Typ_Access is
              access Constrained_Array_Typ (Num_Dims => Num_Indices);

            Res_Typ : constant Constrained_Array_Typ_Access :=
              new Constrained_Array_Typ'
                (Num_Dims          => Num_Indices,
                 Index_Constraints => [others => <>],
                 others            => <>);

            Component_Typ : constant Translation_Result :=
              Translate (Cmp_Typ_Def.F_Type_Expr, Verbose_Diag);
            --  This ignores any constraints on the element type that may
            --  appear in the component definition.

            Index_Typ                      : Base_Type_Decl;
            Has_Constraints                : Boolean;
            Range_Exp                      : Expr;
            Constraint_Min, Constraint_Max : Big_Integer;
            Min_Text, Max_Text             : Unbounded_Text_Type;
            Min_Static, Max_Static         : Boolean;

            Current_Index : Positive := 1;

            Failure_Reason : Unbounded_String;
         begin
            if not Component_Typ.Success then
               return
                 (Success     => False,
                  Diagnostics =>
                    "Failed to translate component type of"
                    & " array decl : "
                    & Component_Typ.Diagnostics);
            end if;
            Res_Typ.all.Component_Type := Component_Typ.Res;

            for Constraint of Constraint_Nodes loop
               Index_Typ := Decl.P_Index_Type (Current_Index - 1);
               case Kind (Constraint) is
                  when Ada_Subtype_Indication_Range =>
                     if Is_Null (Constraint.As_Subtype_Indication.F_Constraint)
                     then
                        Has_Constraints := False;
                     elsif Kind
                             (Constraint
                                .As_Subtype_Indication
                                .F_Constraint
                                .As_Range_Constraint
                                .F_Range
                                .F_Range)
                           in Ada_Attribute_Ref_Range | Ada_Bin_Op_Range
                     then
                        Range_Exp :=
                          Constraint
                            .As_Subtype_Indication
                            .F_Constraint
                            .As_Range_Constraint
                            .F_Range
                            .F_Range;

                        Has_Constraints := True;
                     end if;

                  when Ada_Bin_Op_Range =>
                     Has_Constraints := True;
                     Range_Exp := Constraint.As_Expr;

                  when Ada_Attribute_Ref_Range =>
                     Has_Constraints := True;
                     Range_Exp := Constraint.As_Expr;

                  when others =>
                     Has_Constraints := False;
               end case;

               declare
                  Index_Trans : constant Translation_Result :=
                    Translate (Index_Typ, Verbose_Diag);
               begin
                  if not Index_Trans.Success then
                     Failure_Reason :=
                       "Failed to translate type of the index dimension"
                       & Current_Index'Image
                       & ": "
                       & Index_Trans.Diagnostics;
                     goto Failed_UC_Translation;
                  end if;
                  Res_Typ.Index_Types (Current_Index) := Index_Trans.Res;
               end;

               if Has_Constraints then
                  --  We should only encounter either a Bin Op (A .. B) or a
                  --  range attribute reference according to RM 3.5 (2).
                  begin
                     if Kind (Range_Exp) in Ada_Bin_Op_Range then
                        if Range_Exp.As_Bin_Op.F_Left.P_Is_Static_Expr then
                           Constraint_Min :=
                             Big_Int.From_String
                               (New_Eval_As_Int (Range_Exp.As_Bin_Op.F_Left)
                                  .Image);
                           Min_Static := True;
                        else
                           Min_Static := False;
                           Min_Text := +Range_Exp.As_Bin_Op.F_Left.Text;
                        end if;
                        if Range_Exp.As_Bin_Op.F_Right.P_Is_Static_Expr then
                           Constraint_Max :=
                             Big_Int.From_String
                               (New_Eval_As_Int (Range_Exp.As_Bin_Op.F_Right)
                                  .Image);
                           Max_Static := True;
                        else
                           Max_Static := False;
                           Max_Text := +Range_Exp.As_Bin_Op.F_Right.Text;
                        end if;
                     else
                        if Range_Exp
                             .As_Attribute_Ref
                             .F_Prefix
                             .P_Name_Designated_Type
                             .P_Is_Static_Decl
                          and then not Range_Exp
                                         .As_Attribute_Ref
                                         .F_Prefix
                                         .P_Name_Designated_Type
                                         .P_Is_Enum_Type
                        then
                           Constraint_Min :=
                             Big_Int.From_String
                               (New_Eval_As_Int
                                  (Low_Bound
                                     (Range_Exp
                                        .As_Attribute_Ref
                                        .F_Prefix
                                        .P_Name_Designated_Type
                                        .P_Discrete_Range))
                                  .Image);
                           Constraint_Max :=
                             Big_Int.From_String
                               (New_Eval_As_Int
                                  (High_Bound
                                     (Range_Exp
                                        .As_Attribute_Ref
                                        .F_Prefix
                                        .P_Name_Designated_Type
                                        .P_Discrete_Range))
                                  .Image);
                           Min_Static := True;
                           Max_Static := True;
                        elsif Range_Exp
                                .As_Attribute_Ref
                                .F_Prefix
                                .P_Name_Designated_Type
                                .P_Is_Enum_Type
                        then
                           declare
                              Designed_Ty : constant Unbounded_Text_Type :=
                                (+Range_Exp.As_Attribute_Ref.F_Prefix.Text);
                              use type Ada
                                         .Strings
                                         .Wide_Wide_Unbounded
                                         .Unbounded_Wide_Wide_String;
                           begin
                              Min_Static := False;
                              Max_Static := False;

                              Min_Text :=
                                (Designed_Ty
                                 & "'Pos ("
                                 & Designed_Ty
                                 & "'First)");
                              Max_Text :=
                                (Designed_Ty
                                 & "'Pos ("
                                 & Designed_Ty
                                 & "'Last)");
                           end;
                        else
                           Min_Static := False;
                           Max_Static := False;
                           Max_Text := +Range_Exp.Text;
                        end if;
                     end if;
                  exception
                     when Non_Static_Error =>
                        Min_Static := False;
                        Max_Static := False;
                        Max_Text := +Range_Exp.Text;
                  end;
               end if;

               if not Has_Constraints then
                  --  Check if the index type is a subtype with constraints. If
                  --  this is the case, update the constraints accordingly.

                  if LAL.Kind (Constraint)
                     in LALCO.Ada_Subtype_Indication_Range
                      | LALCO.Ada_Identifier
                  then
                     declare
                        Id_Type_Res : constant Translation_Result :=
                        --  Retrieve the constraint declaration type in case
                        --  it is an identifier
                          (if Constraint.Kind = LALCO.Ada_Identifier
                           then
                             Translate
                               (Constraint
                                  .As_Identifier
                                  .P_Name_Designated_Type)
                           else
                             Translate
                               (Constraint
                                  .As_Subtype_Indication
                                  .P_Designated_Type_Decl));
                     begin
                        --  Create non-static constraints by default...

                        if Id_Type_Res.Success then
                           declare
                              FQN : constant String :=
                                To_Ada (Id_Type_Res.Res.all.Name);
                           begin
                              if Id_Type_Res.Res.all.Kind in Enum_Kind then
                                 Min_Text :=
                                   +(FQN & "'Pos (" & FQN & "'First)");
                                 Max_Text :=
                                   +(FQN & "'Pos (" & FQN & "'Last)");
                              else
                                 --  Necessarily a signed integer type, or a
                                 --  modular type.

                                 Min_Text := +(FQN & "'First");
                                 Max_Text := +(FQN & "'Last");
                              end if;
                           end;
                           Has_Constraints := True;
                           Min_Static := False;
                           Max_Static := False;

                           --  ...But attempt to evaluate the subtype bounds,
                           --  this is still useful in practice to detect
                           --  arrays that could be too large.

                           if As_Discrete_Typ (Id_Type_Res.Res).Is_Static then
                              Min_Static := True;
                              Max_Static := True;
                              Constraint_Min :=
                                As_Discrete_Typ (Id_Type_Res.Res).Low_Bound;
                              Constraint_Max :=
                                As_Discrete_Typ (Id_Type_Res.Res).High_Bound;
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               if not Has_Constraints then
                  Res_Typ.Index_Constraints (Current_Index) :=
                    new Index_Constraint'(Present => False);
               elsif Max_Static and then not Min_Static then
                  Res_Typ.Index_Constraints (Current_Index) :=
                    new Index_Constraint'
                      (Present        => True,
                       Discrete_Range =>
                         (Low_Bound  =>
                            (Kind => Non_Static, Text => +Min_Text),
                          High_Bound =>
                            (Kind => Static, Int_Val => Constraint_Min)));
               elsif Min_Static and not Max_Static then
                  Res_Typ.Index_Constraints (Current_Index) :=
                    new Index_Constraint'
                      (Present        => True,
                       Discrete_Range =>
                         (High_Bound =>
                            (Kind => Non_Static, Text => +Max_Text),
                          Low_Bound  =>
                            (Kind => Static, Int_Val => Constraint_Max)));
               elsif not (Max_Static and then Min_Static) then
                  Res_Typ.Index_Constraints (Current_Index) :=
                    new Index_Constraint'
                      (Present        => True,
                       Discrete_Range =>
                         (High_Bound =>
                            (Kind => Non_Static, Text => +Max_Text),
                          Low_Bound  =>
                            (Kind => Non_Static, Text => +Min_Text)));
               else
                  Res_Typ.Index_Constraints (Current_Index) :=
                    new Index_Constraint'
                      (Present        => True,
                       Discrete_Range =>
                         (Low_Bound  =>
                            (Kind => Static, Int_Val => Constraint_Min),
                          High_Bound =>
                            (Kind => Static, Int_Val => Constraint_Max)));
               end if;
               Current_Index := Current_Index + 1;
            end loop;

            --  For constrained arrays, even if some index type is not
            --  statically known, as long as the matching index constraints
            --  are we should be able to generate values for this type.

            Res_Typ.all.Static_Gen :=
              Res_Typ.all.Component_Type.all.Supports_Static_Gen
              and then (for all Idx in 1 .. Res_Typ.all.Num_Dims =>
                          Static (Res_Typ.all.Index_Constraints (Idx).all));

            --  Check if the translated array type has less elements than what
            --  is allowed.

            Total_Size := Res_Typ.all.Size;
            if Total_Size
              > To_Big_Integer (TGen.Marshalling.Get_Array_Size_Limit)
            then
               return Res : Translation_Result (Success => True) do
                  Res.Res :=
                    new Unsupported_Typ'
                      (Reason =>
                         +("array type "
                           & To_Ada (Res_Typ.Name)
                           & "has more elements ("
                           & Trim (To_String (Total_Size))
                           & ") than the configured limit ("
                           & Trim
                               (Positive'Image
                                  (TGen.Marshalling.Get_Array_Size_Limit))
                           & ")"),
                       others => <>);
               end return;
            end if;

            return Res : Translation_Result (Success => True) do
               Res.Res := Typ_Access (Res_Typ);
            end return;

            <<Failed_UC_Translation>>
            return (Success => False, Diagnostics => Failure_Reason);
         end;
      end Translate_Constrained;

      -----------------------------
      -- Translate_Unconstrained --
      -----------------------------

      function Translate_Unconstrained
        (Def : Array_Type_Def) return Translation_Result
      is
         type Unconstrained_Array_Typ_Access is access Unconstrained_Array_Typ;

         Indices_List : constant Unconstrained_Array_Index_List :=
           Def.F_Indices.As_Unconstrained_Array_Indices.F_Types;
         Num_Indices  : constant Positive := Indices_List.Last_Child_Index;

         Failure_Reason : Unbounded_String;

         Element_Type : constant Translation_Result :=
           Translate (Def.F_Component_Type.F_Type_Expr, Verbose_Diag);
         --  This ignores any constraints on the element type that may appear
         --  in the component definition.

         Current_Index_Type : Positive := 1;

         Res_Typ : constant Unconstrained_Array_Typ_Access :=
           new Unconstrained_Array_Typ'(Num_Dims => Num_Indices, others => <>);

      begin
         if not Element_Type.Success then
            return
              (Success     => False,
               Diagnostics =>
                 "Could not translate element type for array: "
                 & Element_Type.Diagnostics);
         end if;
         Res_Typ.Component_Type := Element_Type.Res;
         for Index of Indices_List loop
            declare
               Index_Type : constant Translation_Result :=
                 Translate
                   (Index.F_Subtype_Name.As_Name.P_Name_Designated_Type,
                    Verbose_Diag);
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
         Res_Typ.Static_Gen :=
           Res_Typ.Component_Type.all.Supports_Static_Gen
           and then (for all Index_Ref of Res_Typ.Index_Types =>
                       Index_Ref.all.Supports_Static_Gen);

         return Res : Translation_Result (Success => True) do
            Res.Res := Typ_Access (Res_Typ);
         end return;

         <<Failed_Translation>>
         return
           (Success     => False,
            Diagnostics =>
              "Failed to translate the type of the"
              & Current_Index_Type'Image
              & "index dimension"
              & ": "
              & Failure_Reason);

      end Translate_Unconstrained;

      --  Start of processing for Translate_Array_Decl

   begin
      case Kind (Decl) is
         when Ada_Subtype_Decl_Range =>
            if Is_Null (Decl.As_Subtype_Decl.F_Subtype.F_Constraint) then
               return
                 Translate_Array_Decl
                   (Decl.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl);
            else
               return Translate_Constrained (Decl);
            end if;

         when Ada_Type_Decl =>

            if Kind (Decl.As_Type_Decl.F_Type_Def)
               in Ada_Derived_Type_Def_Range
            then
               if Is_Null
                    (Decl
                       .As_Type_Decl
                       .F_Type_Def
                       .As_Derived_Type_Def
                       .F_Subtype_Indication
                       .F_Constraint)
               then
                  return
                    Translate_Array_Decl
                      (Decl
                         .As_Type_Decl
                         .F_Type_Def
                         .As_Derived_Type_Def
                         .F_Subtype_Indication
                         .P_Designated_Type_Decl);
               else
                  return Translate_Constrained (Decl);
               end if;
            else
               case Kind
                      (Decl
                         .As_Type_Decl
                         .F_Type_Def
                         .As_Array_Type_Def
                         .F_Indices)
               is
                  when Ada_Constrained_Array_Indices_Range =>
                     return Translate_Constrained (Decl);

                  when Ada_Unconstrained_Array_Indices_Range =>
                     return
                       Translate_Unconstrained
                         (Decl.As_Type_Decl.F_Type_Def.As_Array_Type_Def);

                  when others =>
                     return
                       (Success     => False,
                        Diagnostics =>
                          To_Unbounded_String
                            ("Unexpected array indices for array type def:")
                          & Kind_Name
                              (Decl
                                 .As_Type_Decl
                                 .F_Type_Def
                                 .As_Array_Type_Def
                                 .F_Indices));
               end case;
            end if;

         when others =>
            return
              (Success     => False,
               Diagnostics =>
                 To_Unbounded_String
                   ("Unexpected base type decl kind for an array:")
                 & Kind_Name (Decl));
      end case;
   end Translate_Array_Decl;

   ------------------------
   -- Record_Constrained --
   ------------------------

   function Record_Constrained
     (Decl : Base_Type_Decl; Root : Base_Type_Decl) return Boolean
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
            pragma
              Assert
                (Kind (Decl.As_Type_Decl.F_Type_Def)
                 in Ada_Derived_Type_Def_Range);
            Ancestor_Type :=
              Decl
                .As_Type_Decl
                .F_Type_Def
                .As_Derived_Type_Def
                .F_Subtype_Indication;

         when others =>
            return False;
         --  we should not be able to end up in here, but if we do,
         --  simply ignore the constraints.
      end case;

      if Is_Null (Ancestor_Type.F_Constraint) then
         return
           Record_Constrained (Ancestor_Type.P_Designated_Type_Decl, Root);
      else
         pragma
           Assert
             (Kind (Ancestor_Type.F_Constraint)
              in Ada_Composite_Constraint_Range
                and Ancestor_Type
                      .F_Constraint
                      .As_Composite_Constraint
                      .P_Is_Discriminant_Constraint);
         return True;
      end if;
   end Record_Constrained;

   -------------------------------
   -- Apply_Record_Subtype_Decl --
   -------------------------------

   procedure Apply_Record_Subtype_Decl
     (Decl : Subtype_Indication; Res : in out Discriminated_Record_Typ) is
   begin
      if Is_Null (Decl.F_Constraint) then
         return;
      end if;

      declare
         Const : TGen.Types.Constraints.Discriminant_Constraints :=
           Translate_Discriminant_Constraints
             (Decl.F_Constraint.As_Composite_Constraint);
      begin
         Res.Discriminant_Constraint.Move (Const.Constraint_Map);
      end;
   end Apply_Record_Subtype_Decl;

   -------------------------
   -- Filter_Variant_Part --
   -------------------------

   procedure Filter_Variant_Part
     (Variant       : in out Variant_Part_Acc;
      TL_Components : in out Component_Maps.Map;
      Constraints   : Discriminant_Constraint_Maps.Map;
      Renaming      : Discriminant_Constraint_Maps.Map)
   is
      use Variant_Choice_Lists;

      Choice_Cur : Cursor := Variant.Variant_Choices.First;

      procedure Filter_Variant_Choice (Var_Choice : in out Variant_Choice);

      procedure Delete_Nested_Variant (Var_Choice : in out Variant_Choice);

      ---------------------------
      -- Filter_Variant_Choice --
      ---------------------------

      procedure Filter_Variant_Choice (Var_Choice : in out Variant_Choice) is
      begin
         if Var_Choice.Variant /= null then
            Filter_Variant_Part
              (Var_Choice.Variant,
               Var_Choice.Components,
               Constraints,
               Renaming);
         end if;
      end Filter_Variant_Choice;

      ---------------------------
      -- Delete_Nested_Variant --
      ---------------------------

      procedure Delete_Nested_Variant (Var_Choice : in out Variant_Choice) is
      begin
         if Var_Choice.Variant /= null then
            Free_Variant (Var_Choice.Variant);
         end if;
      end Delete_Nested_Variant;

      Needs_Renaming : constant Boolean :=
        Renaming.Contains (Variant.Discr_Name);

      --  Start of processing for Filter_Variant_Part

   begin
      --  Rename the discriminant name associated with this variant part if it
      --  is in the renaming map.

      if Needs_Renaming then
         Variant.Discr_Name := Renaming.Element (Variant.Discr_Name).Disc_Name;
      end if;

      if Needs_Renaming
        or else not Constraints.Contains (Variant.Discr_Name)
        or else not (Constraints.Element (Variant.Discr_Name).Kind in Static)
      then

         --  If the discriminant name associated with this variant part is
         --  not in the constraint map, or is in the constraint map but the
         --  constraint is not static, simply update the eventual nested
         --  variant parts.

         while Has_Element (Choice_Cur) loop
            Variant.Variant_Choices.Update_Element
              (Choice_Cur, Filter_Variant_Choice'Access);
            Next (Choice_Cur);
         end loop;
      else

         --  Otherwise, check for each choice if it matches the constraint.
         --  If it does (there should only be one possible match), update
         --  the possibly nested variant parts, otherwise, free the possibly
         --  nested variant as the choice will be deleted.
         --  Once this is done, merge the components in the only remaining
         --  Choice to the top level components, and the the variant access
         --  to point to the nested variant part of the choice, if it exists.

         declare
            Discr_Val   : constant Big_Integer :=
              Constraints.Element (Variant.Discr_Name).Int_Val;
            Match_Cur   : Cursor := No_Element;
            Old_Variant : Variant_Part_Acc := Variant;

         begin
            while Has_Element (Choice_Cur) loop
               if not Has_Element (Match_Cur) then
                  for Alt of Element (Choice_Cur).Alt_Set loop
                     if Discr_Val >= Alt.Min and then Discr_Val <= Alt.Max then
                        Match_Cur := Choice_Cur;
                        Variant.Variant_Choices.Update_Element
                          (Choice_Cur, Filter_Variant_Choice'Access);
                     end if;
                  end loop;
               end if;
               if not Has_Element (Match_Cur) then
                  Variant.Variant_Choices.Update_Element
                    (Choice_Cur, Delete_Nested_Variant'Access);
               end if;
               Next (Choice_Cur);
            end loop;
            declare
               Match_Choice : constant Variant_Choice := Element (Match_Cur);
               Comp_Cur     : Component_Maps.Cursor :=
                 Match_Choice.Components.First;

               procedure Set_Null (Var_Choice : in out Variant_Choice);

               procedure Set_Null (Var_Choice : in out Variant_Choice) is
               begin
                  Var_Choice.Variant := null;
               end Set_Null;
            begin
               while Component_Maps.Has_Element (Comp_Cur) loop
                  TL_Components.Insert
                    (Component_Maps.Key (Comp_Cur),
                     Component_Maps.Element (Comp_Cur));
                  Component_Maps.Next (Comp_Cur);
               end loop;
               if Match_Choice.Variant /= null then
                  Variant := Match_Choice.Variant;
                  Old_Variant.Variant_Choices.Update_Element
                    (Match_Cur, Set_Null'Access);
               else
                  Variant := null;
               end if;
               Free_Variant (Old_Variant);
            end;
         end;
      end if;
   end Filter_Variant_Part;

   ------------------------------------
   -- Apply_Record_Derived_Type_Decl --
   ------------------------------------

   function Apply_Record_Derived_Type_Decl
     (Decl : Type_Decl'Class; From : in out Discriminated_Record_Typ)
      return Discriminated_Record_Typ
   is

      use Discriminant_Constraint_Maps;

      Constraints_Map    : Discriminant_Constraint_Maps.Map;
      Discr_Renaming_Map : Discriminant_Constraint_Maps.Map;
      Constraint_Cur     : Cursor;
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

      if Is_Null
           (Decl
              .F_Type_Def
              .As_Derived_Type_Def
              .F_Subtype_Indication
              .F_Constraint)
      then
         return From;
      end if;

      --  Case 2 & 3:

      --  First build a discriminant constraint map to filter out
      --  the unachievable shapes.

      return New_Typ : Discriminated_Record_Typ (Constrained => True) do
         New_Typ.Mutable :=
           not Is_Null (Decl.F_Discriminants)
           and then not Is_Null
                          (Decl
                             .F_Discriminants
                             .As_Known_Discriminant_Part
                             .F_Discr_Specs
                             .First_Child
                             .As_Discriminant_Spec
                             .F_Default_Expr);

         for Pair of
           Decl
             .F_Type_Def
             .As_Derived_Type_Def
             .F_Subtype_Indication
             .F_Constraint
             .As_Composite_Constraint
             .F_Constraints
             .P_Zip_With_Params
         loop
            if Kind (Actual (Pair)) in Ada_Name
              and then not Is_Null
                             (Actual (Pair).As_Name.P_Referenced_Defining_Name)
              and then Kind
                         (Actual (Pair)
                            .As_Name
                            .P_Referenced_Defining_Name
                            .Parent
                            .Parent)
                       in Ada_Discriminant_Spec_Range
            then
               --  Case of a Discriminant correspondence

               Discr_Renaming_Map.Insert
                 (Key      => +Param (Pair).As_Defining_Name.Text,
                  New_Item =>
                    (Kind      => Discriminant,
                     Disc_Name =>
                       +Actual (Pair)
                          .As_Name
                          .P_Referenced_Defining_Name
                          .Text));
            elsif Actual (Pair).P_Is_Static_Expr then
               begin
                  --  Static value in the discriminant constraint
                  Constraints_Map.Insert
                    (Key      => +Param (Pair).As_Defining_Name.Text,
                     New_Item =>
                       (Kind    => Static,
                        Int_Val =>
                          Big_Int.From_String
                            (New_Eval_As_Int (Actual (Pair)).Image)));
               exception
                  when Non_Static_Error =>
                     Constraints_Map.Insert
                       (Key      => +Param (Pair).As_Defining_Name.Text,
                        New_Item =>
                          (Kind => Non_Static, Text => +Actual (Pair).Text));
               end;
            else
               --  Non static value

               Constraints_Map.Insert
                 (Key      => +Param (Pair).As_Defining_Name.Text,
                  New_Item =>
                    (Kind => Non_Static, Text => +Actual (Pair).Text));
            end if;
         end loop;

         --  Copy over the components that are always present

         New_Typ.Component_Types.Move (From.Component_Types);

         --  Then filter the variant part tree to remove any unreachable shape

         if From.Variant /= null then
            New_Typ.Variant := From.Variant;
            Filter_Variant_Part
              (New_Typ.Variant,
               New_Typ.Component_Types,
               Constraints_Map,
               Discr_Renaming_Map);

         end if;

         --  Fill out discriminant types

         Constraint_Cur := Discr_Renaming_Map.First;
         while Has_Element (Constraint_Cur) loop
            if Element (Constraint_Cur).Kind = Discriminant then
               New_Typ.Discriminant_Types.Insert
                 (Key      => Element (Constraint_Cur).Disc_Name,
                  New_Item =>
                    From.Discriminant_Types.Element (Key (Constraint_Cur)));
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
                 (Key      => Key (Constraint_Cur),
                  New_Item =>
                    From.Discriminant_Types.Element (Key (Constraint_Cur)));
            end if;
            Next (Constraint_Cur);
         end loop;
         New_Typ.Name := From.Name;
         New_Typ.Last_Comp_Unit_Idx := From.Last_Comp_Unit_Idx;
      end return;
   end Apply_Record_Derived_Type_Decl;

   --------------------------------
   -- Subtract_Choice_From_Other --
   --------------------------------

   procedure Subtract_Choice_From_Other
     (Others_Cur : Variant_Choice_Lists.Cursor;
      Choice     : Variant_Choice;
      List       : in out Variant_Choice_Lists.List)
   is
      use Alternatives_Sets;
      New_Set            : Alternatives_Set;
      Cur_Alt            : Cursor := Choice.Alt_Set.First;
      Cur_Others_Segment : Cursor;

      type Subtraction_Result is array (Positive range <>) of Int_Range;

      procedure Update_Set (Other_Var : in out Variant_Choice);

      procedure Get_Set (Other_Var : in out Variant_Choice);

      function Overlap (L, R : Int_Range) return Boolean;

      function "-" (L : Int_Range; R : Int_Range) return Subtraction_Result;

      -------------
      -- Get_Set --
      -------------

      procedure Get_Set (Other_Var : in out Variant_Choice) is
      begin
         New_Set.Move (Other_Var.Alt_Set);
      end Get_Set;

      ----------------
      -- Update_Set --
      ----------------

      procedure Update_Set (Other_Var : in out Variant_Choice) is
      begin
         Other_Var.Alt_Set.Move (New_Set);
      end Update_Set;

      -------------
      -- Overlap --
      -------------

      function Overlap (L, R : Int_Range) return Boolean is
      begin
         return R.Min <= L.Max and then L.Min <= R.Max;
      end Overlap;

      ---------
      -- "-" --
      ---------

      function "-" (L : Int_Range; R : Int_Range) return Subtraction_Result is
         One : constant Big_Integer := To_Big_Integer (1);
      begin
         if not Overlap (L, R) then
            return [1 => L];
         elsif R.Min <= L.Min and then L.Max <= R.Max then
            return [1 .. 0 => <>];
         elsif L.Min < R.Min and then R.Min <= L.Max and then L.Max <= R.Max
         then
            return [1 => (Min => L.Min, Max => R.Min - One)];
         elsif R.Min <= L.Min and then L.Min <= R.Max and then R.Max < L.Max
         then
            return [1 => (Min => R.Max + One, Max => L.Max)];
         else
            return
              [1 => (Min => L.Min, Max => R.Min - One),
               2 => (Min => R.Max + One, Max => L.Max)];
         end if;
      end "-";
   begin
      --  Get the Set so it is easier to modify
      List.Update_Element (Others_Cur, Get_Set'Access);

      Cur_Others_Segment := New_Set.First;

      while Has_Element (Cur_Alt) loop

         --  Move the cursor in the "others" set until we have an intersection
         --  or we are past the current alternative range

         while Has_Element (Cur_Others_Segment)
           and then not Overlap
                          (Element (Cur_Others_Segment), Element (Cur_Alt))
           and then Element (Cur_Others_Segment) < Element (Cur_Alt)
         loop
            Next (Cur_Others_Segment);
         end loop;

         exit when not Has_Element (Cur_Others_Segment);
         declare
            Sub_Res : constant Subtraction_Result :=
              Element (Cur_Others_Segment) - Element (Cur_Alt);
            --  Compute difference

            Delete_Cur : Cursor := Cur_Others_Segment;
            --  Save current position to delete the current range if needed

            New_Elt_Cur : Cursor;
            Inserted    : Boolean;
         begin
            if Sub_Res'Length /= 1
              or else Sub_Res (1) /= Element (Cur_Others_Segment)
            then

               --  Here if there is an intersection between the current others
               --  range and the current alternative range. Prefetch the next
               --  others range and delete the current others range.

               Next (Cur_Others_Segment);
               New_Set.Delete (Delete_Cur);

               if Sub_Res'Length = 1 then

                  --  Single element from the difference, it is either before
                  --  the current alternative range or after.
                  --  If it is after however, it will be before what we
                  --  currently have in Cur_Others_Segment, so it needs to be
                  --  the next range to to be checked against the next
                  --  alternative range.

                  New_Set.Insert (Sub_Res (1), New_Elt_Cur, Inserted);
                  if Element (Cur_Alt) < Element (New_Elt_Cur) then
                     Cur_Others_Segment := New_Elt_Cur;
                  end if;
               elsif Sub_Res'Length = 2 then
                  --  If there are two ranges resulting from the difference,
                  --  then we have both cases described above, and we already
                  --  know that the next element that needs to be processed is
                  --  Sub_Res (2), so update Cur_Others_Segment to point to it.

                  New_Set.Insert (Sub_Res (1));
                  New_Set.Insert (Sub_Res (2), Cur_Others_Segment, Inserted);
               end if;
            end if;
         end;
         Next (Cur_Alt);
      end loop;

      --  Store back the List in the variant_choice record.

      List.Update_Element (Others_Cur, Update_Set'Access);

   end Subtract_Choice_From_Other;

   -----------------------------------
   -- Translate_Component_Decl_List --
   -----------------------------------

   function Translate_Component_Decl_List
     (Decl_List : Ada_Node_List; Res : in out Component_Maps.Map)
      return Unbounded_String
   is
      Current_Typ : Translation_Result;
      Comp_Decl   : Component_Decl;
   begin
      for Decl of Decl_List loop
         if Kind (Decl) in Ada_Null_Component_Decl then
            return Null_Unbounded_String;
         end if;
         Comp_Decl := Decl.As_Component_Decl;
         Current_Typ :=
           Translate (Comp_Decl.F_Component_Def.F_Type_Expr, Verbose_Diag);
         if not Current_Typ.Success then
            return
              "Failed to translate type of component"
              & Comp_Decl.Image
              & ": "
              & Current_Typ.Diagnostics;
         end if;
         for Id of Comp_Decl.F_Ids loop
            Res.Insert
              (Key => +Id.As_Defining_Name.Text, New_Item => Current_Typ.Res);
         end loop;
      end loop;
      return Null_Unbounded_String;
   end Translate_Component_Decl_List;

   function Translate_Variant_Part
     (Node : LAL.Variant_Part; Discriminants : Component_Maps.Map)
      return Record_Types.Variant_Part
   is
      use Variant_Choice_Lists;
      Res        : Record_Types.Variant_Part;
      Choice_Min : Big_Int.Big_Integer;
      Choice_Max : Big_Int.Big_Integer;
      Has_Others : Boolean := False;
   begin
      Res.Discr_Name := +Node.F_Discr_Name.P_Referenced_Defining_Name.Text;

      for Var_Choice of Node.F_Variant loop
         declare
            Choice_Trans : Variant_Choice;

            Has_Variant : constant Boolean :=
              not Var_Choice.As_Variant.F_Components.F_Variant_Part.Is_Null;

            Diagnostics : constant Unbounded_String :=
              Translate_Component_Decl_List
                (Var_Choice.As_Variant.F_Components.F_Components,
                 Choice_Trans.Components);
         begin
            if Diagnostics /= Null_Unbounded_String then
               raise Translation_Error
                 with
                   "error while translating Variant part: "
                   & To_String (Diagnostics);
            end if;
            for Alt of Var_Choice.F_Choices loop
               case Alt.Kind is
                  when Ada_Expr =>
                     if Alt.Kind in Ada_Bin_Op then
                        if Alt.As_Bin_Op.F_Op.Kind in Ada_Op_Double_Dot then
                           Choice_Min :=
                             Big_Int.From_String
                               (New_Eval_As_Int (Alt.As_Bin_Op.F_Left).Image);
                           Choice_Max :=
                             Big_Int.From_String
                               (New_Eval_As_Int (Alt.As_Bin_Op.F_Right).Image);
                           Choice_Trans.Alt_Set.Insert
                             ((Min => Choice_Min, Max => Choice_Max));
                        else
                           Choice_Min :=
                             Big_Int.From_String
                               (New_Eval_As_Int (Alt.As_Expr).Image);
                           Choice_Trans.Alt_Set.Insert
                             ((Min => Choice_Min, Max => Choice_Min));
                        end if;
                     elsif Alt.Kind in Ada_Name
                       and then not Is_Null
                                      (Alt.As_Name.P_Name_Designated_Type)
                     then
                        Choice_Min :=
                          Big_Int.From_String
                            (New_Eval_As_Int
                               (Low_Bound
                                  (Alt
                                     .As_Name
                                     .P_Name_Designated_Type
                                     .P_Discrete_Range))
                               .Image);
                        Choice_Max :=
                          Big_Int.From_String
                            (New_Eval_As_Int
                               (High_Bound
                                  (Alt
                                     .As_Name
                                     .P_Name_Designated_Type
                                     .P_Discrete_Range))
                               .Image);
                        Choice_Trans.Alt_Set.Insert
                          ((Min => Choice_Min, Max => Choice_Max));
                     else
                        Choice_Min :=
                          Big_Int.From_String
                            (New_Eval_As_Int (Alt.As_Expr).Image);
                        Choice_Trans.Alt_Set.Insert
                          ((Min => Choice_Min, Max => Choice_Min));
                     end if;

                  when Ada_Others_Designator_Range =>
                     Choice_Trans.Alt_Set.Clear;
                     Has_Others := True;
                     if not Component_Maps.Has_Element
                              (Discriminants.Find (Res.Discr_Name))
                     then
                        raise Translation_Error
                          with
                            "Unknown discriminant name "
                            & To_String (Res.Discr_Name);
                     end if;

                     --  This is not really accurate for enum types if the
                     --  various enum literal positions are not contiguous.

                     Choice_Trans.Alt_Set.Insert
                       ((Min =>
                           As_Discrete_Typ
                             (Discriminants.Element (Res.Discr_Name))
                             .Low_Bound,
                         Max =>
                           As_Discrete_Typ
                             (Discriminants.Element (Res.Discr_Name))
                             .High_Bound));

                  when others =>
                     raise Translation_Error
                       with
                         "Unexpected node kind for a variant choice"
                         & Alt.Image;
               end case;
               exit when Alt.Kind in Ada_Others_Designator_Range;
            end loop;
            if Has_Variant then
               Choice_Trans.Variant :=
                 new Record_Types.Variant_Part'
                   (Translate_Variant_Part
                      (Var_Choice.As_Variant.F_Components.F_Variant_Part,
                       Discriminants));
            end if;
            Res.Variant_Choices.Append (Choice_Trans);
         end;
      end loop;

      if Has_Others then
         for Choice_Cur in Res.Variant_Choices.Iterate loop
            exit when Choice_Cur = Res.Variant_Choices.Last;
            Subtract_Choice_From_Other
              (Res.Variant_Choices.Last,
               Element (Choice_Cur),
               Res.Variant_Choices);
         end loop;
      end if;

      return Res;
   end Translate_Variant_Part;

   ---------------------------
   -- Translate_Record_Decl --
   ---------------------------

   function Translate_Record_Decl
     (Decl : Base_Type_Decl) return Translation_Result
   is

      procedure Apply_Constraints
        (Decl, Root : Base_Type_Decl; Res : in out Discriminated_Record_Typ);
      --  Modify Res to include all the discriminant constraints present in
      --  the type derivation / subtype decl chain.

      -----------------------
      -- Apply_Constraints --
      -----------------------

      procedure Apply_Constraints
        (Decl, Root : Base_Type_Decl; Res : in out Discriminated_Record_Typ) is
      begin
         --  The original Decl of a record is not constrained.

         if Decl = Root then
            return;
         end if;

         case Kind (Decl) is
            when Ada_Type_Decl =>

               --  First apply constraints of the ancestor type

               Apply_Constraints
                 (Decl
                    .As_Type_Decl
                    .F_Type_Def
                    .As_Derived_Type_Def
                    .F_Subtype_Indication
                    .F_Name
                    .P_Name_Designated_Type,
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
               --  This should not be reachable
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

      if Kind (Decl.P_Root_Type.P_Full_View) in Ada_Type_Decl
        and then Kind (Decl.P_Root_Type.P_Full_View.As_Type_Decl.F_Type_Def)
                 in Ada_Record_Type_Def_Range
        and then Is_Null
                   (Decl.P_Root_Type.P_Full_View.As_Type_Decl.F_Discriminants)
      then
         Actual_Decl := Decl.P_Root_Type.P_Full_View.As_Type_Decl;

         declare
            Trans_Res : constant Nondiscriminated_Record_Typ_Access :=
              new Nondiscriminated_Record_Typ'(others => <>);
            Comp_List : constant Ada_Node_List :=
              Actual_Decl
                .F_Type_Def
                .As_Record_Type_Def
                .F_Record_Def
                .F_Components
                .F_Components;
         begin

            Failure_Reason :=
              Translate_Component_Decl_List
                (Comp_List, Trans_Res.all.Component_Types);

            if Failure_Reason = Null_Unbounded_String then
               Trans_Res.all.Static_Gen :=
                 (for all Comp_Ref of Trans_Res.all.Component_Types =>
                    Comp_Ref.all.Supports_Static_Gen);

               return Res : Translation_Result (Success => True) do
                  Res.Res := Typ_Access (Trans_Res);
               end return;

            else
               return (Success => False, Diagnostics => Failure_Reason);
            end if;
         end;

      else
         --  Now the rest

         Actual_Decl := Decl.P_Root_Type.P_Full_View.As_Type_Decl;

         declare
            Trans_Res : constant Discriminated_Record_Typ_Access :=
              new Discriminated_Record_Typ
                    (Constrained =>
                       Record_Constrained
                         (Decl, Actual_Decl.As_Base_Type_Decl));

            Discriminant_List : constant Discriminant_Spec_List :=
              Actual_Decl
                .F_Discriminants
                .As_Known_Discriminant_Part
                .F_Discr_Specs;
            --  ??? We assume that we only have known discriminants for the
            --  moment as we are supposed to be translating the full view of
            --  the type, will need to revisit this to double check.

            Current_Type : Translation_Result;
            Comp_Decl    : constant Component_List :=
              Actual_Decl
                .F_Type_Def
                .As_Record_Type_Def
                .F_Record_Def
                .F_Components;
         begin

            --  First translate the list of discriminants

            for Spec of Discriminant_List loop
               if not Is_Null (Spec.F_Default_Expr) then
                  Trans_Res.all.Mutable := True;
               end if;
               Current_Type := Translate (Spec.F_Type_Expr, Verbose_Diag);
               if not Current_Type.Success then
                  Failure_Reason :=
                    "Failed to translate discriminant spec "
                    & Spec.Image
                    & ": "
                    & Current_Type.Diagnostics;
                  goto Failed_Discr_Rec_Translation;
               end if;
               for Def_Name of Spec.F_Ids loop
                  Trans_Res.all.Discriminant_Types.Insert
                    (Key      => +Def_Name.As_Defining_Name.Text,
                     New_Item => Current_Type.Res);
               end loop;
            end loop;

            --  Then the components always present

            Failure_Reason :=
              Translate_Component_Decl_List
                (Comp_Decl.F_Components, Trans_Res.all.Component_Types);

            if Failure_Reason /= Null_Unbounded_String then
               return (Success => False, Diagnostics => Failure_Reason);
            end if;

            --  And then the variant part if any

            if not Comp_Decl.F_Variant_Part.Is_Null then
               Trans_Res.all.Variant :=
                 new Record_Types.Variant_Part'
                   (Translate_Variant_Part
                      (Comp_Decl.F_Variant_Part,
                       Trans_Res.all.Discriminant_Types));
            end if;

            --  If the record is actually a constrained type, record the
            --  constraints now.

            if Trans_Res.all.Constrained then
               Apply_Constraints
                 (Decl, Actual_Decl.As_Base_Type_Decl, Trans_Res.all);
            end if;

            Trans_Res.all.Static_Gen :=
              (for all Comp_Ref of Trans_Res.all.Component_Types =>
                 Comp_Ref.all.Supports_Static_Gen)
              and then (for all Disc_Ref of Trans_Res.all.Discriminant_Types =>
                          Disc_Ref.all.Supports_Static_Gen)
              and then (not Trans_Res.all.Constrained
                        or else (for all Const of
                                   Trans_Res.all.Discriminant_Constraint =>
                                   Const.Kind in Static | Discriminant))
              and then Variant_Support_Static_Gen (Trans_Res.all.Variant);

            --  Apply_Constraints can actually return a type that isn't
            --  discriminated or that isn't constrained, so lets try to
            --  convert Trans_Res to the correct kind depending on the
            --  attributes.

            if Trans_Res.all.Constrained
              and then Trans_Res.all.Discriminant_Constraint.Is_Empty
            then
               if Trans_Res.all.Discriminant_Types.Is_Empty then

                  --  Normally only checking for the discriminant is sufficient
                  --  to check if Trans_Res will is actually a non
                  --  discriminated type, but we may have some lingering non
                  --  static constraints that don't allow us to determine
                  --  what the final list of components is.

                  if Trans_Res.all.Variant /= null then
                     Free_Variant (Trans_Res.all.Variant);
                  end if;

                  return Res : Translation_Result (Success => True) do
                     Res.Res :=
                       new Nondiscriminated_Record_Typ'
                         (Component_Types => Trans_Res.all.Component_Types,
                          Static_Gen      => Trans_Res.all.Static_Gen,
                          others          => <>);
                  end return;

               else
                  return Res : Translation_Result (Success => True) do
                     declare
                        Rec_Typ : constant Discriminated_Record_Typ_Access :=
                          new Discriminated_Record_Typ'
                            (Constrained => False, others => <>);
                     begin
                        Rec_Typ.all.Component_Types.Move
                          (Trans_Res.all.Component_Types);
                        Rec_Typ.all.Discriminant_Types.Move
                          (Trans_Res.Discriminant_Types);
                        Rec_Typ.all.Variant := Trans_Res.all.Variant;
                        Rec_Typ.all.Mutable := Trans_Res.all.Mutable;
                        Rec_Typ.all.Static_Gen := Trans_Res.all.Static_Gen;
                        Res.Res := Typ_Access (Rec_Typ);
                     end;
                  end return;
               end if;
            end if;

            return Res : Translation_Result (Success => True) do
               Res.Res := Typ_Access (Trans_Res);
            end return;

            <<Failed_Discr_Rec_Translation>>
            if Trans_Res.Variant /= null then
               Free_Variant (Trans_Res.Variant);
            end if;
            return (Success => False, Diagnostics => Failure_Reason);
         end;
      end if;
   exception
      when Exc : Translation_Error =>
         return
           (Success     => False,
            Diagnostics =>
              To_Unbounded_String (Ada.Exceptions.Exception_Message (Exc)));
   end Translate_Record_Decl;

   -------------------------
   -- Eval_Discrete_Range --
   -------------------------

   function Eval_Discrete_Range
     (Rng : Discrete_Range)
      return TGen.Types.Constraints.Discrete_Range_Constraint
   is
      Low_Bnd  : Discrete_Constraint_Value;
      High_Bnd : Discrete_Constraint_Value;
   begin
      begin
         if Low_Bound (Rng).P_Is_Static_Expr then
            Low_Bnd :=
              (Kind    => Static,
               Int_Val =>
                 Big_Int.From_String
                   (New_Eval_As_Int (Low_Bound (Rng)).Image));
         elsif Low_Bound (Rng).Kind in Ada_Name
           and then not Is_Null
                          (Low_Bound (Rng).As_Name.P_Referenced_Defining_Name)
           and then Kind
                      (Low_Bound (Rng)
                         .As_Name
                         .P_Referenced_Defining_Name
                         .Parent
                         .Parent)
                    in Ada_Discriminant_Spec_Range
         then
            Low_Bnd :=
              (Kind      => Discriminant,
               Disc_Name =>
                 +Low_Bound (Rng).As_Name.P_Referenced_Defining_Name.Text);
         else
            Low_Bnd := (Kind => Non_Static, Text => +Low_Bound (Rng).Text);
         end if;
      exception
         when Non_Static_Error =>
            Low_Bnd := (Kind => Non_Static, Text => +Low_Bound (Rng).Text);
      end;

      begin
         if High_Bound (Rng).P_Is_Static_Expr then
            High_Bnd :=
              (Kind    => Static,
               Int_Val =>
                 Big_Int.From_String
                   (New_Eval_As_Int (High_Bound (Rng)).Image));
         elsif High_Bound (Rng).Kind in Ada_Name
           and then not Is_Null
                          (High_Bound (Rng).As_Name.P_Referenced_Defining_Name)
           and then Kind
                      (High_Bound (Rng)
                         .As_Name
                         .P_Referenced_Defining_Name
                         .Parent
                         .Parent)
                    in Ada_Discriminant_Spec_Range
         then
            High_Bnd :=
              (Kind      => Discriminant,
               Disc_Name =>
                 +High_Bound (Rng).As_Name.P_Referenced_Defining_Name.Text);
         else
            High_Bnd := (Kind => Non_Static, Text => +High_Bound (Rng).Text);
         end if;
      exception
         when Non_Static_Error =>
            High_Bnd := (Kind => Non_Static, Text => +High_Bound (Rng).Text);
      end;

      return (Low_Bnd, High_Bnd);
   end Eval_Discrete_Range;

   -----------------------------------------
   -- Translate_Discrete_Range_Constraint --
   -----------------------------------------

   function Translate_Discrete_Range_Constraint
     (Node : LAL.Range_Constraint) return Discrete_Range_Constraint
   is
      Min, Max : Expr;
   begin
      case Kind (Node.F_Range.F_Range) is
         when Ada_Attribute_Ref_Range =>
            pragma
              Assert
                (Node
                   .F_Range
                   .F_Range
                   .As_Attribute_Ref
                   .F_Prefix
                   .P_Name_Designated_Type
                   .P_Is_Discrete_Type);
            Min :=
              Low_Bound
                (Node
                   .F_Range
                   .F_Range
                   .As_Attribute_Ref
                   .F_Prefix
                   .P_Name_Designated_Type
                   .P_Discrete_Range)
                .As_Expr;
            Max :=
              High_Bound
                (Node
                   .F_Range
                   .F_Range
                   .As_Attribute_Ref
                   .F_Prefix
                   .P_Name_Designated_Type
                   .P_Discrete_Range)
                .As_Expr;

         when Ada_Bin_Op_Range =>
            pragma
              Assert
                (Node.F_Range.F_Range.As_Bin_Op.F_Op
                 in Ada_Op_Double_Dot_Range);
            Min := Node.F_Range.F_Range.As_Bin_Op.F_Left;
            Max := Node.F_Range.F_Range.As_Bin_Op.F_Right;

         when others =>
            raise Translation_Error
              with
                "Unexpected expression for a range constraint: "
                & Kind_Name (Node.F_Range.F_Range);
      end case;

      return
        Eval_Discrete_Range
          (Create_Discrete_Range (No_Base_Type_Decl, Min, Max));

   end Translate_Discrete_Range_Constraint;

   function Translate_Real_Constraints
     (Node : LAL.Constraint) return TGen.Types.Constraints.Constraint'Class
   is
      Range_Spc : constant LAL.Range_Spec := Extract_Real_Range_Spec (Node);
      Rnge      : Real_Range_Constraint;
   begin

      if not Is_Null (Range_Spc) then
         Rnge := Translate_Real_Range_Spec (Range_Spc);
      end if;

      case Kind (Node) is
         when Ada_Range_Constraint_Range =>
            pragma Assert (not Is_Null (Range_Spc));
            return Rnge;

         when Ada_Digits_Constraint_Range =>
            if Node.As_Digits_Constraint.F_Digits.P_Is_Static_Expr then
               begin
                  declare
                     Digits_Val : constant Big_Int.Big_Integer :=
                       Big_Int.From_String
                         (New_Eval_As_Int (Node.As_Digits_Constraint.F_Digits)
                            .Image);
                  begin
                     if not Is_Null (Range_Spc) then
                        return
                          TGen.Types.Constraints.Digits_Constraint'
                            (Has_Range    => True,
                             Digits_Value =>
                               (Kind => Static, Int_Val => Digits_Val),
                             Range_Value  => Rnge);
                     else
                        return
                          TGen.Types.Constraints.Digits_Constraint'
                            (Has_Range    => False,
                             Digits_Value =>
                               (Kind => Static, Int_Val => Digits_Val));
                     end if;
                  end;
               exception
                  when Non_Static_Error =>
                     if not Is_Null (Range_Spc) then
                        return
                          TGen.Types.Constraints.Digits_Constraint'
                            (Has_Range    => True,
                             Digits_Value =>
                               (Kind => Non_Static,
                                Text =>
                                  +Node.As_Digits_Constraint.F_Digits.Text),
                             Range_Value  => Rnge);
                     else
                        return
                          TGen.Types.Constraints.Digits_Constraint'
                            (Has_Range    => False,
                             Digits_Value =>
                               (Kind => Non_Static,
                                Text =>
                                  +Node.As_Digits_Constraint.F_Digits.Text));
                     end if;
               end;
            end if;

            --  Case of a non static digit value. This is not possible
            --  according to RM 3.5.9 (5/4).

            raise Translation_Error
              with "Non static digits constraints are forbidden:" & Node.Image;

         when Ada_Delta_Constraint_Range =>
            raise Translation_Error
              with "Delta constraints for anonymous types not implemented yet";

         when others =>
            raise Translation_Error
              with
                "Unexpected expression for a real type constraint: "
                & Kind_Name (Node);
      end case;
   end Translate_Real_Constraints;

   function Translate_Index_Constraints
     (Node : LAL.Constraint; Num_Dims : Positive)
      return TGen.Types.Constraints.Index_Constraints
   is
      Constraint_List : constant Local_Ada_Node_Arr :=
        Gather_Index_Constraint_Nodes (Node, Num_Dims);
      Current_Index   : Positive := 1;

      Discr_Range     : Discrete_Range;
      Referenced_Type : Base_Type_Decl;
   begin
      return Res : Index_Constraints (Num_Dims) do
         for Cst of Constraint_List loop
            case Kind (Cst) is
               when Ada_Subtype_Indication_Range =>
                  if not Is_Null (Cst.As_Subtype_Indication.F_Constraint) then
                     Res.Constraint_Array (Current_Index) :=
                       new TGen.Types.Constraints.Index_Constraint'
                         (Present        => True,
                          Discrete_Range =>
                            Translate_Discrete_Range_Constraint
                              (Cst
                                 .As_Subtype_Indication
                                 .F_Constraint
                                 .As_Range_Constraint));
                     goto Skip_Range_Translation;
                  else
                     Referenced_Type :=
                       Cst.As_Subtype_Indication.F_Name.P_Name_Designated_Type;
                     Discr_Range :=
                       Cst
                         .As_Subtype_Indication
                         .F_Name
                         .P_Name_Designated_Type
                         .P_Discrete_Range;
                  end if;

               when Ada_Bin_Op_Range =>
                  pragma
                    Assert
                      (Kind (Cst.As_Bin_Op.F_Op) in Ada_Op_Double_Dot_Range);
                  Discr_Range :=
                    Create_Discrete_Range
                      (No_Base_Type_Decl,
                       Cst.As_Bin_Op.F_Left,
                       Cst.As_Bin_Op.F_Right);

               when Ada_Attribute_Ref_Range =>
                  Discr_Range :=
                    Cst
                      .As_Attribute_Ref
                      .F_Prefix
                      .P_Name_Designated_Type
                      .P_Discrete_Range;
                  Referenced_Type :=
                    Cst.As_Attribute_Ref.F_Prefix.P_Name_Designated_Type;

               when others =>
                  Discr_Range :=
                    Cst.As_Name.P_Name_Designated_Type.P_Discrete_Range;
                  Referenced_Type := Cst.As_Name.P_Name_Designated_Type;
            end case;

            if not (Is_Null (Referenced_Type)
                    or else Referenced_Type.P_Is_Static_Decl)
            then
               Res.Constraint_Array (Current_Index) :=
                 new TGen.Types.Constraints.Index_Constraint'
                   (Present        => True,
                    Discrete_Range =>
                      (Low_Bound  => (Kind => Non_Static, others => <>),
                       High_Bound => (Kind => Non_Static, Text => +Cst.Text)));
               goto Skip_Range_Translation;
            end if;

            Res.Constraint_Array (Current_Index) :=
              new TGen.Types.Constraints.Index_Constraint'
                (Present        => True,
                 Discrete_Range => Eval_Discrete_Range (Discr_Range));

            <<Skip_Range_Translation>>
            Current_Index := Current_Index + 1;
         end loop;
      end return;
   end Translate_Index_Constraints;

   function Translate_Discriminant_Constraints
     (Node : LAL.Composite_Constraint)
      return TGen.Types.Constraints.Discriminant_Constraints
   is
      New_Item : Discrete_Constraint_Value;
   begin
      return Res : TGen.Types.Constraints.Discriminant_Constraints do
         for Pair of Node.F_Constraints.P_Zip_With_Params loop
            New_Item := (Kind => Non_Static, others => <>);
            begin
               if Actual (Pair).P_Is_Static_Expr then
                  New_Item :=
                    (Kind    => Static,
                     Int_Val =>
                       Big_Int.From_String
                         (New_Eval_As_Int (Actual (Pair)).Image));
               elsif Kind (Actual (Pair)) in Ada_Name
                 and then not Is_Null
                                (Actual (Pair)
                                   .As_Name
                                   .P_Referenced_Defining_Name)
                 and then Kind
                            (Actual (Pair)
                               .As_Name
                               .P_Referenced_Defining_Name
                               .Parent
                               .Parent)
                          in Ada_Discriminant_Spec_Range
               then
                  New_Item :=
                    (Kind      => Discriminant,
                     Disc_Name =>
                       +Actual (Pair).As_Name.P_Referenced_Defining_Name.Text);
               else
                  New_Item :=
                    (Kind => Non_Static, Text => +Actual (Pair).Text);
               end if;
            exception
               when Non_Static_Error =>
                  New_Item :=
                    (Kind => Non_Static, Text => +Actual (Pair).Text);
            end;
            Res.Constraint_Map.Insert
              (Key => +Param (Pair).Text, New_Item => New_Item);
         end loop;
      end return;
   end Translate_Discriminant_Constraints;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N : LAL.Type_Expr; Verbose : Boolean := False) return Translation_Result
   is
      Type_Decl_Node      : Base_Type_Decl;
      Intermediate_Result : Translation_Result;
   begin
      if Kind (N) in Ada_Anonymous_Type_Range then
         Type_Decl_Node := N.As_Anonymous_Type.F_Type_Decl.As_Base_Type_Decl;
      else
         --  For now, work on the full view of the type that we are trying to
         --  translate. If this proves useless/problematic this can be
         --  revisited.

         Type_Decl_Node := N.As_Subtype_Indication.P_Designated_Type_Decl;
      end if;

      Intermediate_Result := Translate (Type_Decl_Node, Verbose);

      if not Intermediate_Result.Success
        or else Kind (N) in Ada_Anonymous_Type
        or else Intermediate_Result.Res.all.Kind in Unsupported
        or else Is_Null (N.As_Subtype_Indication.F_Constraint)
      then
         return Intermediate_Result;
      end if;
      case Intermediate_Result.Res.all.Kind is
         when Discrete_Typ_Range =>
            pragma
              Assert
                (Kind (N.As_Subtype_Indication.F_Constraint)
                 in Ada_Range_Constraint_Range);
            return Res : Translation_Result (Success => True) do
               Res.Res :=
                 new Anonymous_Typ'
                   (Name                => Ada_Identifier_Vectors.Empty_Vector,
                    Last_Comp_Unit_Idx  => 1,
                    Fully_Private       =>
                      Intermediate_Result.Res.all.Fully_Private,
                    Private_Extension   =>
                      Intermediate_Result.Res.all.Private_Extension,
                    Named_Ancestor      => Intermediate_Result.Res,
                    Subtype_Constraints =>
                      new Discrete_Range_Constraint'
                        (Translate_Discrete_Range_Constraint
                           (N
                              .As_Subtype_Indication
                              .F_Constraint
                              .As_Range_Constraint)),
                    others              => <>);
            end return;

         when Real_Typ_Range =>
            return Res : Translation_Result (Success => True) do
               Res.Res :=
                 new Anonymous_Typ'
                   (Name                => Ada_Identifier_Vectors.Empty_Vector,
                    Last_Comp_Unit_Idx  => 1,
                    Named_Ancestor      => Intermediate_Result.Res,
                    Fully_Private       =>
                      Intermediate_Result.Res.all.Fully_Private,
                    Private_Extension   =>
                      Intermediate_Result.Res.all.Private_Extension,
                    Subtype_Constraints =>
                      new TGen.Types.Constraints.Constraint'Class'
                        (Translate_Real_Constraints
                           (N.As_Subtype_Indication.F_Constraint)),
                    others              => <>);
            end return;

         when Array_Typ_Range =>

            --  We need to check wether this anonymous array type isn't
            --  going to be larger than what is supported by the
            --  marshallers.

            declare
               Anon_Typ : constant Typ_Access :=
                 new Anonymous_Typ'
                   (Name                => Ada_Identifier_Vectors.Empty_Vector,
                    Last_Comp_Unit_Idx  => 1,
                    Named_Ancestor      => Intermediate_Result.Res,
                    Fully_Private       =>
                      Intermediate_Result.Res.all.Fully_Private,
                    Private_Extension   =>
                      Intermediate_Result.Res.all.Private_Extension,
                    Subtype_Constraints =>
                      new Index_Constraints'
                        (Translate_Index_Constraints
                           (N.As_Subtype_Indication.F_Constraint,
                            As_Unconstrained_Array_Typ
                              (Intermediate_Result.Res)
                              .Num_Dims)),
                    others              => <>);

               Total_Size : constant Big_Integer :=
                 As_Constrained_Array_Typ
                   (Anonymous_Typ (Anon_Typ.all).As_Named_Typ)
                   .Size;
            begin
               if Total_Size
                 > To_Big_Integer (TGen.Marshalling.Get_Array_Size_Limit)
               then
                  return Res : Translation_Result (Success => False) do
                     Res.Diagnostics :=
                       +("array type has more elements ("
                         & Trim (To_String (Total_Size))
                         & ") than the configured limit ("
                         & Trim
                             (Positive'Image
                                (TGen.Marshalling.Get_Array_Size_Limit))
                         & ")");
                  end return;
               else
                  return Res : Translation_Result (Success => True) do
                     Res.Res := Anon_Typ;
                  end return;
               end if;
            end;

         when Record_Typ_Range =>
            return Res : Translation_Result (Success => True) do
               pragma
                 Assert
                   (Kind (N.As_Subtype_Indication.F_Constraint)
                    in Ada_Composite_Constraint_Range
                      and N
                            .As_Subtype_Indication
                            .F_Constraint
                            .As_Composite_Constraint
                            .P_Is_Discriminant_Constraint);
               Res.Res :=
                 new Anonymous_Typ'
                   (Name                => Ada_Identifier_Vectors.Empty_Vector,
                    Last_Comp_Unit_Idx  => 1,
                    Named_Ancestor      => Intermediate_Result.Res,
                    Fully_Private       =>
                      Intermediate_Result.Res.all.Fully_Private,
                    Private_Extension   =>
                      Intermediate_Result.Res.all.Private_Extension,
                    Subtype_Constraints =>
                      new Discriminant_Constraints'
                        (Translate_Discriminant_Constraints
                           (N
                              .As_Subtype_Indication
                              .F_Constraint
                              .As_Composite_Constraint)),
                    Is_Class_Wide       =>
                      not Type_Decl_Node.P_Classwide_Type.Is_Null,
                    others              => <>);
            end return;

         when others =>
            return Intermediate_Result;
      end case;
   exception
      when Exc : Property_Error =>
         return
           (Success     => False,
            Diagnostics =>
              To_Unbounded_String ("Error translating ")
              & N.Image
              & " : "
              & Ada.Exceptions.Exception_Message (Exc));
      when Exc : Translation_Error =>
         return
           (Success     => False,
            Diagnostics =>
              To_Unbounded_String
                ("Error translating the following constraints:")
              & Ada.Exceptions.Exception_Information (Exc));
   end Translate;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N : LAL.Base_Type_Decl; Verbose : Boolean := False)
      return Translation_Result
   is
      use Translation_Maps;

      Full_Decl : constant Base_Type_Decl := N.P_Full_View;

   begin
      --  Do not memoize anonymous types

      if Is_Null (Full_Decl.F_Name) then
         return Translate_Internal (Full_Decl, Verbose);
      end if;

      declare
         Cache_T : Typ_Access;
         FQN     : constant Ada_Qualified_Name :=
           Convert_Qualified_Name (Full_Decl.P_Fully_Qualified_Name_Array);
      begin
         --  If we have the type name in the cache, return it

         if Get_From_Cache (FQN, Cache_T) then
            return Res : Translation_Result (Success => True) do
               Res.Res := Cache_T;
            end return;
         end if;

         --  Otherwise, compute the type translation and store it in the cache

         declare
            Trans_Res : constant Translation_Result :=
              Translate_Internal (Full_Decl, Verbose);
         begin
            if Trans_Res.Success then
               Translation_Cache.Insert (FQN, Trans_Res.Res);
               Type_Decl_Cache.Insert (FQN, Full_Decl);
            end if;
            return Trans_Res;
         end;

      end;
   end Translate;

   ------------------------
   -- Translate_Internal --
   ------------------------

   function Translate_Internal
     (N                 : LAL.Base_Type_Decl;
      Verbose           : Boolean := False;
      Assume_Non_Static : Boolean := False) return Translation_Result
   is
      Root_Type : constant Base_Type_Decl := N.P_Root_Type.P_Full_View;
      Is_Static : Boolean := not Assume_Non_Static;
      --  Relevant only for Scalar types / array bounds
      --  / discriminant constraints.

      Type_Name : constant Defining_Name :=
        (if not (Kind (N) in Ada_Anonymous_Type_Decl_Range)
         then N.P_Defining_Name
         else No_Defining_Name);

      Comp_Unit_Decl : constant Basic_Decl :=
        Ultimate_Enclosing_Compilation_Unit (N.As_Basic_Decl);

      Comp_Unit_Idx : constant Positive :=
        Comp_Unit_Decl.P_Fully_Qualified_Name_Array'Last;

      FQN : Ada_Qualified_Name :=
        (if not Type_Name.Is_Null
         then Convert_Qualified_Name (Type_Name.P_Fully_Qualified_Name_Array)
         else Ada_Identifier_Vectors.Empty);

      First_Part : constant Basic_Decl'Class := N.P_All_Parts (1);
      --  First part of the declaration. Used to determine whether the type we
      --  are translating is private or not.

      Specialized_Res : Translation_Result;

   begin
      Verbose_Diag := Verbose;
      Is_Static :=
        Is_Static
        and then N.P_Is_Static_Decl

        --  The T'Base of a discrete type T has unknown bounds

        and then not (Kind (N) in Ada_Discrete_Base_Subtype_Decl);

      if Is_Null (Type_Name) then

         --  Anonymous types at this level are either anonymous array
         --  declarations or anonymous access types, both of which we don't
         --  intend to support.

         Specialized_Res := (Success => True, others => <>);

         Specialized_Res.Res :=
           new Unsupported_Typ'
             (Reason =>
                To_Unbounded_String
                  ("Anonymous array or access type unsupported"),
              others => <>);

      --  Types that are declared in a library level generic instantiation
      --  are not supported at the moment, as the support packages would
      --  need to be generic instances themselves (with other rules to
      --  follow), see RM 10.1.1 (17/3, 18).
      --
      --  TODO??? Investigate if there are issues in generating the helper
      --  packages as non-child units in this case, see #184.

      elsif Comp_Unit_Decl.Kind in Ada_Generic_Instantiation then
         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Unsupported_Typ'
             (Reason =>
                To_Unbounded_String
                  ("types declared a generic package instantiation that is a"
                   & " library item are unsupported"),
              others => <>);

      elsif Text.Image (N.P_Root_Type.P_Fully_Qualified_Name)
        = "System.Address"
      then

         --  Special case for System.Address, which is actually defined as a
         --  modular integer but for which we do not want to generate any
         --  values.

         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Unsupported_Typ'
             (Reason => To_Unbounded_String ("System.Address unsupported"),
              others => <>);

      elsif (N.Kind = Ada_Concrete_Type_Decl
             and then N.As_Concrete_Type_Decl.F_Type_Def.Kind
                      = Ada_Derived_Type_Def
             and then TGen.LAL_Utils.Derive_Opaque_Type (N))
        or (N.Kind in Ada_Subtype_Decl_Range
            and then TGen.LAL_Utils.Derive_Opaque_Type (N.As_Base_Type_Decl))
      then

         Specialized_Res := (Success => True, others => <>);

         declare
            Declaration_Type_Name : constant Ada_Qualified_Name :=
              TGen.Strings.To_Qualified_Name
                (Langkit_Support.Text.Encode
                   (N.As_Base_Type_Decl.P_Fully_Qualified_Name, "utf-8"));
            --  Translate the parent type
            Parent_Type           : constant Translation_Result :=
              (if N.Kind = Ada_Concrete_Type_Decl
               then
                 Translate
                   (N
                      .As_Concrete_Type_Decl
                      .F_Type_Def
                      .As_Derived_Type_Def
                      .F_Subtype_Indication
                      .P_Designated_Type_Decl)
               else
                 Translate
                   (N.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl));
         begin
            if Parent_Type.Success then
               if TGen.Marshalling.Needs_Header (Parent_Type.Res.all) then
                  Specialized_Res.Res :=
                    new Unsupported_Typ'
                      (Reason =>
                         To_Unbounded_String
                           ("Opaque type deriving an unconstrained "
                            & "type is not supported"),
                       others => <>);
               else
                  Specialized_Res.Res :=
                    new Derived_Private_Subtype_Typ'
                      (Declaration_Type_Name => Declaration_Type_Name,
                       Parent_Type           => Parent_Type.Res,
                       others                => <>);
               end if;
            else
               return Parent_Type;
            end if;
         end;

      elsif First_Part.As_Base_Type_Decl.P_Is_Private
        and then Positive (FQN.Length) - Comp_Unit_Idx > 1
      then
         --  We are dealing with a private type declared in a nested package,
         --  consider this as unsupported.

         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Unsupported_Typ'
             (Reason =>
                To_Unbounded_String
                  ("Private types declared in nested package are not"
                   & " supported"),
              others => <>);
      elsif Root_Type.P_Is_Formal then
         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Formal_Typ'
             (Reason =>
                To_Unbounded_String ("Generic formal types are unsupported"),
              others => <>);
      elsif Root_Type.P_Is_Int_Type then
         Specialized_Res := Translate_Int_Decl (N);

      elsif P_Is_Derived_Type
              (Node => N, Other_Type => N.P_Bool_Type.As_Base_Type_Decl)
      then
         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res := new Bool_Typ'(Is_Static => True, others => <>);
      elsif Root_Type.P_Is_Enum_Type then

         if not Is_Static then
            Specialized_Res := (Success => True, others => <>);
            Specialized_Res.Res :=
              new Other_Enum_Typ'(Is_Static => False, others => <>);
         end if;
         declare
            Root_Type_Name : constant String :=
              Text.Image (Root_Type.P_Unique_Identifying_Name);
         begin
            if Root_Type_Name = "standard.character"
              or else Root_Type_Name = "standard.wide_character"
              or else Root_Type_Name = "standard.wide_wide_character"
            then
               Specialized_Res := Translate_Char_Decl (N);
            else
               Specialized_Res := Translate_Enum_Decl (N, Root_Type);
            end if;
         end;

      elsif Root_Type.P_Is_Float_Type then
         if Is_Static then
            Specialized_Res := Translate_Float_Decl (N);
         else
            Specialized_Res := (Success => True, others => <>);
            Specialized_Res.Res :=
              new Float_Typ'
                (Is_Static => False, Has_Range => False, others => <>);
         end if;

      elsif Root_Type.P_Is_Fixed_Point then
         if Kind (Root_Type.As_Type_Decl.F_Type_Def)
            in Ada_Ordinary_Fixed_Point_Def_Range
         then
            if Is_Static then
               Specialized_Res := Translate_Ordinary_Fixed_Decl (N);
            else
               Specialized_Res := (Success => True, others => <>);
               Specialized_Res.Res :=
                 new Ordinary_Fixed_Typ'(Is_Static => False, others => <>);
            end if;
         else
            if Is_Static then
               Specialized_Res := Translate_Decimal_Fixed_Decl (N);
            else
               Specialized_Res := (Success => True, others => <>);
               Specialized_Res.Res :=
                 new Decimal_Fixed_Typ'
                   (Is_Static => False, Has_Range => False, others => <>);
            end if;
         end if;

      elsif Root_Type.P_Is_Array_Type then
         Specialized_Res := Translate_Array_Decl (N);

      elsif Root_Type.P_Is_Record_Type then
         if Root_Type.P_Is_Tagged_Type then
            Specialized_Res := (Success => True, others => <>);
            Specialized_Res.Res :=
              new Unsupported_Typ'
                (Reason => To_Unbounded_String ("tagged types not supported"),
                 others => <>);
         else
            Specialized_Res := Translate_Record_Decl (N);
         end if;

      elsif Root_Type.P_Is_Access_Type then
         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Access_Typ'
             (Reason => To_Unbounded_String ("Access types are not supported"),
              others => <>);
      else
         Specialized_Res := (Success => True, others => <>);
         Specialized_Res.Res :=
           new Unsupported_Typ'
             (Reason => To_Unbounded_String ("Unknown type kind"),
              others => <>);
      end if;

      --  Fill the common bits if we got a successful translation

      if Specialized_Res.Success then
         Specialized_Res.Res.all.Name := FQN;
         Specialized_Res.Res.all.Last_Comp_Unit_Idx := Comp_Unit_Idx;
         Specialized_Res.Res.all.Fully_Private := Decl_Is_Fully_Private (N);
         Specialized_Res.Res.all.Private_Extension :=
           Basic_Decl'(N.P_All_Parts (1)).As_Base_Type_Decl.P_Is_Private;
         Specialized_Res.Res.all.Is_Class_Wide :=
           N.Kind in Ada_Classwide_Type_Decl_Range;

         --  Checks if the type has a static predicate aspect.
         Specialized_Res.Res.all.Has_Static_Predicate :=
           N.As_Basic_Decl.P_Has_Aspect
             (Langkit_Support.Text.To_Unbounded_Text ("Static_Predicate"));
      end if;

      return Specialized_Res;

   exception
      when Exc : Property_Error =>
         return
           (Success     => False,
            Diagnostics =>
              To_Unbounded_String ("Error translating ")
              & N.Image
              & " : "
              & Ada.Exceptions.Exception_Information (Exc));
      when Exc : Non_Static_Error =>
         if Verbose_Diag then
            Put_Line
              ("Lal limitation during static evaluation: "
               & Ada.Exceptions.Exception_Message (Exc));
         end if;
         return Translate_Internal (N, Verbose_Diag, True);
      when Exc : Translation_Error =>
         return
           (Success     => False,
            Diagnostics =>
              +(Image (Type_Name.Text)
                & ": "
                & Ada.Exceptions.Exception_Message (Exc)));
   end Translate_Internal;

   function Translate_Globals
     (N : Expr; Verbose : Boolean) return Translation_Result;
   --  Return the list of globals specified in the global aspect, which are
   --  returned encapsulated in a Record_Typ (for conveniency purposes) if the
   --  translation of the globals type is successful. If one of the globals
   --  is not supported, returned an unsuccessful Translation_Result.
   --
   --  See the documentation of the Globals field of the
   --  TGen.Types.Record_Types.Function_Typ type for more information.

   ---------------------
   -- Process_Globals --
   ---------------------

   function Translate_Globals
     (N : Expr; Verbose : Boolean) return Translation_Result
   is
      Rec : constant Nondiscriminated_Record_Typ_Access :=
        new Nondiscriminated_Record_Typ;
      --  Record_Typ encapsulating the globals (that are stored in
      --  Rec.Component_Types).

      Diagnostics : Unbounded_String;
      Result      : Translation_Result (Success => True);

      function Process_Global (N : LAL.Name) return Boolean;
      --  Process the given global. If the global type translation result is
      --  not succesful, return False and store in the local Diagnostics the
      --  diagnostics.

      --------------------
      -- Process_Global --
      --------------------

      function Process_Global (N : LAL.Name) return Boolean is
         Global : constant Basic_Decl := N.P_Referenced_Decl;
      begin
         --  Ignore cases such as Global => null

         if Global.Is_Null then
            return True;
         end if;

         if Kind (Global) = Ada_Object_Decl then
            declare
               Obj_Decl : constant Object_Decl := Global.As_Object_Decl;
            begin
               --  If the global is a constant, ignore it

               if not Obj_Decl.F_Has_Constant then
                  declare
                     Global_Typ_Translation : constant Translation_Result :=
                       Translate (Global.As_Object_Decl.F_Type_Expr, Verbose);
                  begin
                     if Global_Typ_Translation.Success then
                        Rec.all.Component_Types.Insert
                          (+To_Ada
                              (Convert_Qualified_Name
                                 (N
                                    .P_Referenced_Defining_Name
                                    .P_Fully_Qualified_Name_Array)),
                           Global_Typ_Translation.Res);
                     else
                        Diagnostics := Global_Typ_Translation.Diagnostics;
                        return False;
                     end if;
                  end;
               end if;
            end;
         end if;
         return True;
      end Process_Global;

   begin
      case Kind (N) is
         when Ada_Aggregate =>
            for Assoc of N.As_Aggregate.F_Assocs loop
               if Kind (Assoc) = Ada_Aggregate_Assoc then
                  declare
                     Aggr_Assoc  : constant Aggregate_Assoc :=
                       Assoc.As_Aggregate_Assoc;
                     Designator  : Identifier;
                     Designators : constant Alternatives_List :=
                       Aggr_Assoc.F_Designators;
                  begin
                     --  Grab the globals annotation, if any (input,
                     --  output ...).

                     declare
                        Designators_List : Ada_Node_List renames
                          Ada_Node_List (Designators);
                        Fst              : constant Positive :=
                          Ada_Node_List_First (Designators_List);
                     begin
                        if Ada_Node_List_Has_Element (Designators_List, Fst)
                        then
                           declare
                              Fst_Elem : constant Ada_Node'Class :=
                                Ada_Node_List_Element (Designators_List, Fst);
                           begin
                              if Kind (Fst_Elem) = LALCO.Ada_Identifier then
                                 Designator := Fst_Elem.As_Identifier;
                              end if;
                           end;
                        end if;
                     end;

                     if Designator.Is_Null then

                        --  In this case, we have a list of global variables,
                        --  e.g. "with Global => (A, B, C)".

                        declare
                           Assoc_Expr : constant Expr := Aggr_Assoc.F_R_Expr;
                        begin
                           if Kind (Assoc_Expr) in LALCO.Ada_Name then
                              if not Process_Global (Assoc_Expr.As_Name) then
                                 raise Translation_Error with +Diagnostics;
                              end if;
                           end if;
                        end;

                     else
                        --  We are in the case where the user explicitly
                        --  specified the globals kind, e.g.
                        --  "with Global => (Input => (<global>))". Grab the
                        --  Input + In_Out annotated globals.

                        declare
                           Globals_Kind : constant String :=
                             +To_Lower (Designator.Text);
                        begin
                           if Globals_Kind in "input" | "in_out" then
                              declare
                                 Global_Comps : constant Component_Map :=
                                   As_Record_Typ
                                     (Translate_Globals
                                        (Aggr_Assoc.F_R_Expr, Verbose)
                                        .Res)
                                     .Component_Types;
                              begin
                                 for Cur in Global_Comps.Iterate loop
                                    Rec.Component_Types.Insert
                                      (Component_Maps.Key (Cur),
                                       Component_Maps.Element (Cur));
                                 end loop;
                              end;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end loop;

         when LALCO.Ada_Name =>
            if not Process_Global (N.As_Name) then
               raise Translation_Error with +Diagnostics;
            end if;

         when others =>
            null;
      end case;
      Result.Res := Typ_Access (Rec);
      return Result;
   end Translate_Globals;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (N : LAL.Basic_Decl; Verbose : Boolean := False) return Translation_Result
   is
      F_Typ         : constant Function_Typ_Access := new Function_Typ;
      Result        : Translation_Result (Success => True);
      Comp_Unit_Idx : constant Positive :=
        Ultimate_Enclosing_Compilation_Unit (N)
          .P_Fully_Qualified_Name_Array'Last;

      UID      : constant String := Test.Common.Mangle_Hash_16 (Subp => N);
      Long_UID : constant String := Test.Common.Mangle_Hash_Full (Subp => N);

      Designated_Decl : Basic_Decl := N;
   begin
      --  TODO??? when traversing generic instantiations with the
      --  tgen_marshalling testsuite driver, their kind is Ada_Subp_Decl.
      --  Investigate why.

      if Kind (N) = Ada_Generic_Subp_Instantiation then
         Designated_Decl :=
           N
             .As_Generic_Subp_Instantiation
             .P_Designated_Generic_Decl
             .As_Basic_Decl;
      end if;

      --  Check whether this is a private subprogram with private parameter
      --  types.

      if Decl_Is_Fully_Private (N) then
         for Param of Designated_Decl.P_Subp_Spec_Or_Null.P_Params loop
            if Decl_Is_Fully_Private (Param.F_Type_Expr.P_Designated_Type_Decl)
            then
               Result.Res :=
                 new Unsupported_Typ'
                   (Reason =>
                      To_Unbounded_String
                        ("private subprograms with private parameters are"
                         & " unsupported"),
                    Name   =>
                      Convert_Qualified_Name (N.P_Fully_Qualified_Name_Array),
                    others => <>);
               return Result;
            end if;
         end loop;
      end if;

      F_Typ.all.Last_Comp_Unit_Idx := Comp_Unit_Idx;
      F_Typ.all.Name :=
        Convert_Qualified_Name (N.P_Fully_Qualified_Name_Array)
        & TGen.Strings.Ada_Identifier
            (Ada.Strings.Unbounded.To_Unbounded_String (UID));

      --  Check if we have already translated the function type

      declare
         Cache_T : Typ_Access;
      begin
         if Get_From_Cache (F_Typ.all.Name, Cache_T) then
            Result.Res := Cache_T;
            return Result;
         end if;
      end;

      declare
         Subp_Spec : constant Base_Subp_Spec :=
           Designated_Decl.P_Subp_Spec_Or_Null;
      begin
         for Param of Subp_Spec.P_Params loop
            declare
               Current_Typ : constant Translation_Result :=
                 Translate (Param.F_Type_Expr.P_Designated_Type_Decl, Verbose);
            begin
               if Current_Typ.Success then

                  for Id of Param.F_Ids loop
                     F_Typ.all.Component_Types.Insert
                       (Key      => +Id.As_Defining_Name.Text,
                        New_Item => Current_Typ.Res);
                     F_Typ.all.Param_Modes.Insert
                       (Key      => +Id.As_Defining_Name.Text,
                        New_Item =>
                          (case Param.F_Mode is
                             when Ada_Mode_Default | Ada_Mode_In => In_Mode,
                             when Ada_Mode_In_Out => In_Out_Mode,
                             when others => Out_Mode));
                     F_Typ.all.Param_Order.Append (+Id.As_Defining_Name.Text);
                  end loop;
               else
                  return Current_Typ;
               end if;
            end;
         end loop;

         if not Subp_Spec.P_Returns.Is_Null then
            declare
               Ret : constant Translation_Result :=
                 Translate (Subp_Spec.P_Returns, Verbose);
            begin
               if not Ret.Success then
                  return (False, Ret.Diagnostics);
               end if;

               F_Typ.Ret_Typ := Ret.Res;
            end;
         else
            F_Typ.Ret_Typ := null;
         end if;
      end;

      --  Function type was successfully translated

      F_Typ.Subp_UID := +UID;
      F_Typ.Long_UID := +Long_UID;

      --  This function can only be used outside of the private part if none of
      --  its parameter types are fully private.

      F_Typ.Fully_Private :=
        (for some Param of F_Typ.Component_Types => Param.all.Fully_Private);

      --  Check the globals through the Globals aspect

      declare
         Global_Aspect : constant Unbounded_Text_Type :=
           To_Unbounded_Text (To_Text ("Global"));
      begin
         if N.P_Has_Aspect (Global_Aspect) then
            F_Typ.Globals :=
              As_Record_Typ
                (Translate_Globals
                   (N.P_Get_Aspect_Spec_Expr (Global_Aspect), Verbose)
                   .Res)
                .Component_Types;
         end if;
      end;

      Translation_Cache.Insert (F_Typ.Name, Typ_Access (F_Typ));
      Result.Res := Typ_Access (F_Typ);
      return Result;
   exception
      when Exc : Translation_Error =>
         return
           (Success     => False,
            Diagnostics =>
              To_Unbounded_String (Ada.Exceptions.Exception_Message (Exc)));
   end Translate;

   -----------------------
   -- Print_Cache_Stats --
   -----------------------

   procedure Print_Cache_Stats is
   begin
      New_Line;
      Put_Line ("Items in cache :" & Translation_Cache.Length'Image);
      Put_Line ("Cache hits  :" & Cache_Hits'Image);
      Put_Line ("Cache misses:" & Cache_Miss'Image);
   end Print_Cache_Stats;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache is
   begin
      Translation_Cache.Clear;
   end Clear_Cache;

end TGen.Types.Translation;
