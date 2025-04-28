------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with GNAT.OS_Lib;

with Utils.String_Utilities;

pragma Warnings (Off);
with TGen.Big_Int;
with TGen.Big_Reals;
with TGen.Marshalling;
with TGen.Strings;              use TGen.Strings;
with TGen.Templates;
with TGen.Types.Array_Types;    use TGen.Types.Array_Types;
with TGen.Types.Constraints;    use TGen.Types.Constraints;
with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;
with TGen.Types.Enum_Types;     use TGen.Types.Enum_Types;
with TGen.Types.Int_Types;      use TGen.Types.Int_Types;
with TGen.Types.Real_Types;     use TGen.Types.Real_Types;
with TGen.Types.Record_Types;   use TGen.Types.Record_Types;
pragma Warnings (On);

package body TGen.Type_Representation is

   function Esc (Str : String) return String renames
     Utils.String_Utilities.Escape_String_Literal;

   procedure Collect_Info_For_Constraint
     (Ty_Prefix                : String;
      Constraint               : TGen.Types.Constraints.Constraint'Class;
      Constraint_Decl_Template : String;
      Constraint_Init_Template : String;
      Constraint_Decl          : out Unbounded_String;
      Constraint_Init          : out Unbounded_String);
   --  Return the specification and initialization for a constraint

   ---------------------------------
   -- Collect_Info_For_Constraint --
   ---------------------------------

   procedure Collect_Info_For_Constraint
     (Ty_Prefix                : String;
      Constraint               : TGen.Types.Constraints.Constraint'Class;
      Constraint_Decl_Template : String;
      Constraint_Init_Template : String;
      Constraint_Decl          : out Unbounded_String;
      Constraint_Init          : out Unbounded_String)
   is
      Assocs : Translate_Set;

      procedure Collect_Info_For_Discrete_Constraint
        (Constraint      : Discrete_Constraint_Value;
         Constraint_Kind : out Unbounded_String;
         Value           : out Unbounded_String);
      --  Collect the constraint kind and the value for the given constraint

      procedure Collect_Info_For_Real_Constraint
        (Constraint      : Real_Constraint_Value;
         Constraint_Kind : out Unbounded_String;
         Value           : out Unbounded_String);
      --  Collect the constraint kind and the value for the given constraint

      procedure Collect_Info_For_Discrete_Range_Constraint
        (Constraint         : Discrete_Range_Constraint;
         Constraint_Kind_LB : out Unbounded_String;
         Value_LB           : out Unbounded_String;
         Constraint_Kind_UB : out Unbounded_String;
         Value_UB           : out Unbounded_String);

      procedure Collect_Info_For_Real_Range_Constraint
        (Constraint         : Real_Range_Constraint;
         Constraint_Kind_LB : out Unbounded_String;
         Value_LB           : out Unbounded_String;
         Constraint_Kind_UB : out Unbounded_String;
         Value_UB           : out Unbounded_String);

      procedure Collect_Info_For_Discrete_Constraint
        (Constraint      : Discrete_Constraint_Value;
         Constraint_Kind : out Unbounded_String;
         Value           : out Unbounded_String) is
      begin
         Constraint_Kind := +(Constraint.Kind'Image);
         case Constraint.Kind is
            when Static =>
               Value := +Big_Int.To_String (Constraint.Int_Val);

            when Non_Static =>
               Value := Constraint.Text;

            when Discriminant =>
               Value := Constraint.Disc_Name;
         end case;
      end Collect_Info_For_Discrete_Constraint;

      --------------------------------------
      -- Collect_Info_For_Real_Constraint --
      --------------------------------------

      procedure Collect_Info_For_Real_Constraint
        (Constraint      : Real_Constraint_Value;
         Constraint_Kind : out Unbounded_String;
         Value           : out Unbounded_String) is
      begin
         Constraint_Kind := +(Constraint.Kind'Image);
         case Constraint.Kind is
            when Static =>
               Value := +Big_Reals.To_String (Constraint.Real_Val);

            when Non_Static =>
               Value := Constraint.Text;
         end case;
      end Collect_Info_For_Real_Constraint;

      ------------------------------------------------
      -- Collect_Info_For_Discrete_Range_Constraint --
      ------------------------------------------------

      procedure Collect_Info_For_Discrete_Range_Constraint
        (Constraint         : Discrete_Range_Constraint;
         Constraint_Kind_LB : out Unbounded_String;
         Value_LB           : out Unbounded_String;
         Constraint_Kind_UB : out Unbounded_String;
         Value_UB           : out Unbounded_String) is
      begin
         Collect_Info_For_Discrete_Constraint
           (Constraint.Low_Bound, Constraint_Kind_LB, Value_LB);
         Collect_Info_For_Discrete_Constraint
           (Constraint.High_Bound, Constraint_Kind_UB, Value_UB);
      end Collect_Info_For_Discrete_Range_Constraint;

      --------------------------------------------
      -- Collect_Info_For_Real_Range_Constraint --
      --------------------------------------------

      procedure Collect_Info_For_Real_Range_Constraint
        (Constraint         : Real_Range_Constraint;
         Constraint_Kind_LB : out Unbounded_String;
         Value_LB           : out Unbounded_String;
         Constraint_Kind_UB : out Unbounded_String;
         Value_UB           : out Unbounded_String) is
      begin
         Collect_Info_For_Real_Constraint
           (Constraint.Low_Bound, Constraint_Kind_LB, Value_LB);
         Collect_Info_For_Real_Constraint
           (Constraint.High_Bound, Constraint_Kind_UB, Value_UB);
      end Collect_Info_For_Real_Range_Constraint;

   begin
      Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));

      if Constraint in Discrete_Range_Constraint'Class then
         Insert (Assocs, Assoc ("CONSTRAINT", "Discrete_Range_Constraint"));
         declare
            Constraint_Kind_LB, Constraint_Kind_UB : Unbounded_String;
            Value_LB, Value_UB                     : Unbounded_String;
         begin
            Collect_Info_For_Discrete_Range_Constraint
              (Discrete_Range_Constraint (Constraint),
               Constraint_Kind_LB,
               Value_LB,
               Constraint_Kind_UB,
               Value_UB);
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_LB", Constraint_Kind_LB));
            Insert (Assocs, Assoc ("VALUE_LB", Value_LB));
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_UB", Constraint_Kind_UB));
            Insert (Assocs, Assoc ("VALUE_UB", Value_UB));
         end;

      elsif Constraint in Real_Range_Constraint'Class then
         Insert (Assocs, Assoc ("CONSTRAINT", "Real_Range_Constraint"));
         declare
            Constraint_Kind_LB, Constraint_Kind_UB : Unbounded_String;
            Value_LB, Value_UB                     : Unbounded_String;
         begin
            Collect_Info_For_Real_Range_Constraint
              (Real_Range_Constraint (Constraint),
               Constraint_Kind_LB,
               Value_LB,
               Constraint_Kind_UB,
               Value_UB);
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_LB", Constraint_Kind_LB));
            Insert (Assocs, Assoc ("VALUE_LB", Value_LB));
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_UB", Constraint_Kind_UB));
            Insert (Assocs, Assoc ("VALUE_UB", Value_UB));
         end;

      elsif Constraint in Digits_Constraint'Class then
         Insert (Assocs, Assoc ("CONSTRAINT", "Digits_Constraint"));
         declare
            Dig_Constraint         : constant Digits_Constraint'Class :=
              Digits_Constraint'Class (Constraint);
            Digits_Constraint_Kind : Unbounded_String;
            Digits_Value           : Unbounded_String;
         begin
            --  Fill in the discrete digits constraints

            Collect_Info_For_Discrete_Constraint
              (Dig_Constraint.Digits_Value,
               Digits_Constraint_Kind,
               Digits_Value);
            Insert
              (Assocs,
               Assoc ("CONSTRAINT_KIND_DIGITS_VALUE", Digits_Constraint_Kind));
            Insert (Assocs, Assoc ("DIGITS_VALUE", Digits_Value));

            --  Now fill the range constraint if any

            Insert (Assocs, Assoc ("HAS_RANGE", Dig_Constraint.Has_Range));

            if Dig_Constraint.Has_Range then
               declare
                  Constraint_Kind_LB, Constraint_Kind_UB : Unbounded_String;
                  Value_LB, Value_UB                     : Unbounded_String;
               begin
                  Collect_Info_For_Real_Range_Constraint
                    (Dig_Constraint.Range_Value,
                     Constraint_Kind_LB,
                     Value_LB,
                     Constraint_Kind_UB,
                     Value_UB);
                  Insert
                    (Assocs, Assoc ("CONSTRAINT_KIND_LB", Constraint_Kind_LB));
                  Insert (Assocs, Assoc ("VALUE_LB", Value_LB));
                  Insert
                    (Assocs, Assoc ("CONSTRAINT_KIND_UB", Constraint_Kind_UB));
                  Insert (Assocs, Assoc ("VALUE_UB", Value_UB));
               end;
            end if;
         end;

      elsif Constraint in Index_Constraints'Class then
         Insert (Assocs, Assoc ("CONSTRAINT", "Index_Constraints"));
         declare
            Ind_Constr                               :
              constant Index_Constraints'Class :=
                Index_Constraints'Class (Constraint);
            I                                        : Positive := 1;
            Index_Numbers                            : Vector_Tag;
            Presents                                 : Vector_Tag;
            Constraint_Kind_LBs, Constraint_Kind_UBs : Vector_Tag;
            Value_LBs, Value_UBs                     : Vector_Tag;
         begin
            for Index of Ind_Constr.Constraint_Array loop
               declare
                  Constraint_Kind_LB, Constraint_Kind_UB : Unbounded_String;
                  Value_LB, Value_UB                     : Unbounded_String;
               begin
                  if Index.Present then
                     Collect_Info_For_Discrete_Range_Constraint
                       (Index.Discrete_Range,
                        Constraint_Kind_LB,
                        Value_LB,
                        Constraint_Kind_UB,
                        Value_UB);
                  end if;
                  Constraint_Kind_LBs :=
                    Constraint_Kind_LBs & Constraint_Kind_LB;
                  Constraint_Kind_UBs :=
                    Constraint_Kind_UBs & Constraint_Kind_UB;
                  Value_LBs := Value_LBs & Value_LB;
                  Value_UBs := Value_UBs & Value_UB;
                  Presents := Presents & Index.Present;
                  Index_Numbers := Index_Numbers & I;
                  I := I + 1;
               end;
            end loop;
            Insert
              (Assocs, Assoc ("NUM_DIMS", Ind_Constr.Constraint_Array'Length));
            Insert (Assocs, Assoc ("INDEX_NUMBER", Index_Numbers));
            Insert (Assocs, Assoc ("PRESENT", Presents));
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_LB", Constraint_Kind_LBs));
            Insert (Assocs, Assoc ("VALUE_LB", Value_LBs));
            Insert (Assocs, Assoc ("CONSTRAINT_KIND_UB", Constraint_Kind_UBs));
            Insert (Assocs, Assoc ("VALUE_UB", Value_UBs));
         end;

      elsif Constraint in Discriminant_Constraints'Class then
         Insert (Assocs, Assoc ("CONSTRAINT", "Discriminant_Constraints"));
         declare
            Disc_Constraints : constant Discriminant_Constraints'Class :=
              Discriminant_Constraints'Class (Constraint);
            Discr_Names      : Vector_Tag;
            Constraint_Kinds : Vector_Tag;
            Values           : Vector_Tag;
         begin
            for Disc_Constraint in Disc_Constraints.Constraint_Map.Iterate loop
               declare
                  use Discriminant_Constraint_Maps;
                  Constraint_Kind, Value : Unbounded_String;
               begin
                  Collect_Info_For_Discrete_Constraint
                    (Element (Disc_Constraint), Constraint_Kind, Value);
                  Discr_Names := Discr_Names & Key (Disc_Constraint);
                  Constraint_Kinds := Constraint_Kinds & Constraint_Kind;
                  Values := Values & Value;
               end;
            end loop;

            --  Fill in the association

            Insert (Assocs, Assoc ("DISCR_NAME", Discr_Names));
            Insert (Assocs, Assoc ("CONSTRAINT_KIND", Constraint_Kinds));
            Insert (Assocs, Assoc ("VALUE", Values));
         end;
      else
         raise Program_Error;
      end if;

      --  Now we can generate the constraint template

      Constraint_Decl := Parse (Constraint_Decl_Template, Assocs);
      Constraint_Init := Parse (Constraint_Init_Template, Assocs);
   end Collect_Info_For_Constraint;

   procedure Collect_Info_For_Anonymous_Typ
     (T                                                        :
        Anonymous_Typ'Class;
      Anonymous_Typ_Decl_Template, Anonymous_Typ_Init_Template : String;
      Constraint_Decl_Template, Constraint_Init_Template       : String;
      T_Decl, T_Init                                           :
        out Unbounded_String;
      Is_Top_Level_Generic                                     : Boolean :=
        False);
   --  Return the declarations and initialization for an anonymous type

   ------------------------------------
   -- Collect_Info_For_Anonymous_Typ --
   ------------------------------------

   procedure Collect_Info_For_Anonymous_Typ
     (T                                                        :
        Anonymous_Typ'Class;
      Anonymous_Typ_Decl_Template, Anonymous_Typ_Init_Template : String;
      Constraint_Decl_Template, Constraint_Init_Template       : String;
      T_Decl, T_Init                                           :
        out Unbounded_String;
      Is_Top_Level_Generic                                     : Boolean :=
        False)
   is
      Ty_Prefix : constant String := T.Slug (Is_Top_Level_Generic);
      Ty_Name   : constant String :=
        Esc
          (T.FQN (No_Std => True, Top_Level_Generic => Is_Top_Level_Generic));
      Assocs    : Translate_Set;
   begin
      Insert (Assocs, Assoc ("TY_NAME", Ty_Name));
      Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));

      --  Start with writing the ancestor

      Insert (Assocs, Assoc ("TY_PREFIX_ANCESTOR", T.Named_Ancestor.all.Slug));

      --  Then write down the constraint

      if T.Subtype_Constraints /= null then
         declare
            Constraint_Decl, Constraint_Init : Unbounded_String;
         begin
            Collect_Info_For_Constraint
              (Ty_Prefix,
               T.Subtype_Constraints.all,
               Constraint_Decl_Template,
               Constraint_Init_Template,
               Constraint_Decl,
               Constraint_Init);

            Insert (Assocs, Assoc ("HAS_CONSTRAINT", True));
            Insert (Assocs, Assoc ("CONSTRAINT_SPEC", Constraint_Decl));
            Insert (Assocs, Assoc ("CONSTRAINT_INIT", Constraint_Init));
         end;
      end if;

      T_Decl := Parse (Anonymous_Typ_Decl_Template, Assocs);
      T_Init := Parse (Anonymous_Typ_Init_Template, Assocs);
   end Collect_Info_For_Anonymous_Typ;

   procedure Collect_Info_For_Instance_Typ
     (T                                                      :
        Instance_Typ'Class;
      Instance_Typ_Decl_Template, Instance_Typ_Init_Template : String;
      T_Decl, T_Init                                         :
        out Unbounded_String;
      Is_Top_Level_Generic                                   : Boolean :=
        False);

   procedure Collect_Info_For_Instance_Typ
     (T                                                      :
        Instance_Typ'Class;
      Instance_Typ_Decl_Template, Instance_Typ_Init_Template : String;
      T_Decl, T_Init                                         :
        out Unbounded_String;
      Is_Top_Level_Generic                                   : Boolean :=
        False)
   is
      Ty_Prefix : constant String := T.Slug (Is_Top_Level_Generic);
      Ty_Name   : constant String :=
        Esc
          (T.FQN (No_Std => True, Top_Level_Generic => Is_Top_Level_Generic));
      Assocs    : Translate_Set;
   begin
      Insert (Assocs, Assoc ("TY_NAME", Ty_Name));
      Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));

      --  Start with writing the original type

      Insert (Assocs, Assoc ("ORIG_TY_PREFIX", T.Orig_Typ.all.Slug));

      T_Decl := Parse (Instance_Typ_Decl_Template, Assocs);
      T_Init := Parse (Instance_Typ_Init_Template, Assocs);
   end Collect_Info_For_Instance_Typ;

   procedure Collect_Info_For_Scalar_Typ
     (T                                                  : Scalar_Typ'Class;
      Scalar_Typ_Decl_Template, Scalar_Typ_Init_Template : String;
      Scalar_Typ_Decl                                    :
        out Unbounded_String;
      Scalar_Typ_Init                                    :
        out Unbounded_String;
      Is_Top_Level_Generic                               : Boolean := False);

   ---------------------------------
   -- Collect_Info_For_Scalar_Typ --
   ---------------------------------

   procedure Collect_Info_For_Scalar_Typ
     (T                                                  : Scalar_Typ'Class;
      Scalar_Typ_Decl_Template, Scalar_Typ_Init_Template : String;
      Scalar_Typ_Decl                                    :
        out Unbounded_String;
      Scalar_Typ_Init                                    :
        out Unbounded_String;
      Is_Top_Level_Generic                               : Boolean := False)
   is
      Ty_Prefix : constant String := T.Slug (Is_Top_Level_Generic);
      Ty_Name   : constant String :=
        Esc
          (T.FQN (No_Std => True, Top_Level_Generic => Is_Top_Level_Generic));
      Assocs    : Translate_Set;
   begin
      Insert (Assocs, Assoc ("TY_NAME", Ty_Name));
      Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));
      if T in Signed_Int_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Signed_Int_Typ"));
      elsif T in Mod_Int_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Mod_Int_Typ"));
      elsif T in Char_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Char_Typ"));
      elsif T in Bool_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Bool_Typ"));
      elsif T in Other_Enum_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Other_Enum_Typ"));
      elsif T in Float_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Float_Typ"));
      elsif T in Ordinary_Fixed_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Ordinary_Fixed_Typ"));
      elsif T in Decimal_Fixed_Typ'Class then
         Insert (Assocs, Assoc ("SCALAR_TYP", "Decimal_Fixed_Typ"));
      else
         raise Program_Error;
      end if;

      Scalar_Typ_Decl := Parse (Scalar_Typ_Decl_Template, Assocs);
      Scalar_Typ_Init := Parse (Scalar_Typ_Init_Template, Assocs);
   end Collect_Info_For_Scalar_Typ;

   ------------------------------------------
   -- Generate_Type_Representation_For_Typ --
   ------------------------------------------

   procedure Generate_Type_Representation_For_Typ
     (F_Spec, F_Body     : File_Type with Unreferenced;
      Ctx                : TGen.Libgen.Libgen_Context;
      Typ                : TGen.Types.Typ'Class;
      Templates_Root_Dir : String;
      Strategies         : FQN_To_Parsed_Strat_Maps.Map;
      Init_Package_Code  : in out Tag;
      Is_Top_Level_Gen   : Boolean := False)
   is
      TRD : constant String :=
        Templates_Root_Dir
        & GNAT.OS_Lib.Directory_Separator
        & "type_representation_templates"
        & GNAT.OS_Lib.Directory_Separator;

      package Templates is new TGen.Templates (TRD);
      use Templates.Type_Representation;

      Ty_Prefix          : constant String := Typ.Slug (Is_Top_Level_Gen);
      Ty_Name            : constant String :=
        Esc (Typ.FQN (No_Std => True, Top_Level_Generic => Is_Top_Level_Gen));
      Anonymous_Ty_Index : Positive := 1;
      Variant_Index      : Positive := 1;

      Assocs : Translate_Set;

      procedure Collect_Info_For_Component
        (T                   : TGen.Types.Typ'Class;
         Anonymous_Decl      : out Unbounded_String;
         Anonymous_Init      : out Unbounded_String;
         Component_Ty_Prefix : out Unbounded_String);
      --  Return the specification and initialization for the instantiation of
      --  a component.

      procedure Collect_Info_For_Variant
        (Variant      : Variant_Part_Acc;
         Ty_Prefix    : String;
         Variant_Decl : in out Unbounded_String;
         Variant_Init : in out Unbounded_String);
      --  Return the specification and initialization for a variant. Note that
      --  Variant_Index is incremented every time this procedure is called.

      procedure Collect_Info_For_Record
        (T               : Record_Typ'Class;
         Record_Typ_Decl : out Unbounded_String;
         Record_Typ_Init : out Unbounded_String);
      --  Return the specification and initialization for a record type

      procedure Collect_Info_For_Array
        (T              : Array_Typ'Class;
         Array_Typ_Decl : out Unbounded_String;
         Array_Typ_Init : out Unbounded_String);
      --  Return the specification and initialization for an array type

      --------------------------------
      -- Collect_Info_For_Component --
      --------------------------------

      procedure Collect_Info_For_Component
        (T                   : TGen.Types.Typ'Class;
         Anonymous_Decl      : out Unbounded_String;
         Anonymous_Init      : out Unbounded_String;
         Component_Ty_Prefix : out Unbounded_String)
      is
         Is_Top_Level_Gen : constant Boolean :=
           not T.Package_Name.Is_Empty
           and then Ctx.Pack_Is_Top_Level_Instantiation (T.Package_Name);
      begin
         --  We have to collect anonymous types there and instantiate a new
         --  prefix for them. It will be the type name + the anonymous type
         --  index.

         if T in Anonymous_Typ'Class then
            declare
               Ano_Typ : Anonymous_Typ'Class := Anonymous_Typ'Class (T);
            begin
               Ano_Typ.Name :=
                 Ada_Identifier_Vectors."&"
                   (Typ.Name,
                    Ada_Identifier
                      (Ada.Strings.Unbounded.To_Unbounded_String
                         (Trim (Anonymous_Ty_Index'Image))));
               Collect_Info_For_Anonymous_Typ
                 (Ano_Typ,
                  Anonymous_Typ_Decl_Template,
                  Anonymous_Typ_Init_Template,
                  Constraint_Decl_Template,
                  Constraint_Init_Template,
                  Anonymous_Decl,
                  Anonymous_Init,
                  Is_Top_Level_Gen);
               Component_Ty_Prefix := +Ano_Typ.Slug (Is_Top_Level_Gen);
               Anonymous_Ty_Index := Anonymous_Ty_Index + 1;

               --  Add the type reference declaration in the body declarative
               --  part, as the only uses of this type reference will be
               --  accessed through the parent record type ref.

               Anonymous_Decl :=
                 Anonymous_Decl
                 & (+Ano_Typ.Slug (Is_Top_Level_Gen))
                 & "_Typ_Ref : TGen.Types.Typ_Access;";
            end;
         else
            Component_Ty_Prefix := +T.Slug (Is_Top_Level_Gen);
         end if;
      end Collect_Info_For_Component;

      ------------------------------
      -- Collect_Info_For_Variant --
      ------------------------------

      procedure Collect_Info_For_Variant
        (Variant      : Variant_Part_Acc;
         Ty_Prefix    : String;
         Variant_Decl : in out Unbounded_String;
         Variant_Init : in out Unbounded_String)
      is
         Assocs : Translate_Set;
         I      : Positive := 1;

      begin
         if Variant = null then
            return;
         end if;

         Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));
         Insert (Assocs, Assoc ("VARIANT_NUMBER", Variant_Index));
         Variant_Index := Variant_Index + 1;

         for Choice of Variant.Variant_Choices loop
            declare
               Low_Bounds, High_Bounds                  : Vector_Tag;
               Comp_Names, Comp_Types_Prefix            : Vector_Tag;
               Anonymous_Typ_Inits, Anonymous_Typ_Decls : Unbounded_String;
            begin
               --  Fill in the component for this variant

               for Comp in Choice.Components.Iterate loop
                  declare
                     use Component_Maps;
                     Component_Name                         :
                       constant Unbounded_String := Key (Comp);
                     Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
                     Component_Ty_Prefix                    : Unbounded_String;
                  begin
                     Collect_Info_For_Component
                       (Element (Comp).all,
                        Anonymous_Typ_Decl,
                        Anonymous_Typ_Init,
                        Component_Ty_Prefix);
                     Anonymous_Typ_Inits :=
                       Anonymous_Typ_Inits & Anonymous_Typ_Init;
                     Anonymous_Typ_Decls :=
                       Anonymous_Typ_Decls & Anonymous_Typ_Decl;
                     Comp_Names := Comp_Names & Component_Name;
                     Comp_Types_Prefix :=
                       Comp_Types_Prefix & Component_Ty_Prefix;
                  end;
               end loop;

               --  Insert the anonymous type declarations

               Insert
                 (Assocs, Assoc ("ANONYMOUS_TYP_SPEC", Anonymous_Typ_Decls));
               Insert
                 (Assocs, Assoc ("ANONYMOUS_TYP_INIT", Anonymous_Typ_Inits));
               Insert (Assocs, Assoc ("COMP_NAME", Comp_Names));
               Insert (Assocs, Assoc ("COMP_TYP_PREFIX", Comp_Types_Prefix));

               --  Fill in the alternative set

               for Alt of Choice.Alt_Set loop
                  Low_Bounds := Low_Bounds & Big_Int.To_String (Alt.Min);
                  High_Bounds := High_Bounds & Big_Int.To_String (Alt.Max);
               end loop;

               Insert (Assocs, Assoc ("LOW_BOUND", Low_Bounds));
               Insert (Assocs, Assoc ("HIGH_BOUND", High_Bounds));

               if Choice.Variant /= null then
                  Insert (Assocs, Assoc ("HAS_VARIANT", True));

                  --  The nested variant number is this Variant_Index + 1 as we
                  --  do an infix traversal of the variant tree. Make sure
                  --  to save its value before calling Collect_Info_For_Variant
                  --  as recursive calls will modify the Variant_Index.

                  Insert
                    (Assocs, Assoc ("NESTED_VARIANT_NUMBER", Variant_Index));
                  Collect_Info_For_Variant
                    (Choice.Variant, Ty_Prefix, Variant_Decl, Variant_Init);
               else
                  Insert (Assocs, Assoc ("HAS_VARIANT", False));
               end if;

               Insert (Assocs, Assoc ("VARIANT_CHOICE_NUMBER", I));

               Variant_Decl :=
                 Variant_Decl
                 & String'(Parse (Variant_Choice_Decl_Template, Assocs));
               Variant_Init :=
                 Variant_Init
                 & String'(Parse (Variant_Choice_Init_Template, Assocs));
               I := I + 1;
            end;
         end loop;

         --  Now that the variant choices have been handled, we can generate
         --  code for the variant

         declare
            Variant_Choice_Number : Vector_Tag;
         begin
            I := 1;
            for Choice of Variant.Variant_Choices loop
               Variant_Choice_Number := Variant_Choice_Number & I;
               I := I + 1;
            end loop;
            Insert
              (Assocs, Assoc ("VARIANT_CHOICE_NUMBER", Variant_Choice_Number));
            Insert (Assocs, Assoc ("DISCR_NAME", Variant.Discr_Name));
            Variant_Decl :=
              Variant_Decl & String'(Parse (Variant_Decl_Template, Assocs));
            Variant_Init :=
              Variant_Init & String'(Parse (Variant_Init_Template, Assocs));
         end;
      end Collect_Info_For_Variant;

      -----------------------------
      -- Collect_Info_For_Record --
      -----------------------------

      procedure Collect_Info_For_Record
        (T               : Record_Typ'Class;
         Record_Typ_Decl : out Unbounded_String;
         Record_Typ_Init : out Unbounded_String)
      is
         Discr_Names : Vector_Tag;
         Discr_Types : Vector_Tag;
         Comp_Names  : Vector_Tag;
         Comp_Types  : Vector_Tag;

         Anonymous_Typ_Inits, Anonymous_Typ_Decls : Unbounded_String;
      begin
         if T in Discriminated_Record_Typ'Class then
            declare
               Disc_T                     :
                 constant Discriminated_Record_Typ'Class :=
                   Discriminated_Record_Typ'Class (T);
               Variant_Decl, Variant_Init : Unbounded_String;
            begin
               Insert
                 (Assocs, Assoc ("RECORD_TYP", "Discriminated_Record_Typ"));
               Insert (Assocs, Assoc ("HAS_CONSTRAINTS", Disc_T.Constrained));
               Insert (Assocs, Assoc ("MUTABLE", Disc_T.Mutable));

               --  Start off by encoding the constraints

               if Disc_T.Constrained then
                  declare
                     Constraint_Decl, Constraint_Init : Unbounded_String;
                  begin
                     Collect_Info_For_Constraint
                       (Ty_Prefix,
                        Discriminant_Constraints'
                          (Constraint_Map => Disc_T.Discriminant_Constraint),
                        Constraint_Decl_Template,
                        Constraint_Init_Template,
                        Constraint_Decl,
                        Constraint_Init);
                     Insert
                       (Assocs, Assoc ("CONSTRAINT_SPEC", Constraint_Decl));
                     Insert
                       (Assocs, Assoc ("CONSTRAINT_INIT", Constraint_Init));
                  end;
               end if;

               --  Then encode the variants

               if Disc_T.Variant /= null then
                  Collect_Info_For_Variant
                    (Variant      => Disc_T.Variant,
                     Ty_Prefix    => Ty_Prefix,
                     Variant_Decl => Variant_Decl,
                     Variant_Init => Variant_Init);

                  Insert (Assocs, Assoc ("HAS_VARIANT_PART", True));
                  Insert (Assocs, Assoc ("VARIANT_SPEC", Variant_Decl));
                  Insert (Assocs, Assoc ("VARIANT_INIT", Variant_Init));
                  Insert (Assocs, Assoc ("VARIANT_NUMBER", 1));
               end if;

               for Cur in Disc_T.Discriminant_Types.Iterate loop
                  declare
                     use Component_Maps;
                     Discr_Name                             :
                       constant Unbounded_String := Key (Cur);
                     Discr_Ty_Prefix                        : Unbounded_String;
                     Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
                  begin
                     Collect_Info_For_Component
                       (Element (Cur).all,
                        Anonymous_Typ_Decl,
                        Anonymous_Typ_Init,
                        Discr_Ty_Prefix);
                     Anonymous_Typ_Inits :=
                       Anonymous_Typ_Inits & Anonymous_Typ_Init;
                     Anonymous_Typ_Decls :=
                       Anonymous_Typ_Decls & Anonymous_Typ_Decl;
                     Discr_Names := Discr_Names & Discr_Name;
                     Discr_Types := Discr_Types & Discr_Ty_Prefix;
                  end;
               end loop;
            end;
         elsif T in Nondiscriminated_Record_Typ'Class then
            Insert
              (Assocs, Assoc ("RECORD_TYP", "Nondiscriminated_Record_Typ"));
         elsif T in Function_Typ'Class then
            Insert (Assocs, Assoc ("RECORD_TYP", "Function_Typ"));
         end if;

         --  Common processing for (discriminated)? record / function
         --  types

         for Cur in T.Component_Types.Iterate loop
            declare
               use Component_Maps;
               Comp_Name                              :
                 constant Unbounded_String := Key (Cur);
               Comp_Ty_Prefix                         : Unbounded_String;
               Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
            begin
               Collect_Info_For_Component
                 (Element (Cur).all,
                  Anonymous_Typ_Decl,
                  Anonymous_Typ_Init,
                  Comp_Ty_Prefix);
               Anonymous_Typ_Inits := Anonymous_Typ_Inits & Anonymous_Typ_Init;
               Anonymous_Typ_Decls := Anonymous_Typ_Decls & Anonymous_Typ_Decl;
               Comp_Names := Comp_Names & Comp_Name;
               Comp_Types := Comp_Types & Comp_Ty_Prefix;
            end;
         end loop;

         --  If this type is a function type, also get the order of the
         --  components.

         if T in Function_Typ'Class then
            declare
               Function_Type : constant Function_Typ := Function_Typ (T);

               Comp_Names_Ordered : Vector_Tag;
               Global_Names       : Vector_Tag;
               Global_Types       : Vector_Tag;
            begin
               for Comp_Name of Function_Type.Param_Order loop
                  Comp_Names_Ordered := Comp_Names_Ordered & Comp_Name;
               end loop;
               Insert (Assocs, Assoc ("COMP_NAME_ORD", Comp_Names_Ordered));

               --  Deal with globals

               for Cur in Function_Type.Globals.Iterate loop
                  Global_Names := Global_Names & Component_Maps.Key (Cur);
                  Global_Types :=
                    Global_Types & Component_Maps.Element (Cur).all.Slug;
               end loop;
               Insert (Assocs, Assoc ("GLOBAL_NAME", Global_Names));
               Insert (Assocs, Assoc ("GLOBAL_TYPE", Global_Types));
            end;
         end if;

         --  Print the templates

         Insert (Assocs, Assoc ("ANONYMOUS_TYP_SPEC", Anonymous_Typ_Decls));
         Insert (Assocs, Assoc ("ANONYMOUS_TYP_INIT", Anonymous_Typ_Inits));
         Insert (Assocs, Assoc ("COMP_NAME", Comp_Names));
         Insert (Assocs, Assoc ("COMP_TYPE", Comp_Types));
         Insert (Assocs, Assoc ("DISCR_NAME", Discr_Names));
         Insert (Assocs, Assoc ("DISCR_TYPE", Discr_Types));
         Record_Typ_Decl := Parse (Record_Typ_Decl_Template, Assocs);
         if T in Function_Typ'Class then
            Record_Typ_Init := Parse (Function_Typ_Init_Template, Assocs);
         else
            Record_Typ_Init := Null_Unbounded_String;
         end if;
         Record_Typ_Init :=
           Record_Typ_Init
           & Unbounded_String'(Parse (Record_Typ_Init_Template, Assocs));

      end Collect_Info_For_Record;

      ----------------------------
      -- Collect_Info_For_Array --
      ----------------------------

      procedure Collect_Info_For_Array
        (T              : Array_Typ'Class;
         Array_Typ_Decl : out Unbounded_String;
         Array_Typ_Init : out Unbounded_String)
      is
         Index_Ty_Prefixes                        : Vector_Tag;
         Anonymous_Typ_Inits, Anonymous_Typ_Decls : Unbounded_String;
      begin
         --  Fill the number of dimension

         Insert (Assocs, Assoc ("NUM_DIMS", T.Num_Dims));

         --  Deal with the component type

         declare
            Component_Ty_Prefix                    : Unbounded_String;
            Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
         begin
            Collect_Info_For_Component
              (T.Component_Type.all,
               Anonymous_Typ_Decl,
               Anonymous_Typ_Init,
               Component_Ty_Prefix);
            Anonymous_Typ_Inits := Anonymous_Typ_Inits & Anonymous_Typ_Init;
            Anonymous_Typ_Decls := Anonymous_Typ_Decls & Anonymous_Typ_Decl;
            Insert
              (Assocs, Assoc ("COMPONENT_TY_PREFIX", Component_Ty_Prefix));
         end;

         --  Deal with the index types

         for Index_T of T.Index_Types loop
            declare
               Index_Ty_Prefix                        : Unbounded_String;
               Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
            begin
               Collect_Info_For_Component
                 (Index_T.all,
                  Anonymous_Typ_Init,
                  Anonymous_Typ_Decl,
                  Index_Ty_Prefix);
               Anonymous_Typ_Inits := Anonymous_Typ_Inits & Anonymous_Typ_Init;
               Anonymous_Typ_Decls := Anonymous_Typ_Decls & Anonymous_Typ_Decl;
               Index_Ty_Prefixes := Index_Ty_Prefixes & Index_Ty_Prefix;
            end;
         end loop;
         Insert (Assocs, Assoc ("INDEX_TY_PREFIX", Index_Ty_Prefixes));

         --  Done with anonymous types

         Insert (Assocs, Assoc ("ANONYMOUS_TYP_SPEC", Anonymous_Typ_Decls));
         Insert (Assocs, Assoc ("ANONYMOUS_TYP_INIT", Anonymous_Typ_Inits));

         --  Deal with index constraints for constrained arrays

         if T in Constrained_Array_Typ'Class then
            declare
               T_Const                          :
                 constant Constrained_Array_Typ'Class :=
                   Constrained_Array_Typ'Class (T);
               Constraint_Decl, Constraint_Init : Unbounded_String;
            begin
               Collect_Info_For_Constraint
                 (Ty_Prefix,
                  Index_Constraints'
                    (T_Const.Num_Dims, T_Const.Index_Constraints),
                  Constraint_Decl_Template,
                  Constraint_Init_Template,
                  Constraint_Decl,
                  Constraint_Init);
               Insert (Assocs, Assoc ("ARRAY_TYP", "Constrained_Array_Typ"));
               Insert (Assocs, Assoc ("CONSTRAINT_SPEC", Constraint_Decl));
               Insert (Assocs, Assoc ("CONSTRAINT_INIT", Constraint_Init));
            end;
         else
            pragma Assert (T in Unconstrained_Array_Typ'Class);
            Insert (Assocs, Assoc ("ARRAY_TYP", "Unconstrained_Array_Typ"));
         end if;
         Array_Typ_Decl := Parse (Array_Typ_Decl_Template, Assocs);
         Array_Typ_Init := Parse (Array_Typ_Init_Template, Assocs);

      end Collect_Info_For_Array;

   begin
      Insert (Assocs, Assoc ("TY_NAME", Ty_Name));
      Insert (Assocs, Assoc ("TY_PREFIX", Ty_Prefix));
      Insert
        (Assocs, Assoc ("HAS_STATIC_PREDICATE", Typ.Has_Static_Predicate));

      --  If we have a strategy for the type, start off by generating it

      if Strategies.Contains (+Ty_Name) then
         declare
            Parsed_Strat : constant Parsed_Strategy :=
              Strategies.Element (+Ty_Name);
         begin
            if Typ.Kind /= Instance_Kind then
               raise Program_Error;
            end if;
            if Parsed_Strat.Kind = Predefined then
               --  TODO
               raise Program_Error;
            else
               --  custom strat

               declare
                  --  Original type must be an anonymous named type

                  To_JSON_Fname : constant String :=
                    TGen.Marshalling.Output_Fname_For_Typ
                      (Instance_Typ'Class (Typ).Orig_Typ.all.Name);
               begin
                  Insert
                    (Assocs,
                     Assoc ("FUNCTION_NAME", Parsed_Strat.Generate_Name));
                  Insert (Assocs, Assoc ("TO_JSON_FUNCTION", To_JSON_Fname));
                  Put_Line
                    (F_Spec, Parse (Custom_Strat_Spec_Template, Assocs));
                  Put_Line
                    (F_Body, Parse (Custom_Strat_Body_Template, Assocs));
               end;
            end if;
         end;
      end if;

      if Typ in Record_Typ'Class then
         declare
            Record_Typ_Init, Record_Typ_Decl : Unbounded_String;
         begin
            Collect_Info_For_Record
              (Record_Typ'Class (Typ), Record_Typ_Decl, Record_Typ_Init);
            Put_Line
              (F_Spec,
               "   " & Ty_Prefix & "_Typ_Ref : TGen.Types.Typ_Access;");
            Put_Line (F_Body, +Record_Typ_Decl);
            Init_Package_Code := Init_Package_Code & Record_Typ_Init;
         end;

      elsif Typ in Anonymous_Typ'Class then
         declare
            Anonymous_Typ_Init, Anonymous_Typ_Decl : Unbounded_String;
         begin
            Collect_Info_For_Anonymous_Typ
              (Anonymous_Typ'Class (Typ),
               Anonymous_Typ_Decl_Template,
               Anonymous_Typ_Init_Template,
               Constraint_Decl_Template,
               Constraint_Init_Template,
               Anonymous_Typ_Decl,
               Anonymous_Typ_Init,
               Is_Top_Level_Gen);
            Put_Line
              (F_Spec,
               "   "
               & Anonymous_Typ'Class (Typ).Slug
               & "_Typ_Ref : Typ_Access;");
            Put_Line (F_Body, +Anonymous_Typ_Decl);
            Init_Package_Code := Init_Package_Code & Anonymous_Typ_Init;
         end;

      elsif Typ in Instance_Typ'Class then
         declare
            Instance_Typ_Init, Instance_Typ_Decl : Unbounded_String;
         begin
            Collect_Info_For_Instance_Typ
              (Instance_Typ'Class (Typ),
               Instance_Decl_Template,
               Instance_Init_Template,
               Instance_Typ_Decl,
               Instance_Typ_Init,
               Is_Top_Level_Gen);
            Put_Line
              (F_Spec,
               "   " & Ty_Prefix & "_Typ_Ref : TGen.Types.Typ_Access;");
            Put_Line (F_Body, +Instance_Typ_Decl);
            Init_Package_Code := Init_Package_Code & Instance_Typ_Init;
         end;

      elsif Typ in Array_Typ'Class then
         declare
            Array_Typ_Init, Array_Typ_Decl : Unbounded_String;
         begin
            Collect_Info_For_Array
              (Array_Typ'Class (Typ), Array_Typ_Decl, Array_Typ_Init);
            Put_Line
              (F_Spec,
               "   " & Ty_Prefix & "_Typ_Ref : TGen.Types.Typ_Access;");
            Put_Line (F_Body, +Array_Typ_Decl);
            Init_Package_Code := Init_Package_Code & Array_Typ_Init;
         end;

      elsif Typ in Scalar_Typ'Class then
         if Typ in Signed_Int_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Signed_Int_Typ"));
         elsif Typ in Mod_Int_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Mod_Int_Typ"));
         elsif Typ in Other_Enum_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Char_Typ"));
         elsif Typ in Other_Enum_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Other_Enum_Typ"));
         elsif Typ in Float_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Float_Typ"));
         elsif Typ in Ordinary_Fixed_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Ordinary_Fixed_Typ"));
         elsif Typ in Decimal_Fixed_Typ'Class then
            Insert (Assocs, Assoc ("SCALAR_TYP", "Decimal_Fixed_Typ"));
         end if;

         declare
            Scalar_Typ_Decl, Scalar_Typ_Init : Unbounded_String;
         begin
            Collect_Info_For_Scalar_Typ
              (Scalar_Typ'Class (Typ),
               Scalar_Typ_Decl_Template,
               Scalar_Typ_Init_Template,
               Scalar_Typ_Decl,
               Scalar_Typ_Init,
               Is_Top_Level_Gen);
            Put_Line
              (F_Spec,
               "   " & Ty_Prefix & "_Typ_Ref : TGen.Types.Typ_Access;");
            Put_Line (F_Body, +Scalar_Typ_Decl);
            Init_Package_Code := Init_Package_Code & Scalar_Typ_Init;
         end;

      else
         raise Program_Error;
      end if;

   end Generate_Type_Representation_For_Typ;

end TGen.Type_Representation;
