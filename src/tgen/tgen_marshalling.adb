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

with Ada.Containers;               use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Text_IO;                  use Ada.Text_IO;

with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;            use Libadalang.Common;
with Libadalang.Helpers;

with Templates_Parser;             use Templates_Parser;

with TGen.Types.Array_Types;       use TGen.Types.Array_Types;
with TGen.Types.Enum_Types;        use TGen.Types.Enum_Types;
with TGen.Types.Int_Types;         use TGen.Types.Int_Types;
with TGen.Types.Real_Types;        use TGen.Types.Real_Types;
with TGen.Types.Record_Types;      use TGen.Types.Record_Types;
with TGen.Types;                   use TGen.Types;
with TGen.Types.Translation;       use TGen.Types.Translation;

procedure TGen_Marshalling is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   use type LALCO.Ada_Node_Kind_Type;

   Global_Prefix  : constant String := "TAGAda_Marshalling";

   --------------------
   -- Template Files --
   --------------------

   Template_Folder            : constant String :=
     "tgen/marshalling_templates/";
   Header_Body_Template       : constant String :=
     Template_Folder & "header_body.tmplt";
   Header_Spec_Template       : constant String :=
     Template_Folder & "header_spec.tmplt";
   In_Out_Body_Template       : constant String :=
     Template_Folder & "in_out_body.tmplt";
   In_Out_Spec_Template       : constant String :=
     Template_Folder & "in_out_spec.tmplt";
   Array_Read_Write_Template  : constant String :=
     Template_Folder & "array_read_write.tmplt";
   Record_Read_Write_Template  : constant String :=
     Template_Folder & "record_read_write.tmplt";
   Scalar_Read_Write_Template : constant String :=
     Template_Folder & "scalar_read_write.tmplt";
   Variant_Part_Template      : constant String :=
     Template_Folder & "variant_part.tmplt";

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Prefix_For_Typ
     (Ty_Name : String; For_Base, For_Header : Boolean := False) return String
   is
     (Global_Prefix & "_" & Ty_Name
      & (if For_Base then "_Base" else "")
      & (if For_Header then "_Header" else ""));
   --  Construct a prefix that will be shared by all entities generated for a
   --  given type.

   function Hash_Defining_Name
     (Node : LAL.Defining_Name) return Ada.Containers.Hash_Type is
       (Node.As_Ada_Node.Hash);

   package Type_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => SP.Ref,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => LAL."=",
      "="             => SP."=");

   package Name_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => LAL.Defining_Name,
      Hash                => Hash_Defining_Name,
      Equivalent_Elements => LAL."=",
      "="                 => LAL."=");

   function Is_Supported_Type (Typ : TGen.Types.Typ'Class) return Boolean;
   --  Return True for types which are currently supported by the prototype

   function Needs_Header (Typ : TGen.Types.Typ'Class) return Boolean;
   --  Retyrn True for type which have an immutable part (bounds of
   --  unconstrained arrays, and discriminants of unconstrained types with
   --  immutable discriminants).

   Used_Types : Type_Maps.Map;

   function Get_All_Types
     (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
   --  If Node is a subprogram declaration, collect the type of its parameters
   --  in Used_Types.

   function Create_Tag_For_Constraints
     (Typ : TGen.Types.Typ'Class; Header_Name : String) return Tag;
   --  Return as a tag the constraints from the header Header_Name for type
   --  Typ.

   function Create_Tag_For_Intervals
     (Intervals : Alternatives_Set; Typ : TGen.Types.Typ'Class) return Tag;
   --  Return as a tag the choices represented by a set of intervals

   procedure Generate_Header_For_Typ
     (F_Spec, F_Body : File_Type; Typ : TGen.Types.Typ'Class);
   --  Generate a type plus some marshalling and unmarshalling functions for
   --  the immutable part of a type.

   procedure Generate_Base_Functions_For_Typ
     (F : File_Type; Typ : TGen.Types.Typ'Class; For_Base : Boolean := False)
   with Pre => (if For_Base then Typ in Scalar_Typ'Class);
   --  Generate base marshalling and unmarshalling functions for Typ. Store
   --  types for which the base generating function have already been generated
   --  to avoid generating them twice.
   --  If For_Base is True, generate the functions for Typ'Base.

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body : File_Type; Typ : TGen.Types.Typ'Class);
   --  Generate marshalling and unmarshalling functions for Typ

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit);
   --  From a unit containing a single package, go through the subprogram
   --  declarations in the package, detect those for which we can generate the
   --  marshalling and unmarshalling functions, and do the generation.
   --  This creates a child TAGAda_Parsing for the input unit containing a
   --  pair of procedures Input and Output for each type occurring as a
   --  parameter type in a supported subprograms of the unit.

   package App is new Libadalang.Helpers.App
     (Name         => "Marshalling",
      Description  => "Generation of marshalling and unmarshalling functions",
      Process_Unit => Process_Unit);

   --------------------------------
   -- Create_Tag_For_Constraints --
   --------------------------------

   function Create_Tag_For_Constraints
     (Typ : TGen.Types.Typ'Class; Header_Name : String) return Tag
   is
      Constraints_Tag : Tag;

   begin
      --  If Type does not need a header, there is nothing to do

      if not Needs_Header (Typ) then
         null;

      --  For an array constraint, we generate:
      --
      --    (Header_Name.First_1 .. Header_Name.Last_1, ..)

      elsif Typ in Unconstrained_Array_Typ'Class then
         declare
            Bound_Constraint_Tmplt : constant String :=
              "@_HEADER_NAME_@.First_@_DIM_@ .. @_HEADER_NAME_@.Last_@_DIM_@";
         begin
            for I in Unconstrained_Array_Typ'Class (Typ).Index_Types'Range loop
               Constraints_Tag := Constraints_Tag
                 & Translate
                 (Template     => Bound_Constraint_Tmplt,
                  Translations => (1 => Assoc ("HEADER_NAME", Header_Name),
                                   2 => Assoc ("DIM", I)));
            end loop;
         end;

      --  Discriminants are not supported yet

      else
         declare
            D_Typ                  : Discriminated_Record_Typ'Class renames
              Discriminated_Record_Typ'Class (Typ);
            Discr_Constraint_Tmplt : constant String :=
              "@_COMP_NAME_@ => @_HEADER_NAME_@.@_COMP_NAME_@";
         begin
            for Cu in D_Typ.Discriminant_Types.Iterate loop
               declare
                  Comp_Name : constant String :=
                    Langkit_Support.Text.Image
                      (Component_Maps.Key (Cu).Text);
               begin
                  Constraints_Tag := Constraints_Tag
                    & Translate
                    (Template     => Discr_Constraint_Tmplt,
                     Translations => (1 => Assoc ("HEADER_NAME", Header_Name),
                                      2 => Assoc ("COMP_NAME", Comp_Name)));
               end;
            end loop;
         end;
      end if;

      return Constraints_Tag;
   end Create_Tag_For_Constraints;

   ------------------------------
   -- Create_Tag_For_Intervals --
   ------------------------------

   function Create_Tag_For_Intervals
     (Intervals : Alternatives_Set; Typ : TGen.Types.Typ'Class) return Tag
   is
      Ty_Name : constant String :=
        Langkit_Support.Text.Image (Typ.Name.Text);

      function String_Value (V : TGen.Types.Big_Integer) return String;
      --  Get a string for the value at position V in Typ

      ------------------
      -- String_Value --
      ------------------

      function String_Value (V : TGen.Types.Big_Integer) return String is
      begin
         --  For now, generate Typ'Val (V) for enumerations. We might be able
         --  to use Lit_Image when it is more reliable.

         if Typ in Enum_Typ'Class then
            return Ty_Name & "'Val (" & Trim (To_String (V), Left) & ")";
         else
            return Trim (To_String (V), Left);
         end if;
      end String_Value;

      Choices_Tag : Tag;
   begin
      for Int of Intervals loop
         if Int.Min = Int.Max then
            Choices_Tag := Choices_Tag & String_Value (Int.Min);
         else
            Choices_Tag := Choices_Tag &
            (String_Value (Int.Min) & " .. " & String_Value (Int.Max));
         end if;
      end loop;

      Set_Separator (Choices_Tag, " | ");
      return Choices_Tag;
   end Create_Tag_For_Intervals;

   -------------------------------------
   -- Generate_Base_Functions_For_Typ --
   -------------------------------------

   Base_Seen    : Name_Sets.Set;
   Already_Seen : Name_Sets.Set;

   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, String);

   procedure Generate_Base_Functions_For_Typ
     (F : File_Type; Typ : TGen.Types.Typ'Class; For_Base : Boolean := False)
   is
      B_Name    : constant String := Langkit_Support.Text.Image
        (Typ.Name.Text);
      Ty_Prefix : constant String := Prefix_For_Typ (B_Name, For_Base);
      Ty_Name   : constant String :=
        (if For_Base then B_Name & "'Base" else B_Name);

      procedure Collect_Info_For_Components
        (Components    : Component_Maps.Map;
         Comp_Name_Tag : in out Vector_Tag;
         Comp_Pref_Tag : in out Vector_Tag);
         --  Go over the components in Components and fill the associations for
         --  the components names and prefixes in the tags given as parameters.
         --  Along the way, generate base functions for the component types.

      function Variant_Part_To_String
        (V             : Variant_Part;
         Discriminants : Component_Maps.Map;
         Spacing       : Natural) return String;
      --  Recursive function which instanciates the variant part template to
      --  create a string for a variant part V.

      ---------------------------------
      -- Collect_Info_For_Components --
      ---------------------------------

      procedure Collect_Info_For_Components
        (Components    : Component_Maps.Map;
         Comp_Name_Tag : in out Vector_Tag;
         Comp_Pref_Tag : in out Vector_Tag)
      is
      begin
         --  Go over the record component to fill the associations

         for Cu in Components.Iterate loop
            Generate_Base_Functions_For_Typ
              (F, Component_Maps.Element (Cu).Get);

            declare
               Comp_Name   : constant String :=
                 Langkit_Support.Text.Image
                   (Component_Maps.Key (Cu).Text);
               Comp_Prefix : constant String :=
                 Prefix_For_Typ
                   (Langkit_Support.Text.Image
                      (Component_Maps.Element (Cu).Get.Name.Text));
            begin
               Comp_Name_Tag := Comp_Name_Tag & Comp_Name;
               Comp_Pref_Tag := Comp_Pref_Tag & Comp_Prefix;
            end;
         end loop;
      end Collect_Info_For_Components;

      ----------------------------
      -- Variant_Part_To_String --
      ----------------------------

      function Variant_Part_To_String
        (V             : Variant_Part;
         Discriminants : Component_Maps.Map;
         Spacing       : Natural) return String
      is
         Discr_Name : constant String := Langkit_Support.Text.Image
           (V.Discr_Name.Text);
         Discr_Typ  : constant TGen.Types.Typ'Class :=
           Discriminants (V.Discr_Name).Get;

         Choices_Tag      : Matrix_Tag;
         Comp_Pref_Tag    : Matrix_Tag;
         Comp_Name_Tag    : Matrix_Tag;
         Variant_Part_Tag : Vector_Tag;
      begin
         for V_Choice of V.Variant_Choices loop
            Choices_Tag :=
              Choices_Tag & Create_Tag_For_Intervals
                (V_Choice.Alt_Set, Discr_Typ);

            declare
               Prefs : Tag;
               Names : Tag;
            begin
               Collect_Info_For_Components
                 (V_Choice.Components, Names, Prefs);
               Comp_Pref_Tag := Comp_Pref_Tag & Prefs;
               Comp_Name_Tag := Comp_Name_Tag & Names;
            end;

            if V_Choice.Variant = null then
               Variant_Part_Tag := Variant_Part_Tag & "";
            else
               declare
                  Variant_String : constant String :=
                    Variant_Part_To_String
                      (V_Choice.Variant.all, Discriminants, Spacing + 5);
               begin
                  Variant_Part_Tag := Variant_Part_Tag & Variant_String;
               end;
            end if;
         end loop;

         declare
            Assocs       : constant Translate_Table :=
              (1 => Assoc ("OBJECT_NAME", "@_OBJECT_NAME_@"),
               2 => Assoc ("DISCR_NAME", Discr_Name),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("CHOICES", Choices_Tag),
               5 => Assoc ("COMP_PREFIX", Comp_Pref_Tag),
               6 => Assoc ("COMP_NAME", Comp_Name_Tag),
               7 => Assoc ("VARIANT_PART", Variant_Part_Tag),
               8 => Assoc ("SPACING", (1 .. Spacing => ' ')),
               9 => Assoc ("ACTION", "@_ACTION_@"));

         begin
            return Parse (Variant_Part_Template, Assocs);
         end;
      end Variant_Part_To_String;

   begin
      --  Check for inclusion in the general map or in the map for base types

      if For_Base then
         if Base_Seen.Contains (Typ.Name) then
            return;
         end if;
         Base_Seen.Include (Typ.Name);
      else
         if Already_Seen.Contains (Typ.Name) then
            return;
         end if;
         Already_Seen.Include (Typ.Name);
      end if;

      --  Scalar types: we use clones

      if Typ in Scalar_Typ'Class then
         declare
            Generic_Pack : constant String := "TAGAda_Marshalling_Lib";
            Generic_Name : constant String :=
              (if Typ in Discrete_Typ'Class then "Read_Write_Discrete"
               elsif Typ in Float_Typ'Class then "Read_Write_Float"
               elsif Typ in Ordinary_Fixed_Typ'Class
               then "Read_Write_Ordinary_Fixed"
               else "Read_Write_Decimal_Fixed");
            Assocs       : constant Translate_Table :=
              (1 => Assoc ("TY_NAME", Ty_Name),
               2 => Assoc ("TY_PREFIX", Ty_Prefix),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("GENERIC_PACK", Generic_Pack),
               5 => Assoc ("GENERIC_NAME", Generic_Name),
               6 => Assoc ("IS_DISCRETE", Typ in Discrete_Typ'Class));

         begin
            Put_Line (F, Parse (Scalar_Read_Write_Template, Assocs));
            New_Line (F);
         end;

      --  Array types: generate a loop

      elsif Typ in Constrained_Array_Typ'Class
                 | Unconstrained_Array_Typ'Class
      then
         declare
            Comp_Ty     : constant TGen.Types.SP.Ref :=
              (if Typ in Constrained_Array_Typ'Class
               then Constrained_Array_Typ'Class (Typ).Component_Type
               else Unconstrained_Array_Typ'Class (Typ).Component_Type);
            Comp_Prefix : constant String :=
              Prefix_For_Typ
                (Langkit_Support.Text.Image (Comp_Ty.Get.Name.Text));
            Assocs      : constant Translate_Table :=
              (1 => Assoc ("TY_NAME", Ty_Name),
               2 => Assoc ("TY_PREFIX", Ty_Prefix),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("COMP_PREFIX", Comp_Prefix));

         begin
            --  Generate the base function of the component

            Generate_Base_Functions_For_Typ (F, Comp_Ty.Get);

            Put_Line (F, Parse (Array_Read_Write_Template, Assocs));
            New_Line (F);
         end;

      --  Record types: generate a call per component

      else
         pragma Assert (Typ in Record_Typ'Class);

         declare
            Comp_Name_Tag : Tag;
            Comp_Pref_Tag : Tag;

         begin
            Collect_Info_For_Components
              (Record_Typ'Class (Typ).Component_Types,
               Comp_Name_Tag,
               Comp_Pref_Tag);

            declare
               Variant_Part  : constant String :=
                 (if Typ not in Discriminated_Record_Typ'Class
                  or else
                    Discriminated_Record_Typ'Class (Typ).Variant = null
                  then ""
                  else Variant_Part_To_String
                    (Discriminated_Record_Typ'Class (Typ).Variant.all,
                     Discriminated_Record_Typ'Class (Typ).Discriminant_Types,
                     5));
               Variant_Read  : constant String :=
                 Translate (Variant_Part,
                            (1 => Assoc ("OBJECT_NAME", Global_Prefix & "_V"),
                             2 => Assoc ("ACTION", "Read")));
               Variant_Write : constant String :=
                 Translate (Variant_Part,
                            (1 => Assoc ("OBJECT_NAME", Global_Prefix & "_V"),
                             2 => Assoc ("ACTION", "Write")));
               Assocs        : constant Translate_Table :=
                 (1 => Assoc ("TY_NAME", Ty_Name),
                  2 => Assoc ("TY_PREFIX", Ty_Prefix),
                  3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
                  4 => Assoc ("COMP_NAME", Comp_Name_Tag),
                  5 => Assoc ("COMP_PREFIX", Comp_Pref_Tag),
                  6 => Assoc ("VARIANT_READ", Variant_Read),
                  7 => Assoc ("VARIANT_WRITE", Variant_Write));

            begin
               Put_Line (F, Parse (Record_Read_Write_Template, Assocs));
               New_Line (F);
            end;
         end;
      end if;
   end Generate_Base_Functions_For_Typ;

   -----------------------------
   -- Generate_Header_For_Typ --
   -----------------------------

   procedure Generate_Header_For_Typ
     (F_Spec, F_Body : File_Type; Typ : TGen.Types.Typ'Class)
   is
      Ty_Name       : constant String := Langkit_Support.Text.Image
        (Typ.Name.Text);
      Ty_Prefix     : constant String :=
        Prefix_For_Typ (Ty_Name, For_Header => True);

      --  Table associations for the components of the header:
      --    * Comp_Name_Tag contains the name of the name of the component,
      --    * Comp_Typ_Tag contains its type,
      --    * Comp_Pref_Tag contains the prefix associated to the component
      --      type, and
      --    * Ada_Comp_Or_Attr contains the attribute or component access used
      --      to query the corresponding part in the Ada object.

      Comp_Name_Tag    : Tag;
      Comp_Typ_Tag     : Tag;
      Comp_Pref_Tag    : Tag;
      Ada_Comp_Or_Attr : Tag;

   begin
      --  Fill the tags for the components of the header and generate
      --  additional base functions if needed.

      if Typ in Unconstrained_Array_Typ'Class then
         declare
            U_Typ            : Unconstrained_Array_Typ'Class renames
              Unconstrained_Array_Typ'Class (Typ);
            First_Attr_Tmplt : constant String := "'First (@_DIM_@)";
            Last_Attr_Tmplt  : constant String := "'Last (@_DIM_@)";
            First_Name_Tmplt : constant String := "First_@_DIM_@";
            Last_Name_Tmplt  : constant String := "Last_@_DIM_@";
         begin
            for I in U_Typ.Index_Types'Range loop

               --  Generate base functions for the base type of the index types

               Generate_Base_Functions_For_Typ
                 (F_Body, U_Typ.Index_Types (I).Get, For_Base => True);

               --  Fill the association maps

               declare
                  Index_Type : constant String :=
                    Langkit_Support.Text.Image
                      (U_Typ.Index_Types (I).Get.Name.Text);
                  Type_Name  : constant String := Index_Type & "'Base";
                  Index_Pref : constant String :=
                    Prefix_For_Typ (Index_Type, For_Base => True);
                  Assocs     : constant Translate_Table :=
                    (1 => Assoc ("DIM", I));
               begin
                  if U_Typ.Num_Dims = 1 then
                     Ada_Comp_Or_Attr := Ada_Comp_Or_Attr & "'First";
                     Ada_Comp_Or_Attr := Ada_Comp_Or_Attr & "'Last";
                  else
                     Ada_Comp_Or_Attr := Ada_Comp_Or_Attr
                       & Translate (First_Attr_Tmplt, Assocs);
                     Ada_Comp_Or_Attr := Ada_Comp_Or_Attr
                       & Translate (Last_Attr_Tmplt, Assocs);
                  end if;

                  Comp_Name_Tag := Comp_Name_Tag
                    & Translate (First_Name_Tmplt, Assocs);
                  Comp_Name_Tag := Comp_Name_Tag
                    & Translate (Last_Name_Tmplt, Assocs);

                  Comp_Typ_Tag := Comp_Typ_Tag & Type_Name & Type_Name;
                  Comp_Pref_Tag := Comp_Pref_Tag & Index_Pref & Index_Pref;
               end;
            end loop;
         end;

      else
         declare
            D_Typ : Discriminated_Record_Typ'Class renames
              Discriminated_Record_Typ'Class (Typ);
         begin
            for Cu in D_Typ.Discriminant_Types.Iterate loop

               --  Generate base functions for the discriminant types

               Generate_Base_Functions_For_Typ
                 (F_Body, Component_Maps.Element (Cu).Get);

               --  Fill the association maps

               declare
                  Comp_Name : constant String :=
                    Langkit_Support.Text.Image
                      (Component_Maps.Key (Cu).Text);
                  Comp_Typ  : constant String :=
                    Langkit_Support.Text.Image
                      (Component_Maps.Element (Cu).Get.Name.Text);
                  Comp_Pref : constant String :=
                    Prefix_For_Typ (Comp_Typ);
               begin
                  Ada_Comp_Or_Attr := Ada_Comp_Or_Attr & ("." & Comp_Name);
                  Comp_Name_Tag := Comp_Name_Tag & Comp_Name;
                  Comp_Typ_Tag := Comp_Typ_Tag & Comp_Typ;
                  Comp_Pref_Tag := Comp_Pref_Tag & Comp_Pref;
               end;
            end loop;
         end;
      end if;

      --  Generate the header

      declare
         Assocs : constant Translate_Table :=
           (1 => Assoc ("TY_NAME", Ty_Name),
            2 => Assoc ("TY_PREFIX", Ty_Prefix),
            3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
            4 => Assoc ("COMP_NAME", Comp_Name_Tag),
            5 => Assoc ("COMP_TYP", Comp_Typ_Tag),
            6 => Assoc ("COMP_PREFIX", Comp_Pref_Tag),
            7 => Assoc ("ADA_COMP_OR_ATTR", Ada_Comp_Or_Attr));

      begin
         Put_Line (F_Spec, Parse (Header_Spec_Template, Assocs));
         New_Line (F_Spec);

         Put_Line (F_Body, Parse (Header_Body_Template, Assocs));
         New_Line (F_Body);
      end;
   end Generate_Header_For_Typ;

   --------------------------------------------
   -- Generate_Marshalling_Functions_For_Typ --
   --------------------------------------------

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body : File_Type; Typ : TGen.Types.Typ'Class)
   is
      Ty_Name       : constant String := Langkit_Support.Text.Image
        (Typ.Name.Text);
      Ty_Prefix     : constant String := Prefix_For_Typ (Ty_Name);
      Header_Prefix : constant String :=
        Prefix_For_Typ (Ty_Name, For_Header => True);
      Generic_Pack  : constant String := "TAGAda_Marshalling_Lib";
      Generic_Name  : constant String := "In_Out";
      Header_Name   : constant String :=
        Global_Prefix & "_H";
      Assocs        : constant Translate_Table :=
        (1 => Assoc ("TY_NAME", Ty_Name),
         2 => Assoc ("TY_PREFIX", Ty_Prefix),
         3 => Assoc ("GENERIC_PACK", Generic_Pack),
         4 => Assoc ("GENERIC_NAME", Generic_Name),
         5 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
         6 => Assoc ("NEEDS_HEADER", Needs_Header (Typ)),
         7 => Assoc ("HEADER_PREFIX", Header_Prefix),
         8 => Assoc ("HEADER_NAME", Header_Name),
         9 => Assoc
           ("CONSTRAINTS", Create_Tag_For_Constraints (Typ, Header_Name)));

   begin
      --  Generate the type and marshalling functions for the immutable part of
      --  Typ if any.

      if Needs_Header (Typ) then
         Generate_Header_For_Typ (F_Spec, F_Body, Typ);
      end if;

      Put_Line (F_Spec, Parse (In_Out_Spec_Template, Assocs));
      New_Line (F_Spec);

      Put_Line (F_Body, Parse (In_Out_Body_Template, Assocs));
      New_Line (F_Body);
   end Generate_Marshalling_Functions_For_Typ;

   -------------------
   -- Get_All_Types --
   -------------------

   function Get_All_Types (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
      Local_Types : Type_Maps.Map;
   begin
      if Node.Kind = LALCO.Ada_Subp_Decl then
         for Param of Node.As_Subp_Decl.F_Subp_Spec.P_Params loop
            declare
               Typ       : constant LAL.Type_Expr := Param.F_Type_Expr;
               Is_Anon   : constant Boolean :=
                 Typ.Kind = LALCO.Ada_Anonymous_Type;
               Typ_Decl  : LAL.Base_Type_Decl;
               Typ_Trans : Translation_Result;
            begin
               if not Is_Anon then
                  Typ_Decl := Typ.P_Designated_Type_Decl;
                  Typ_Trans := Translate (Typ_Decl);
               end if;

               if Is_Anon
                 or else not Typ_Trans.Success
                 or else not Is_Supported_Type (Typ_Trans.Res.Get)
               then
                  if not Typ_Trans.Success then
                     Put_Line ("Failed: " & To_String (Typ_Trans.Diagnostics));
                  end if;
                  declare
                     Type_Name : constant String :=
                       (if Is_Anon
                        then Param.F_Type_Expr.Image
                        else Typ_Decl.P_Defining_Name.F_Name.Image);
                  begin
                     Put_Line ("Type " & Type_Name & " is not supported yet");
                  end;
                  Put_Line ("Subp " &
                              Node.As_Subp_Decl.P_Defining_Name.F_Name.Image &
                              " is ignored");
                  Local_Types.Clear;
                  exit;
               end if;

               Local_Types.Include (Typ_Decl.F_Name, Typ_Trans.Res);
            end;
         end loop;

         for Cu in Local_Types.Iterate loop
            Used_Types.Include (Type_Maps.Key (Cu), Type_Maps.Element (Cu));
         end loop;
         Local_Types.Clear;
      end if;
      return LALCO.Over;
   end Get_All_Types;

   -----------------------
   -- Is_Supported_Type --
   -----------------------

   function Is_Supported_Type (Typ : TGen.Types.Typ'Class) return Boolean is
      use Component_Maps;

      function Is_Supported_Variant
        (Variant_Part : Variant_Part_Acc) return Boolean;
      --  Recursive function used to check the variant part of a discriminated
      --  record.

      --------------------------
      -- Is_Supported_Variant --
      --------------------------

      function Is_Supported_Variant
        (Variant_Part : Variant_Part_Acc) return Boolean
      is
      begin
         if Variant_Part /= null then

            --  Check each variant choice

            for V_Choice of Variant_Part.Variant_Choices loop

               --  Check components

               for Cu in V_Choice.Components.Iterate loop
                  if not Is_Supported_Type (Element (Cu).Get) then
                     Ada.Text_IO.Put_Line (Element (Cu).Get.Image);
                     return False;
                  end if;
               end loop;

               --  Check the variant part if any

               if not Is_Supported_Variant (V_Choice.Variant) then
                  return False;
               end if;
            end loop;
         end if;

         return True;
      end Is_Supported_Variant;

   begin
      if Typ in Scalar_Typ'Class then
         return True;
      elsif Typ in Constrained_Array_Typ'Class then
         return Is_Supported_Type
           (Constrained_Array_Typ'Class (Typ).Component_Type.Get);
      elsif Typ in Unconstrained_Array_Typ'Class then
         return Is_Supported_Type
           (Unconstrained_Array_Typ'Class (Typ).Component_Type.Get);
      elsif Typ in Record_Typ'Class then

         --  Check specific components of discriminated records

         if Typ in Discriminated_Record_Typ'Class then
            declare
               D_Typ : Discriminated_Record_Typ'Class renames
                 Discriminated_Record_Typ'Class (Typ);

            begin
               --  We don't support mutable discriminants yet

               if D_Typ.Mutable then
                  Ada.Text_IO.Put_Line ("mutable discriminants");
                  return False;
               end if;

               --  Check that the discriminant types are supported

               for Cu in D_Typ.Discriminant_Types.Iterate loop
                  if not Is_Supported_Type (Element (Cu).Get) then
                     Ada.Text_IO.Put_Line (Element (Cu).Get.Image);
                     return False;
                  end if;
               end loop;

               --  Check the variant parts if any

               if not Is_Supported_Variant (D_Typ.Variant) then
                  return False;
               end if;
            end;
         end if;

         --  Check regular component types

         for Cu in Record_Typ'Class (Typ).Component_Types.Iterate loop
            if not Is_Supported_Type (Element (Cu).Get) then
               Ada.Text_IO.Put_Line (Element (Cu).Get.Image);
               return False;
            end if;
         end loop;

         return True;
      else
         Ada.Text_IO.Put_Line (Typ.Image);
         return False;
      end if;
   end Is_Supported_Type;

   ------------------
   -- Needs_Header --
   ------------------

   function Needs_Header (Typ : TGen.Types.Typ'Class) return Boolean is
     (Typ in Unconstrained_Array_Typ'Class
      or else (Typ in Discriminated_Record_Typ'Class
            and then not Discriminated_Record_Typ'Class (Typ).Constrained
            and then not Discriminated_Record_Typ'Class (Typ).Mutable));

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Job_Ctx : Libadalang.Helpers.App_Job_Context; Unit : LAL.Analysis_Unit)
   is
      pragma Unreferenced (Job_Ctx);
   begin
      --  Report parsing errors, if any

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;

      --  Check that the file contains a compilation unit for a package
      --  declaration.

      elsif Unit.Root.Kind /= LALCO.Ada_Compilation_Unit then
         Put_Line ("Unit is not a compilation unit");
      elsif Unit.Root.As_Compilation_Unit.P_Decl.Kind /= LALCO.Ada_Package_Decl
      then
         Put_Line ("Unit does not contain a package declaration");

      --  Collect all types used as parameters in subprogram declarations
      else
         for Decl of Unit.Root.As_Compilation_Unit.P_Decl.As_Package_Decl.
           F_Public_Part.F_Decls
         loop
            Decl.Traverse (Get_All_Types'Access);
         end loop;

         declare
            Pack_Name : constant String := Langkit_Support.Text.Image
              (Unit.Root.As_Compilation_Unit.P_Decl.
                 As_Package_Decl.F_Package_Name.Text);
            F_Spec    : File_Type;
            F_Body    : File_Type;
            File_Name : constant String :=
              (for C of Pack_Name =>
                 (case C is
                     when '.'           => '-',
                     when 'A' .. 'Z'    =>
                       Character'Val
                    (Character'Pos (C) - Character'Pos ('A')
                     + Character'Pos ('a')),
                     when others        => C))
               & "-tagada_parsing";
         begin
            Create (F_Spec, Out_File, File_Name & ".ads");
            Put_Line (F_Spec, "with Ada.Streams; use Ada.Streams;");
            Put_Line (F_Spec, "package " & Pack_Name & ".TAGAda_Parsing is");
            New_Line (F_Spec);

            Create (F_Body, Out_File, File_Name & ".adb");
            Put (F_Body, "with TAGAda_Marshalling_Lib; ");
            Put_Line (F_Body, "use TAGAda_Marshalling_Lib;");
            Put (F_Body, "with Interfaces; ");
            Put_Line (F_Body, "use Interfaces;");
            Put_Line
              (F_Body, "package body " & Pack_Name & ".TAGAda_Parsing is");
            New_Line (F_Body);

            --  Disable predicate checks in the marshalling and unmarshalling
            --  functions.

            Put_Line
              (F_Body, "   pragma Assertion_Policy (Predicate => Ignore);");
            New_Line (F_Body);

            for Cu in Used_Types.Iterate loop
               Generate_Base_Functions_For_Typ
                 (F_Body, Type_Maps.Element (Cu).Get);
            end loop;

            for Cu in Used_Types.Iterate loop
               Generate_Marshalling_Functions_For_Typ
                 (F_Spec, F_Body, Type_Maps.Element (Cu).Get);
            end loop;

            Put_Line (F_Body, "end " & Pack_Name & ".TAGAda_Parsing;");
            Close (F_Body);
            Put_Line (F_Spec, "end " & Pack_Name & ".TAGAda_Parsing;");
            Close (F_Spec);
         end;
      end if;
   end Process_Unit;
begin
   App.Run;
end TGen_Marshalling;
