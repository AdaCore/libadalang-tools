------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2021-2022, AdaCore                    --
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
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with Langkit_Support.Text;         use Langkit_Support.Text;

with Templates_Parser;             use Templates_Parser;

with TGen.Strings;                 use TGen.Strings;
with TGen.Types.Array_Types;       use TGen.Types.Array_Types;
with TGen.Types.Constraints;       use TGen.Types.Constraints;
with TGen.Types.Discrete_Types;    use TGen.Types.Discrete_Types;
with TGen.Types.Enum_Types;        use TGen.Types.Enum_Types;
with TGen.Types.Real_Types;        use TGen.Types.Real_Types;
with TGen.Types.Record_Types;      use TGen.Types.Record_Types;

package body TGen.Marshalling is

   Global_Prefix   : constant String := "TAGAda_Marshalling";
   Marshalling_Lib : constant String := "TAGAda_Marshalling_Lib";

   --------------------
   -- Template Files --
   --------------------

   Template_Folder               : constant String :=
     "marshalling_templates/";
   Array_Read_Write_Template     : constant String :=
     Template_Folder & "array_read_write.tmplt";
   Component_Read_Write_Template : constant String :=
     Template_Folder & "component_read_write.tmplt";
   Component_Size_Max_Template   : constant String :=
     Template_Folder & "component_size_max.tmplt";
   Component_Size_Template       : constant String :=
     Template_Folder & "component_size.tmplt";
   Composite_Base_Spec_Template  : constant String :=
     Template_Folder & "composite_base_spec.tmplt";
   Header_Body_Template          : constant String :=
     Template_Folder & "header_body.tmplt";
   Header_Spec_Template          : constant String :=
     Template_Folder & "header_spec.tmplt";
   Header_Wrappers_Body_Template : constant String :=
     Template_Folder & "header_wrappers_body.tmplt";
   Header_Wrappers_Spec_Template : constant String :=
     Template_Folder & "header_wrappers_spec.tmplt";
   In_Out_Body_Template          : constant String :=
     Template_Folder & "in_out_body.tmplt";
   In_Out_Spec_Template          : constant String :=
     Template_Folder & "in_out_spec.tmplt";
   Record_Read_Write_Template    : constant String :=
     Template_Folder & "record_read_write.tmplt";
   Record_Size_Max_Template      : constant String :=
     Template_Folder & "record_size_max.tmplt";
   Record_Size_Template          : constant String :=
     Template_Folder & "record_size.tmplt";
   Scalar_Base_Spec_Template     : constant String :=
     Template_Folder & "scalar_base_spec.tmplt";
   Scalar_Read_Write_Template    : constant String :=
     Template_Folder & "scalar_read_write.tmplt";
   Variant_Read_Write_Template   : constant String :=
     Template_Folder & "variant_read_write.tmplt";
   Variant_Size_Max_Template     : constant String :=
     Template_Folder & "variant_size_max.tmplt";
   Variant_Size_Template         : constant String :=
     Template_Folder & "variant_size.tmplt";
   Array_Size_Max_Template       : constant String :=
     Template_Folder & "array_size_max.tmplt";
   Array_Size_Template           : constant String :=
     Template_Folder & "array_size.tmplt";

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Prefix_For_Typ (Ty_Name : String) return String is
     (Global_Prefix & "_" & Ty_Name);
   --  Construct a prefix that will be shared by all entities generated for a
   --  given type.

   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => String,
      Hash                => Ada.Strings.Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   function Needs_Wrappers (Typ : TGen.Types.Typ'Class) return Boolean;
   --  Return True for types with headers when they can occur nested in the
   --  data-structure (not at the top level).
   --  For now, this is only true for types with mutable discriminants as we
   --  do not support access types.

   function Create_Tag_For_Constraints
     (Comp_Ty : TGen.Types.Typ'Class) return Tag;
   --  Create a tag for the constraints of anonymous types.
   --   * For scalars and uni-dimensional arrays:
   --       <Global_Prefix>_First => <Low_Bound>,
   --       <Global_Prefix>_Last  => <High_Bound>
   --   * For mulit-dimensional arrays:
   --       <Global_Prefix>_First_1 => <Low_Bound for first index>,
   --       <Global_Prefix>_Last_1  => <High_Bound for first index>,
   --       <Global_Prefix>_First_2 => <Low_Bound for second index>,
   --       ...
   --   * For records:
   --       <Global_Prefix>_<Discr_Name>_Min => <Low_Bound for Discr_Name>,
   --       <Global_Prefix>_<Discr_Name>_Max => <High_Bound for Discr_Name>,
   --       ...
   --     Min and Max values for discriminants come from types constrained
   --     using a discriminant from the upper level. They are the bounds of
   --     the upper level discriminant type.
   --
   --  ??? For now, we do not support numeric constraints which are not static.
   --  We use the bounds of the type instead. We could possibly do better when
   --  dynamic values are better handled inside tgen. However, we might want to
   --  try to detect cases where a dynamic numeric constraint depends on
   --  variables which might have been modified after the type elaboration.

   function Create_Tag_For_Intervals
     (Intervals : Alternatives_Set; Typ : TGen.Types.Typ'Class) return Tag;
   --  Return as a tag the choices represented by a set of intervals

   procedure Generate_Base_Functions_For_Typ
     (F_Spec   : File_Type;
      F_Body   : File_Type;
      Typ      : TGen.Types.Typ'Class;
      For_Base : Boolean := False)
   with Pre => (if For_Base then Typ in Scalar_Typ'Class)
     and then Typ not in Anonymous_Typ'Class;
   --  Generate base marshalling and unmarshalling functions for Typ. Store
   --  types for which the base generating function have already been generated
   --  to avoid generating them twice.
   --  If For_Base is True, generate the functions for Typ'Base.
   --
   --  For scalars, we generate:
   --
   --  procedure TAGAda_Marshalling_Typ_Write
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : Typ;
   --     TAGAda_Marshalling_First  : Typ := Typ'First;
   --     TAGAda_Marshalling_Last   : Typ := Typ'Last);
   --
   --  procedure TAGAda_Marshalling_Typ_Read
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : out Typ;
   --     TAGAda_Marshalling_First  : Typ := Typ'First;
   --     TAGAda_Marshalling_Last   : Typ := Typ'Last);
   --
   --  function TAGAda_Marshalling_Typ_Size
   --    (TAGAda_Marshalling_First  : Typ := Typ'First;
   --     TAGAda_Marshalling_Last   : Typ := Typ'Last)
   --     return Natural;
   --
   --  The addtional First and Last parameters are used to handle anonymous
   --  subtypes in record or array components.
   --
   --  For composite types with no headers, we generate:
   --
   --  procedure TAGAda_Marshalling_Typ_Write
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  procedure TAGAda_Marshalling_Typ_Read
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : out Typ);
   --
   --  function TAGAda_Marshalling_Typ_Size
   --    (TAGAda_Marshalling_V : Typ)
   --    return Natural;
   --
   --  function TAGAda_Marshalling_Typ_Size_Max
   --    return Natural;
   --
   --  The Size and Size_Max functions should be the same here, except if Typ
   --  contains component with dynamic bounds which we do not handle well
   --  currently.
   --
   --  For composite types with with headers, we generate:
   --
   --  type TAGAda_Marshalling_Typ_Header_Type is record
   --     < Typ's array bound or record discriminants >
   --  end record;
   --
   --  function TAGAda_Marshalling_Typ_Input_Header
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class)
   --    return TAGAda_Marshalling_Typ_Header_Type;
   --
   --  procedure TAGAda_Marshalling_Typ_Output_Header
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  function TAGAda_Marshalling_Typ_Bit_Size_Header return Natural;
   --
   --  function TAGAda_Marshalling_Typ_Byte_Size_Header return Natural;
   --
   --  procedure TAGAda_Marshalling_Typ_Write
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  procedure TAGAda_Marshalling_Typ_Read
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : out Typ);
   --
   --  function TAGAda_Marshalling_Typ_Size
   --    (TAGAda_Marshalling_V : Typ)
   --    return Natural;
   --
   --  function TAGAda_Marshalling_Typ_Size_Max
   --    (<Min and Max value of discriminants or array bounds>)
   --    return Natural;
   --
   --  If the type can be used as a component (see Needs_Wrapper), we also
   --  generate:
   --
   --  procedure TAGAda_Marshalling_Typ_Write_All
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : Typ);
   --
   --  procedure TAGAda_Marshalling_Typ_Read_All
   --    (TAGAda_Marshalling_Stream : not null access Root_Stream_Type'Class;
   --     TAGAda_Marshalling_Buffer : in out Unsigned_8;
   --     TAGAda_Marshalling_Offset : in out Offset_Type;
   --     TAGAda_Marshalling_V      : out Typ);
   --
   --  function TAGAda_Marshalling_Typ_Size_Max_All return Natural;
   --
   --  They also marshall the header and add some padding so that there is
   --  enough room to read a correct value if the header is mutated.

   procedure Create_Tags_For_Array_Bounds
     (U_Typ        : Unconstrained_Array_Typ'Class;
      Fst_Name_Tag : in out Tag;
      Lst_Name_Tag : in out Tag;
      Typ_Tag      : in out Tag;
      Pref_Tag     : in out Tag);
   --  Compute the tags for the bounds of an unconstrained array type:
   --    * Fst_Name_Tag contains the names of the objects corresponding to
   --      the lower bounds: First_1, ...,
   --    * Lst_Name_Tag contains the names of the objects corresponding to
   --      the higher bounds: Last_1, ...,
   --    * Typ_Tag contains the index types: First_Index ..., and
   --    * Pref_Tag contains the prefix associated to the index base types:
   --      Global_Prefix_First_Index_Base...

   function Create_Tags_For_Array_Dims (A_Typ : Array_Typ'Class) return Tag;
   --  Compute the string to be associated to array attributes for each
   --  dimension. Return "" for unidimensional arrays and (1) (2)... for
   --  multidimensional ones.

   procedure Create_Tags_For_Discriminants
     (D_Typ    : Discriminated_Record_Typ'Class;
      Name_Tag : in out Tag;
      Typ_Tag  : in out Tag;
      Pref_Tag : in out Tag);
     --  Compute the tags for the discriminant of a record type:
     --    * Name_Tag contains the names of the  discriminants: Discr, ...,
     --    * Typ_Tag contains their types: Discr_Ty ..., and
     --    * Pref_Tag contains the prefix associated to the type:
     --      Global_Prefix_Descr_Ty...,

   function String_Value
     (V   : TGen.Types.Big_Integer;
      Typ : TGen.Types.Typ'Class) return String;
   --  Get a string for the value at position V in Typ

   --------------------------------
   -- Create_Tag_For_Constraints --
   --------------------------------

   function Create_Tag_For_Constraints
     (Comp_Ty : TGen.Types.Typ'Class) return Tag
   is

      function Bound_To_String
        (C      : Discrete_Constraint_Value;
         Typ    : TGen.Types.Typ'Class;
         Is_Min : Boolean := True) return String
      is
        (case C.Kind is
            when Static       => String_Value (C.Int_Val, Typ),
            when Discriminant =>
             Global_Prefix & "_" & String'(+C.Disc_Name)
             & (if Is_Min then "_D_Min" else "_D_Max"),
            when Non_Static   => "");
      --  Compute the constraint from a discrete value C. If C has a
      --  static value, we use it. If it is a discriminant, we use the min or
      --  max value for this discriminant. We do not supply values for
      --  dynamic bounds for now. The default values (the First and Last
      --  attribute of the expected type) will be used instead.

      procedure Append_Association
        (Name         : String;
         Value        : String;
         Associations : in out Tag);
      --  Append the association Name => Value to Associations

      ------------------------
      -- Append_Association --
      ------------------------

      procedure Append_Association
        (Name         : String;
         Value        : String;
         Associations : in out Tag)
      is
      begin
         if Value'Length = 0 then
            return;
         end if;

         Associations := Associations & (Name & " => " & Value);
      end Append_Association;

   begin
      --  Nothing to do for named types

      if Comp_Ty not in Anonymous_Typ'Class then
         return +"";
      end if;

      declare
         Associations : Tag;
         Ancestor     : constant TGen.Types.Typ'Class :=
           Anonymous_Typ'Class (Comp_Ty).Named_Ancestor.Get;
         Constraint   : constant Constraint_Acc :=
           Anonymous_Typ'Class (Comp_Ty).Subtype_Constraints;

      begin
         if Constraint = null then
            return +"";

         --  For a discrete range constraint:
         --    range Low .. High
         --  We generate:
         --    First => Low, Last => High

         elsif Constraint.all in Discrete_Range_Constraint'Class then
            declare
               D_Constr : constant Discrete_Range_Constraint'Class :=
                 Discrete_Range_Constraint'Class (Constraint.all);
            begin
               Append_Association
                 (Name         => Global_Prefix & "_First",
                  Value        => Bound_To_String
                    (D_Constr.Low_Bound, Typ => Ancestor),
                  Associations => Associations);
               Append_Association
                 (Name         => Global_Prefix & "_Last",
                  Value        => Bound_To_String
                    (D_Constr.High_Bound, Typ => Ancestor),
                  Associations => Associations);
            end;

         --  For an array constraint:
         --    (F1 .. L1, ...)
         --  We generate:
         --    First_1 => F1, Last_1  => L1, ...
         --  If F1 or L1 are discriminants, we generate instead:
         --    First_1 => F1_Min, Last_1  => L1_Max, ...

         elsif Constraint.all in Index_Constraints'Class then
            declare
               A_Typ       : constant Array_Typ'Class :=
                 Array_Typ'Class (Ancestor);
               Idx_Constrs : constant Index_Constraints'Class :=
                 Index_Constraints'Class (Constraint.all);
            begin
               for Index in Idx_Constrs.Constraint_Array'Range loop
                  declare
                     Idx_Constr : constant Index_Constraint :=
                       Idx_Constrs.Constraint_Array (Index);
                     Idx_Typ    : constant TGen.Types.Typ'Class :=
                       A_Typ.Index_Types (Index).Get;
                     Dim        : constant String := Trim (Index'Image, Left);
                  begin
                     if Idx_Constr.Present then
                        Append_Association
                          (Name         =>
                             Global_Prefix & "_First_" & Dim,
                           Value        => Bound_To_String
                             (Idx_Constr.Discrete_Range.Low_Bound, Idx_Typ,
                              Is_Min => True),
                           Associations => Associations);
                        Append_Association
                          (Name         =>
                             Global_Prefix & "_Last_" & Dim,
                           Value        => Bound_To_String
                             (Idx_Constr.Discrete_Range.High_Bound, Idx_Typ,
                              Is_Min => False),
                        Associations => Associations);
                     end if;
                  end;
               end loop;
            end;

         --  For a discriminant constraint:
         --    (D1 => V1, ...)
         --  We generate:
         --    D1_Min => V1, D1_Max => V1, ...
         --  If V1 is a discriminant, we generate instead:
         --    D1_Min => V1_Min, D1_Max => V1_Max, ...

         elsif Constraint.all in Discriminant_Constraints'Class then
            declare
               D_Typ        : constant Discriminated_Record_Typ'Class :=
                 Discriminated_Record_Typ'Class (Ancestor);
               Discr_Constraints : constant Discriminant_Constraint_Map :=
                 Discriminant_Constraints (Constraint.all).Constraint_Map;
            begin

               for Cu in Discr_Constraints.Iterate loop
                  declare
                     Discr_Text   : constant Unbounded_Text_Type :=
                       Discriminant_Constraint_Maps.Key (Cu);
                     Discr_Name   : constant String := +Discr_Text;
                     Discr_Constr : constant Discrete_Constraint_Value :=
                       Discriminant_Constraint_Maps.Element (Cu);
                     Discr_Typ    : constant TGen.Types.Typ'Class :=
                       D_Typ.Discriminant_Types.Element (Discr_Text).Get;
                  begin
                     Append_Association
                       (Name         =>
                          Global_Prefix & "_" & Discr_Name & "_D_Min",
                        Value        => Bound_To_String
                          (Discr_Constr, Discr_Typ, Is_Min => True),
                        Associations => Associations);
                     Append_Association
                       (Name         =>
                          Global_Prefix & "_" & Discr_Name & "_D_Max",
                        Value        => Bound_To_String
                          (Discr_Constr, Discr_Typ, Is_Min => False),
                        Associations => Associations);
                  end;
               end loop;
            end;
         else
            raise Program_Error;
         end if;
         Set_Separator (Associations, ", ");
         return Associations;
      end;
   end Create_Tag_For_Constraints;

   ------------------------------
   -- Create_Tag_For_Intervals --
   ------------------------------

   function Create_Tag_For_Intervals
     (Intervals : Alternatives_Set; Typ : TGen.Types.Typ'Class) return Tag
   is
      Choices_Tag : Tag;
   begin
      for Int of Intervals loop
         if Int.Min = Int.Max then
            Choices_Tag := Choices_Tag & String_Value (Int.Min, Typ);
         else
            Choices_Tag := Choices_Tag &
            (String_Value (Int.Min, Typ) & " .. "
             & String_Value (Int.Max, Typ));
         end if;
      end loop;

      Set_Separator (Choices_Tag, " | ");
      return Choices_Tag;
   end Create_Tag_For_Intervals;

   ----------------------------------
   -- Create_Tags_For_Array_Bounds --
   ----------------------------------

   procedure Create_Tags_For_Array_Bounds
     (U_Typ        : Unconstrained_Array_Typ'Class;
      Fst_Name_Tag : in out Tag;
      Lst_Name_Tag : in out Tag;
      Typ_Tag      : in out Tag;
      Pref_Tag     : in out Tag)
   is
      First_Name_Tmplt : constant String := "First_@_DIM_@";
      Last_Name_Tmplt  : constant String := "Last_@_DIM_@";

   begin
      for I in U_Typ.Index_Types'Range loop
         declare
            Index_Type : constant String :=
              U_Typ.Index_Types (I).Get.Type_Name;
            Index_Pref : constant String := Prefix_For_Typ (Index_Type);
            Assocs     : constant Translate_Table :=
              [1 => Assoc ("DIM", I)];
         begin
            Fst_Name_Tag := Fst_Name_Tag
              & Translate (First_Name_Tmplt, Assocs);
            Lst_Name_Tag := Lst_Name_Tag
              & Translate (Last_Name_Tmplt, Assocs);

            Typ_Tag := Typ_Tag & Index_Type;
            Pref_Tag := Pref_Tag & Index_Pref;
         end;
      end loop;
   end Create_Tags_For_Array_Bounds;

   --------------------------------
   -- Create_Tags_For_Array_Dims --
   --------------------------------

   function Create_Tags_For_Array_Dims (A_Typ : Array_Typ'Class) return Tag is
      Ada_Dim_Tmplt    : constant String := "(@_DIM_@)";

   begin
      return Ada_Dim_Tag : Tag do
         for I in A_Typ.Index_Types'Range loop
            declare
               Assocs     : constant Translate_Table :=
                 [1 => Assoc ("DIM", I)];
            begin
               if A_Typ.Num_Dims = 1 then
                  Ada_Dim_Tag := Ada_Dim_Tag & "";
               else
                  Ada_Dim_Tag := Ada_Dim_Tag
                    & Translate (Ada_Dim_Tmplt, Assocs);
               end if;
            end;
         end loop;
      end return;
   end Create_Tags_For_Array_Dims;

   -----------------------------------
   -- Create_Tags_For_Discriminants --
   -----------------------------------

   procedure Create_Tags_For_Discriminants
     (D_Typ    : Discriminated_Record_Typ'Class;
      Name_Tag : in out Tag;
      Typ_Tag  : in out Tag;
      Pref_Tag : in out Tag)
   is
   begin
      for Cu in D_Typ.Discriminant_Types.Iterate loop
         declare
            Discr_Name : constant String := (+Component_Maps.Key (Cu));
            Discr_Typ  : constant String :=
              (Component_Maps.Element (Cu).Get.Type_Name);
            Discr_Pref : constant String :=
              Prefix_For_Typ (Discr_Typ);
         begin
            Name_Tag := Name_Tag & Discr_Name;
            Typ_Tag := Typ_Tag & Discr_Typ;
            Pref_Tag := Pref_Tag & Discr_Pref;
         end;
      end loop;
   end Create_Tags_For_Discriminants;

   -------------------------------------
   -- Generate_Base_Functions_For_Typ --
   -------------------------------------

   Base_Seen    : Name_Sets.Set;
   Already_Seen : Name_Sets.Set;

   procedure Generate_Base_Functions_For_Typ
     (F_Spec   : File_Type;
      F_Body   : File_Type;
      Typ      : TGen.Types.Typ'Class;
      For_Base : Boolean := False)
   is
      B_Name         : constant String := Typ.Type_Name;
      Ty_Prefix      : constant String := Prefix_For_Typ (B_Name);
      Ty_Name        : constant String :=
        (if For_Base then B_Name & "'Base" else B_Name);

      --  Function computing the indentation for component handling

      --  Initial spacing for records
      RW_Init_Spacing   : constant Natural := 6;
      Max_Init_Spacing  : constant Natural := 9;
      Size_Init_Spacing : constant Natural := 9;
      --  Incremental spacing for record variants
      Var_Incr_Spacing  : constant Natural := 6;
      --  Spacing for arrays
      RW_Arr_Spacing    : constant Natural := 9;
      Max_Arr_Spacing   : constant Natural := 6;
      Size_Arr_Spacing  : constant Natural := 9;

      function Max_Spacing (Spacing : Natural) return String is
        (if Typ in Array_Typ'Class then [1 .. Max_Arr_Spacing => ' ']
         else [1 .. Max_Init_Spacing + Spacing * Var_Incr_Spacing => ' ']);

      function RW_Spacing (Spacing : Natural) return String is
        (if Typ in Array_Typ'Class then [1 .. RW_Arr_Spacing => ' ']
         else [1 .. RW_Init_Spacing + Spacing * Var_Incr_Spacing => ' ']);

      function Size_Spacing (Spacing : Natural) return String is
        (if Typ in Array_Typ'Class then [1 .. Size_Arr_Spacing => ' ']
         else [1 .. Size_Init_Spacing + Spacing * Var_Incr_Spacing => ' ']);

      procedure Collect_Info_For_Component
        (Comp_Name    : String;
         Comp_Ty      : TGen.Types.Typ'Class;
         Read_Tag     : out Unbounded_String;
         Write_Tag    : out Unbounded_String;
         Size_Tag     : out Unbounded_String;
         Size_Max_Tag : out Unbounded_String;
         Spacing      : Natural);
      --  Generate the parts of the subprograms Read, Write, Size, and Size_Max
      --  for a component Comp_Name of type Comp_Ty. Also generate base
      --  functions for Comp_Ty.
      --  Spacing is used to tabulate the generated code, see ..._Spacing
      --  above.

      procedure Collect_Info_For_Components
        (Components   : Component_Maps.Map;
         Read_Tag     : in out Vector_Tag;
         Write_Tag    : in out Vector_Tag;
         Size_Tag     : in out Vector_Tag;
         Size_Max_Tag : in out Vector_Tag;
         Spacing      : Natural;
         Object_Name  : String);
      --  Go over the components in Components and generate the parts of the
      --  subprograms Read, Write, Size, and Size_Max for the components.
      --  Along the way, generate base functions for the component types.
      --  Spacing is used to tabulate the generated code Object_Name is the
      --  name of the object we are traversing.

      procedure Collect_Info_For_Variants
        (V             : TGen.Types.Record_Types.Variant_Part;
         Discriminants : Component_Maps.Map;
         Read_Tag      : out Tag;
         Write_Tag     : out Tag;
         Size_Tag      : out Tag;
         Size_Max_Tag  : out Vector_Tag;
         Spacing       : Natural;
         Object_Name   : String);
      --  Recursive function which instanciates the variant part templates to
      --  create strings for the operations Read, Write, Size, and Size_Max on
      --  a variant part V.
      --  Spacing is used to tabulate the generated code Object_Name is the
      --  name of the object we are traversing.

      --------------------------------
      -- Collect_Info_For_Component --
      --------------------------------

      procedure Collect_Info_For_Component
        (Comp_Name    : String;
         Comp_Ty      : TGen.Types.Typ'Class;
         Read_Tag     : out Unbounded_String;
         Write_Tag    : out Unbounded_String;
         Size_Tag     : out Unbounded_String;
         Size_Max_Tag : out Unbounded_String;
         Spacing      : Natural)
      is
         Named_Comp_Ty    : constant TGen.Types.Typ'Class :=
           (if Comp_Ty in Anonymous_Typ'Class
            then Anonymous_Typ'Class (Comp_Ty).Named_Ancestor.Get
            else Comp_Ty);
         Comp_Scalar      : constant Boolean :=
           Named_Comp_Ty in Scalar_Typ'Class;
         Comp_Prefix      : constant String :=
           Prefix_For_Typ (Named_Comp_Ty.Type_Name);
         Comp_Constraints : constant Tag :=
           Create_Tag_For_Constraints (Comp_Ty);
         Assocs           : constant Translate_Table :=
           [1 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
            2 => Assoc ("COMP_PREFIX", Comp_Prefix),
            3 => Assoc ("COMPONENT", Comp_Name),
            4 => Assoc ("CONSTRAINTS", Comp_Constraints),
            5 => Assoc ("COMP_SCALAR", Comp_Scalar),
            6 => Assoc ("NEEDS_HEADER", Needs_Header (Named_Comp_Ty))];
      begin
         Generate_Base_Functions_For_Typ (F_Spec, F_Body, Named_Comp_Ty);

         Read_Tag := Parse
           (Component_Read_Write_Template,
            Assocs &
            [1 => Assoc ("SPACING", RW_Spacing (Spacing)),
             2 => Assoc ("ACTION", "Read")]);
         Write_Tag := Parse
           (Component_Read_Write_Template,
            Assocs &
            [1 => Assoc ("SPACING", RW_Spacing (Spacing)),
             2 => Assoc ("ACTION", "Write")]);
         Size_Tag := Parse
           (Component_Size_Template,
            Assocs & Assoc ("SPACING", Size_Spacing (Spacing)));
         Size_Max_Tag := Parse
           (Component_Size_Max_Template,
            Assocs & Assoc ("SPACING", Max_Spacing (Spacing)));
      end Collect_Info_For_Component;

      ---------------------------------
      -- Collect_Info_For_Components --
      ---------------------------------

      procedure Collect_Info_For_Components
        (Components   : Component_Maps.Map;
         Read_Tag     : in out Vector_Tag;
         Write_Tag    : in out Vector_Tag;
         Size_Tag     : in out Vector_Tag;
         Size_Max_Tag : in out Vector_Tag;
         Spacing      : Natural;
         Object_Name  : String)
      is
      begin
         --  Go over the record components to fill the associations

         for Cu in Components.Iterate loop
            declare
               Comp_Ty   : constant TGen.Types.Typ'Class :=
                 Component_Maps.Element (Cu).Get;
               Comp_Name : constant String := +Component_Maps.Key (Cu);
               Read      : Unbounded_String;
               Write     : Unbounded_String;
               Size      : Unbounded_String;
               Size_Max  : Unbounded_String;
            begin
               Collect_Info_For_Component
                 (Object_Name & "." & Comp_Name, Comp_Ty,
                  Read, Write, Size, Size_Max, Spacing);
               Read_Tag := Read_Tag & Read;
               Write_Tag := Write_Tag & Write;
               Size_Tag := Size_Tag & Size;
               Size_Max_Tag := Size_Max_Tag & Size_Max;
            end;
         end loop;
      end Collect_Info_For_Components;

      ----------------------------------
      -- Collect_Info_For_Variants --
      ----------------------------------

      procedure Collect_Info_For_Variants
        (V             : TGen.Types.Record_Types.Variant_Part;
         Discriminants : Component_Maps.Map;
         Read_Tag      : out Tag;
         Write_Tag     : out Tag;
         Size_Tag      : out Tag;
         Size_Max_Tag  : out Vector_Tag;
         Spacing       : Natural;
         Object_Name   : String)
      is
         Discr_Name : constant String := +V.Discr_Name;
         Discr_Typ  : constant TGen.Types.Typ'Class :=
           Discriminants (V.Discr_Name).Get;

         Choices_Tag          : Matrix_Tag;
         Comp_Read_Tag        : Matrix_Tag;
         Comp_Write_Tag       : Matrix_Tag;
         Comp_Size_Tag        : Matrix_Tag;
         Comp_Size_Max_Tag    : Matrix_Tag;
         Variant_Read_Tag     : Vector_Tag;
         Variant_Write_Tag    : Vector_Tag;
         Variant_Size_Tag     : Vector_Tag;
         Variant_Size_Max_Tag : Vector_Tag;

      begin
         for V_Choice of V.Variant_Choices loop

            --  Get tags for the variant choices

            Choices_Tag :=
              Choices_Tag & Create_Tag_For_Intervals
                (V_Choice.Alt_Set, Discr_Typ);

            --  Hanlde the components

            declare
               Comp_Read     : Tag;
               Comp_Write    : Tag;
               Comp_Size     : Tag;
               Comp_Size_Max : Tag;
            begin
               Collect_Info_For_Components
                 (V_Choice.Components,
                  Comp_Read, Comp_Write, Comp_Size, Comp_Size_Max,
                  Spacing     => Spacing + 1,
                  Object_Name => Object_Name);
               Comp_Read_Tag := Comp_Read_Tag & Comp_Read;
               Comp_Write_Tag := Comp_Write_Tag & Comp_Write;
               Comp_Size_Tag := Comp_Size_Tag & Comp_Size;
               Comp_Size_Max_Tag := Comp_Size_Max_Tag & Comp_Size_Max;
            end;

            --  Handle the nested variant if any

            if V_Choice.Variant = null then
               Variant_Read_Tag := Variant_Read_Tag & "";
               Variant_Write_Tag := Variant_Write_Tag & "";
               Variant_Size_Tag := Variant_Size_Tag & "";
               Variant_Size_Max_Tag := Variant_Size_Max_Tag & "";
            else
               declare
                  Variant_Read     : Tag;
                  Variant_Write    : Tag;
                  Variant_Size     : Tag;
                  Variant_Size_Max : Tag;
               begin
                  Collect_Info_For_Variants
                    (V_Choice.Variant.all, Discriminants,
                     Variant_Read, Variant_Write,
                     Variant_Size, Variant_Size_Max,
                     Spacing + 1, Object_Name);
                  Variant_Read_Tag := Variant_Read_Tag & Variant_Read;
                  Variant_Write_Tag := Variant_Write_Tag & Variant_Write;
                  Variant_Size_Tag := Variant_Size_Tag & Variant_Size;
                  Variant_Size_Max_Tag :=
                    Variant_Size_Max_Tag & Variant_Size_Max;
               end;
            end if;
         end loop;

         --  Instanciate the appropriate template to glue the pieces together

         declare
            Assocs : constant Translate_Table :=
              [1 => Assoc ("OBJECT_NAME", Object_Name),
               2 => Assoc ("DISCR_NAME", Discr_Name),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("CHOICES", Choices_Tag)];

         begin
            Read_Tag := +String'
              (Parse (Variant_Read_Write_Template, Assocs &
                      [1 => Assoc ("COMPONENT_ACTION", Comp_Read_Tag),
                       2 => Assoc ("VARIANT_PART", Variant_Read_Tag),
                       3 => Assoc ("SPACING", RW_Spacing (Spacing))]));
            Write_Tag := +String'
              (Parse (Variant_Read_Write_Template, Assocs &
                      [1 => Assoc ("COMPONENT_ACTION", Comp_Write_Tag),
                       2 => Assoc ("VARIANT_PART", Variant_Write_Tag),
                       3 => Assoc ("SPACING", RW_Spacing (Spacing))]));
            Size_Tag := +String'
              (Parse (Variant_Size_Template, Assocs &
                      [1 => Assoc ("COMPONENT_SIZE", Comp_Size_Tag),
                       2 => Assoc ("VARIANT_PART", Variant_Size_Tag),
                       3 => Assoc ("SPACING", Size_Spacing (Spacing))]));
            Size_Max_Tag := +String'
              (Parse (Variant_Size_Max_Template, Assocs &
                      [1 => Assoc ("COMPONENT_SIZE_MAX", Comp_Size_Max_Tag),
                       2 => Assoc ("VARIANT_PART", Variant_Size_Max_Tag),
                       3 => Assoc ("SPACING", Max_Spacing (Spacing))]));
         end;
      end Collect_Info_For_Variants;

      --  Tags for the components of the header if any, their types, their
      --  prefix and the corresponding Ada Value.

      Discr_Name_Tag : Tag;
      First_Name_Tag : Tag;
      Last_Name_Tag  : Tag;
      Comp_Typ_Tag   : Tag;
      Comp_Pref_Tag  : Tag;
      Ada_Dim_Tag    : constant Tag :=
        (if Typ in Array_Typ'Class
         then Create_Tags_For_Array_Dims (Array_Typ'Class (Typ))
         else +"");

   begin
      --  1. Check for inclusion in the general map or in the map for base
      --     types.

      if For_Base then
         if Base_Seen.Contains (Typ.Fully_Qualified_Name) then
            return;
         end if;
         Base_Seen.Include (Typ.Fully_Qualified_Name);
      else
         if Already_Seen.Contains (Typ.Fully_Qualified_Name) then
            return;
         end if;
         Already_Seen.Include (Typ.Fully_Qualified_Name);
      end if;

      --  2. Generate operations for the header if needed

      if Needs_Header (Typ) then

         --  Fill the tags for the components of the header and generate
         --  additional base functions if needed.

         if Typ in Unconstrained_Array_Typ'Class then
            declare
               U_Typ :  Unconstrained_Array_Typ'Class renames
                 Unconstrained_Array_Typ'Class (Typ);

            begin
               --  Generate base functions for the base type of the index types

               for I in U_Typ.Index_Types'Range loop
                  Generate_Base_Functions_For_Typ
                    (F_Spec, F_Body, U_Typ.Index_Types (I).Get,
                     For_Base => True);
               end loop;

               --  Fill the association maps

               Create_Tags_For_Array_Bounds
                 (U_Typ,
                  First_Name_Tag, Last_Name_Tag, Comp_Typ_Tag, Comp_Pref_Tag);
            end;

         else
            declare
               D_Typ : Discriminated_Record_Typ'Class renames
                 Discriminated_Record_Typ'Class (Typ);

            begin
               --  Generate base functions for the discriminant types

               for Cu in D_Typ.Discriminant_Types.Iterate loop
                  Generate_Base_Functions_For_Typ
                    (F_Spec, F_Body, Component_Maps.Element (Cu).Get);
               end loop;

               --  Fill the association maps

               Create_Tags_For_Discriminants
                 (D_Typ, Discr_Name_Tag, Comp_Typ_Tag, Comp_Pref_Tag);
            end;
         end if;

         --  Generate the header

         declare
            Assocs      : constant Translate_Table :=
              [1  => Assoc ("TY_NAME", Ty_Name),
               2  => Assoc ("TY_PREFIX", Ty_Prefix),
               3  => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4  => Assoc ("DISCR_NAME", Discr_Name_Tag),
               5  => Assoc ("FIRST_NAME", First_Name_Tag),
               6  => Assoc ("LAST_NAME", Last_Name_Tag),
               7  => Assoc ("COMP_TYP", Comp_Typ_Tag),
               8  => Assoc ("COMP_PREFIX", Comp_Pref_Tag),
               9  => Assoc ("ADA_DIM", Ada_Dim_Tag)];

         begin
            Put_Line (F_Spec, Parse (Header_Spec_Template, Assocs));
            New_Line (F_Spec);

            Put_Line (F_Body, Parse (Header_Body_Template, Assocs));
            New_Line (F_Body);
         end;
      end if;

      --  3. Generate the specifications of the base operations

      declare
         Spec_Template : constant String :=
           (if Typ in Scalar_Typ'Class then Scalar_Base_Spec_Template
            else Composite_Base_Spec_Template);
         Assocs        : constant Translate_Table :=
           [1  => Assoc ("TY_NAME", Ty_Name),
            2  => Assoc ("TY_PREFIX", Ty_Prefix),
            3  => Assoc ("GLOBAL_PREFIX", Global_Prefix),
            4  => Assoc ("DISCR_NAME", Discr_Name_Tag),
            5  => Assoc ("FIRST_NAME", First_Name_Tag),
            6  => Assoc ("LAST_NAME", Last_Name_Tag),
            7  => Assoc ("COMP_TYP", Comp_Typ_Tag),
            8  => Assoc ("COMP_PREFIX", Comp_Pref_Tag),
            9  => Assoc ("ADA_DIM", Ada_Dim_Tag),
            10 => Assoc ("FOR_BASE", For_Base)];

      begin
         Put_Line (F_Spec, Parse (Spec_Template, Assocs));
         New_Line (F_Spec);
      end;

      --  4. Generate the body of the base operations
      --  4.1. For scalar types, we generate clones. We need to provide the
      --       name of the appropriate generic unit depending on the scalar
      --       kind (Discrete, Fixed, or Float).

      if Typ in Scalar_Typ'Class then
         declare
            Generic_Name : constant String :=
              (if Typ in Discrete_Typ'Class then "Read_Write_Discrete"
               elsif Typ in Float_Typ'Class then "Read_Write_Float"
               elsif Typ in Ordinary_Fixed_Typ'Class
               then "Read_Write_Ordinary_Fixed"
               else "Read_Write_Decimal_Fixed");
            Assocs       : constant Translate_Table :=
              [1 => Assoc ("TY_NAME", Ty_Name),
               2 => Assoc ("TY_PREFIX", Ty_Prefix),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("MARSHALLING_LIB", Marshalling_Lib),
               5 => Assoc ("GENERIC_NAME", Generic_Name),
               6 => Assoc ("IS_DISCRETE", Typ in Discrete_Typ'Class),
               7 => Assoc ("FOR_BASE", For_Base)];

         begin
            Put_Line (F_Body, Parse (Scalar_Read_Write_Template, Assocs));
            New_Line (F_Body);
         end;

      --  4.2 For array types, we generate the calls for the components and we
      --      instanciate the appropriate patterns.

      elsif Typ in Array_Typ'Class then
         declare
            Comp_Ty            : constant TGen.Types.Typ'Class :=
              Array_Typ'Class (Typ).Component_Type.Get;
            Named_Comp_Ty      : constant TGen.Types.Typ'Class :=
              (if Comp_Ty in Anonymous_Typ'Class
               then Anonymous_Typ'Class (Comp_Ty).Named_Ancestor.Get
               else Comp_Ty);
            Component_Read     : Unbounded_String;
            Component_Write    : Unbounded_String;
            Component_Size     : Unbounded_String;
            Component_Size_Max : Unbounded_String;
         begin
            --  Contruct the calls for the components

            Collect_Info_For_Component
              (Global_Prefix & "_E", Comp_Ty,
               Component_Read, Component_Write,
               Component_Size, Component_Size_Max, 1);

            --  Generate the basic operations

            declare
               Assocs : constant Translate_Table :=
                 [1  => Assoc ("TY_NAME", Ty_Name),
                  2  => Assoc ("TY_PREFIX", Ty_Prefix),
                  3  => Assoc ("GLOBAL_PREFIX", Global_Prefix),
                  4  => Assoc ("COMPONENT_READ", Component_Read),
                  5  => Assoc ("COMPONENT_WRITE", Component_Write),
                  6  => Assoc ("COMPONENT_SIZE", Component_Size),
                  7  => Assoc ("COMPONENT_SIZE_MAX", Component_Size_Max),
                  8  => Assoc ("COMP_TYP", Named_Comp_Ty.Type_Name),
                  9  => Assoc ("ADA_DIM", Ada_Dim_Tag),
                  10 => Assoc ("FIRST_NAME", First_Name_Tag),
                  11 => Assoc ("LAST_NAME", Last_Name_Tag),
                  12 => Assoc ("BOUND_TYP", Comp_Typ_Tag)];

            begin
               Put_Line (F_Body, Parse (Array_Read_Write_Template, Assocs));
               New_Line (F_Body);
               Put_Line (F_Body, Parse (Array_Size_Template, Assocs));
               New_Line (F_Body);
               Put_Line (F_Body, Parse (Array_Size_Max_Template, Assocs));
               New_Line (F_Body);
            end;
         end;

      --  4.3 For record types, we generate the calls for the components and
      --      the variant part and instanciate the appropriate patterns.

      --  Record types: generate a call per component

      else
         pragma Assert (Typ in Record_Typ'Class);

         declare
            Object_Name        : constant String := Global_Prefix & "_V";
            Component_Read     : Tag;
            Component_Write    : Tag;
            Component_Size     : Tag;
            Component_Size_Max : Tag;
            Variant_Read       : Tag;
            Variant_Write      : Tag;
            Variant_Size       : Tag;
            Variant_Size_Max   : Tag;

         begin
            --  Contruct the calls for the components

            Collect_Info_For_Components
              (Record_Typ'Class (Typ).Component_Types,
               Component_Read, Component_Write,
               Component_Size, Component_Size_Max,
               Object_Name => Object_Name,
               Spacing     => 0);

            --  Contruct the calls for the variant part if any

            if Typ in Discriminated_Record_Typ'Class then
               declare
                  D_Typ : Discriminated_Record_Typ'Class renames
                    Discriminated_Record_Typ'Class (Typ);
               begin
                  if D_Typ.Variant /= null then
                     Collect_Info_For_Variants
                       (D_Typ.Variant.all,
                        D_Typ.Discriminant_Types,
                        Variant_Read, Variant_Write,
                        Variant_Size, Variant_Size_Max,
                        Object_Name => Object_Name,
                        Spacing     => 0);
                  end if;
               end;
            end if;

            --  Generate the basic operations

            declare
               Assocs : constant Translate_Table :=
                 [1 => Assoc ("TY_NAME", Ty_Name),
                  2  => Assoc ("TY_PREFIX", Ty_Prefix),
                  3  => Assoc ("GLOBAL_PREFIX", Global_Prefix),
                  4  => Assoc ("COMPONENT_READ", Component_Read),
                  5  => Assoc ("COMPONENT_WRITE", Component_Write),
                  6  => Assoc ("COMPONENT_SIZE", Component_Size),
                  7  => Assoc ("COMPONENT_SIZE_MAX", Component_Size_Max),
                  8  => Assoc ("VARIANT_READ", Variant_Read),
                  9  => Assoc ("VARIANT_WRITE", Variant_Write),
                  10 => Assoc ("VARIANT_SIZE", Variant_Size),
                  11 => Assoc ("VARIANT_SIZE_MAX", Variant_Size_Max),
                  12 => Assoc ("DISCR_NAME", Discr_Name_Tag),
                  13 => Assoc ("DISCR_TYP", Comp_Typ_Tag)];

            begin
               Put_Line (F_Body, Parse (Record_Read_Write_Template, Assocs));
               New_Line (F_Body);
               Put_Line (F_Body, Parse (Record_Size_Template, Assocs));
               New_Line (F_Body);
               Put_Line (F_Body, Parse (Record_Size_Max_Template, Assocs));
               New_Line (F_Body);
            end;
         end;
      end if;

      --  4. Generate the wrappers for writing both the header and the
      --     components if necessary.

      if Needs_Wrappers (Typ) then
         declare
            Assocs : constant Translate_Table :=
              [1 => Assoc ("TY_NAME", Ty_Name),
               2 => Assoc ("TY_PREFIX", Ty_Prefix),
               3 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
               4 => Assoc ("DISCR_NAME", Discr_Name_Tag),
               5 => Assoc ("DISCR_TYP", Comp_Typ_Tag),
               6 => Assoc ("DISCR_PREFIX", Comp_Pref_Tag)];

         begin
            Put_Line (F_Spec, Parse (Header_Wrappers_Spec_Template, Assocs));
            New_Line (F_Spec);
            Put_Line (F_Body, Parse (Header_Wrappers_Body_Template, Assocs));
            New_Line (F_Body);
         end;
      end if;
   end Generate_Base_Functions_For_Typ;

   --------------------------------------------
   -- Generate_Marshalling_Functions_For_Typ --
   --------------------------------------------

   procedure Generate_Marshalling_Functions_For_Typ
     (F_Spec, F_Body : File_Type; Typ : TGen.Types.Typ'Class)
   is
      Ty_Name       : constant String := Typ.Type_Name;
      Ty_Prefix     : constant String := Prefix_For_Typ (Ty_Name);
      Generic_Name  : constant String :=
        (if Needs_Header (Typ) then "In_Out_Unconstrained" else "In_Out");
      Assocs        : constant Translate_Table :=
        [1 => Assoc ("TY_NAME", Ty_Name),
         2 => Assoc ("TY_PREFIX", Ty_Prefix),
         3 => Assoc ("MARSHALLING_LIB", Marshalling_Lib),
         4 => Assoc ("GENERIC_NAME", Generic_Name),
         5 => Assoc ("GLOBAL_PREFIX", Global_Prefix),
         6 => Assoc ("NEEDS_HEADER", Needs_Header (Typ)),
         7 => Assoc ("IS_SCALAR", Typ in Scalar_Typ'Class)];

   begin
      --  Generate the base functions for Typ

      Generate_Base_Functions_For_Typ (F_Spec, F_Body, Typ);

      --  Generate the Input and Output subprograms

      Put_Line (F_Spec, Parse (In_Out_Spec_Template, Assocs));
      New_Line (F_Spec);

      Put_Line (F_Body, Parse (In_Out_Body_Template, Assocs));
      New_Line (F_Body);
   end Generate_Marshalling_Functions_For_Typ;

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

      elsif Typ in Anonymous_Typ'Class then

         --  We don't support real constraints yet, as they are (incorrectly)
         --  handled using Long_Float by libadalang.

         if Anonymous_Typ'Class (Typ).Named_Ancestor.Get in Real_Typ'Class then
            Ada.Text_IO.Put_Line ("real constraints");
            return False;
         else
            return Is_Supported_Type
              (Anonymous_Typ'Class (Typ).Named_Ancestor.Get);
         end if;

      else
         Ada.Text_IO.Put_Line (Typ.Image);
         Ada.Text_IO.Put_Line (Kind (Typ)'Image);
         return False;
      end if;
   end Is_Supported_Type;

   ------------------
   -- Needs_Header --
   ------------------

   function Needs_Header (Typ : TGen.Types.Typ'Class) return Boolean is
     (Typ in Unconstrained_Array_Typ'Class
      or else (Typ in Discriminated_Record_Typ'Class
            and then not Discriminated_Record_Typ'Class (Typ).Constrained));

   --------------------
   -- Needs_Wrappers --
   --------------------

   function Needs_Wrappers (Typ : TGen.Types.Typ'Class) return Boolean is
     (Typ in Discriminated_Record_Typ'Class
      and then not Discriminated_Record_Typ'Class (Typ).Constrained
      and then Discriminated_Record_Typ'Class (Typ).Mutable);

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (V   : TGen.Types.Big_Integer;
      Typ : TGen.Types.Typ'Class) return String
   is
   begin
      if Typ in Enum_Typ'Class then
         return Lit_Image (Enum_Typ'Class (Typ), V);
      else
         return Trim (To_String (V), Left);
      end if;
   end String_Value;

end TGen.Marshalling;
