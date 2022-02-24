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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Libadalang.Analysis;

with TGen.Context;    use TGen.Context;
with TGen.Strategies; use TGen.Strategies;
with TGen.Types; use TGen.Types;
with TGen.Types.Int_Types;
with TGen.Types.Constraints; use TGen.Types.Constraints;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams; use Ada.Streams;

package TGen.Types.Record_Types is

   package LAL renames Libadalang.Analysis;

   Discriminant_Value_Error : exception;
   --  Will be raised each time an illegal value is used for a discriminant,
   --  either because it is outside the bounds of the type of the discriminant
   --  or because it does not respect the discriminant constraints of a record.

   package Component_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => SP.Ref,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => LAL."=",
      "="             => SP."=");
   subtype Component_Map is Component_Maps.Map;
   --  Maps for discriminants and components, from their defining name to
   --  their type translation. Since the order of the elements in these maps is
   --  not specified, initialyzing a record with a positional aggregate will
   --  very likely result in an error, a named association should be used
   --  instead.

   package Component_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => LAL.Defining_Name,
      "="          => LAL."=");
   subtype Component_Vector is Component_Vectors.Vector;

   type Record_Typ is new Composite_Typ with record

      Component_Types : Component_Maps.Map;

   end record;

   function Image (Self : Record_Typ) return String;

   --  overriding function Generate_Static
   --    (Self         : Record_Typ;
   --     Disc_Context : Disc_Value_Map) return String;

   function Image_Internal
     (Self    : Record_Typ;
      Padding : Natural := 0) return String;

   overriding function Generate_Static
     (Self    : Record_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;

   function As_Record_Typ (Self : SP.Ref)
     return Record_Typ'Class is
     (Record_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Record_Typ_Range);
   pragma Inline (As_Record_Typ);

   type Nondiscriminated_Record_Typ is new Record_Typ with null record;

   function Kind (Self : Nondiscriminated_Record_Typ) return Typ_Kind is
     (Non_Disc_Record_Kind);

   overriding function Generate_Static
     (Self    : Nondiscriminated_Record_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;

   function As_Nondiscriminated_Record_Typ (Self : SP.Ref)
     return Nondiscriminated_Record_Typ'Class is
     (Nondiscriminated_Record_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Non_Disc_Record_Kind);
   pragma Inline (As_Nondiscriminated_Record_Typ);

   --  overriding function Generate_Static
   --    (Self         : Nondiscriminated_Record_Typ;
   --     Disc_Context : Disc_Value_Map) return String;

   type Discriminant_Choice_Entry is record
      Defining_Name : LAL.Defining_Name;
      Choices       : LAL.Alternatives_List;
   end record;

   package Discriminant_Choices_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type        => Positive,
      Element_Type    => Discriminant_Choice_Entry);
   --  We cannot simply use a Defining_Name -> Choice_List map here because
   --  a discriminant may appear multiple time in the variant parts.

   package Alternatives_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => TGen.Types.Int_Types.Int_Range,
      "<"          => TGen.Types.Int_Types."<",
      "="          => TGen.Types.Int_Types."=");
   subtype Alternatives_Set is Alternatives_Sets.Set;

   package Alternatives_Set_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Alternatives_Set,
      "="          => Alternatives_Sets."=");
   subtype Alternatives_Set_Vector is Alternatives_Set_Vectors.Vector;

   type Variant_Part;

   type Variant_Part_Acc is access Variant_Part;

   type Variant_Choice is record
      Alternatives : LAL.Alternatives_List;
      Alt_Set      : Alternatives_Set;
      Components   : Component_Maps.Map;

      Variant : Variant_Part_Acc;
      --  Variant part associated to this variant choice. Null if there is no
      --  variant part in this variant choice.

   end record;

   procedure Free_Variant (Var : in out Variant_Part_Acc);

   function Clone (Var : Variant_Part_Acc) return Variant_Part_Acc;

   package Variant_Choice_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Variant_Choice);

   type Variant_Part is record
      Discr_Name : LAL.Defining_Name;
      Variant_Choices : Variant_Choice_Lists.List;
   end record;

   type Discriminated_Record_Typ (Constrained : Boolean)
   is
     new Record_Typ with record

      Mutable : Boolean := False;
      --  Whether this is a mutable type or not.

      Discriminant_Types : Component_Maps.Map;
      --  Map from discriminant defining names to their type translation

      Variant : Variant_Part_Acc;
      --  Variant part associated with the record. Null if there is no variant
      --  part in this record.

      case Constrained is
         when True =>
            Discriminant_Constraint : Discriminant_Constraint_Maps.Map;
            --  Constraints associated to this record type. Not all the
            --  defining names in Discriminant_Types will be present in this
            --  map, because discriminant correspondance (See RM 3.7 (18))
            --  defined in type derivation are represented by simply "renaming"
            --  one of the discriminants of the ancestor part, which is then
            --  not constrained.

         when others =>
            null;
      end case;
   end record;
   --  The component Component_Types of a Discriminated_Record_Typ is the set
   --  of components that are always present no matter the values of the
   --  discriminants (excluding discriminants which have their own map)

   function Constraints_Respected
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Disc_Value_Map)
      return Boolean;
   --  Check whether the values given for the discriminants in
   --  Discriminant_Values respect the constraints that may already exist for
   --  Self. If Self has any non-static constraints or constraints bound to
   --  a discriminant of an enclosing type, then they are always considered to
   --  be satisfied.

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Disc_Value_Map)
      return Component_Maps.Map;
   --  Given a set of Discriminant_Values for the discriminants of Self, return
   --  the set of components that are actually present in the record.
   --  Raises Discriminant_Value_Error if the unique set of components cannot
   --  be determined from the list of discriminant values.

   function Image (Self : Discriminated_Record_Typ) return String;

   overriding function Generate_Static
     (Self    : Discriminated_Record_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;

   overriding function Generate_Random_Strategy
     (Self    : Discriminated_Record_Typ;
      Context : in out Generation_Context) return Strategy_Type'Class;

   overriding function Generate_Constrained_Random_Strategy
     (Self    : Discriminated_Record_Typ;
      Context : Generation_Context) return Strategy_Type'Class;

   function Get_All_Components
     (Self : Discriminated_Record_Typ) return Component_Map;
   --  Aggregates all the components of a record (as two components of a record
   --  cannot have the same name, even if they are in a distinct variant
   --  choice).

   function Image_Internal
     (Self : Discriminated_Record_Typ; Padding : Natural := 0) return String;

   function Kind (Self : Discriminated_Record_Typ) return Typ_Kind is
     (Disc_Record_Kind);

   procedure Free_Content (Self : in out Discriminated_Record_Typ);

   overriding function Is_Constrained
     (Self : Discriminated_Record_Typ) return Boolean
     is (True);

   function As_Discriminated_Record_Typ
     (Self : SP.Ref) return Discriminated_Record_Typ'Class is
     (Discriminated_Record_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Disc_Record_Kind);
   pragma Inline (As_Discriminated_Record_Typ);

end TGen.Types.Record_Types;
