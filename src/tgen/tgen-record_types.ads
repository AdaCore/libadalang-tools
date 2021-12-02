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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Libadalang.Analysis;

with TGen.Types; use TGen.Types;
with TGen.Int_Types;

package TGen.Record_Types is

   package LAL renames Libadalang.Analysis;

   Discriminant_Value_Error : exception;
   --  Will be raised each time an illegal value is used for a discriminant,
   --  either because it is outside the bounds of the type of the discriminant
   --  or because it does not respect the discriminant constraints of a record.

   function Hash_Defining_Name
     (Node : LAL.Defining_Name) return Ada.Containers.Hash_Type is
       (Node.As_Ada_Node.Hash);

   function Equivalent_Keys (L, R : LAL.Defining_Name) return Boolean is
     (LAL."=" (L, R));

   package Component_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => SP.Ref,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => Equivalent_Keys,
      "="             => SP."=");
   --  Maps for discriminants and components, from their defining name to
   --  their type translation. Since the order of the elements in these maps is
   --  not specified, initialyzing a record with a positional aggregate will
   --  very likely result in an error, a named association should be used
   --  instead.

   type Record_Typ is new Composite_Typ with record

      Component_Types : Component_Maps.Map;

   end record;

   type Nondiscriminated_Record_Typ is new Record_Typ with null record;

   function Image (Self : Nondiscriminated_Record_Typ) return String;

   function Kind (Self : Nondiscriminated_Record_Typ) return Typ_Kind is
     (Non_Disc_Record_Kind);

   function As_Nondiscriminated_Record_Typ (Self : SP.Ref)
     return Nondiscriminated_Record_Typ'Class is
     (Nondiscriminated_Record_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Non_Disc_Record_Kind);
   pragma Inline (As_Nondiscriminated_Record_Typ);

   type Discriminant_Choice_Entry is record
      Defining_Name : LAL.Defining_Name;
      Choices       : LAL.Alternatives_List;
   end record;

   package Discriminant_Choices_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type        => Positive,
      Element_Type    => Discriminant_Choice_Entry);
   --  We cannot simply use a Defining_Name -> Choice_List map here because
   --  a discriminant may appear multiple time in the variant parts.

   type Shape is record

      Components : Component_Maps.Map;
      --  List of components present for the given variant choices

      Discriminant_Choices : Discriminant_Choices_Maps.Map;
      --  List of alternatives to be satisfied by each discriminent so that the
      --  record has the components defined above.

   end record;

   package Shape_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Positive, Element_Type => Shape);

   type Shape_Arr is array (Positive range <>) of Shape;

   package Discriminant_Constraint_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => Constraint_Value,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => Equivalent_Keys);
   --  Maps to represent discriminant constraints. Each entry specifies the
   --  value given to the discriminant of which the defining name is used as
   --  key. For enumeration types used as discriminants, this is the 'Pos value
   --  of the corresponding literal.

   type Discriminated_Record_Typ (Constrained : Boolean)
   is
     new Record_Typ with record

      Mutable : Boolean := False;
      --  Whether this is a mutable type or not.

      Discriminant_Types : Component_Maps.Map;
      --  Map from discriminant defining names to their type translation

      Shapes : Shape_Maps.Map;
      --  Different (possible or not) shapes for the record, depending on the
      --  discrete choices expressed in the variant parts.

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
   --  The component Component_Types of a Discriminated_Record_Typ is the full
   --  set of components that are present in the type declaration
   --  (excluding discriminants which have their own map), but they are not
   --  all present for a given set of discriminant values. To get the component
   --  that are present given a certain set of discriminants, use the
   --  Components function defined bellow.

   function Constraints_Respected
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Discriminant_Constraint_Maps.Map)
      return Boolean;
   --  Check whether the values given for the discriminants in
   --  Discriminant_Values respect the constraints that may already exist for
   --  Self. If Self has any non-static constraints or constraints bound to
   --  a discriminant of an enclosing type, then they are always considered to
   --  be satisfied.

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Discriminant_Constraint_Maps.Map)
      return Component_Maps.Map;
   --  Given a set of Discriminant_Values for the discriminants of Self, return
   --  the set of components that are actually present in the record.
   --  Note that this does not resolves the eventual constraints on the
   --  components that depend on discriminants.

   function Shape_Matches
     (Shp : Shape;
      Discriminant_Values : Discriminant_Constraint_Maps.Map) return Boolean;
    --  Given the set of (incomplete) discriminant values, return whether this
    --  shape is achievable.

   function Image (Self : Discriminated_Record_Typ) return String;

   function Kind (Self : Discriminated_Record_Typ) return Typ_Kind is
     (Disc_Record_Kind);

   function As_Discriminated_Record_Typ
     (Self : SP.Ref) return Discriminated_Record_Typ'Class is
     (Discriminated_Record_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Disc_Record_Kind);
   pragma Inline (As_Discriminated_Record_Typ);

end TGen.Record_Types;
