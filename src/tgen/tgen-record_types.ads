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

with Libadalang.Analysis;

with TGen.Types; use TGen.Types;
with TGen.Int_Types;

package TGen.Record_Types is

   package LAL renames Libadalang.Analysis;

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
   --  not specified, initiallyzing a record with a positional aggregate will
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

   --  WIP
   --  type Discriminant_Constraint (Static : Boolean := False) is record
   --     case Static is
   --        when True =>
   --           Val : Integer;
   --        when False =>
   --           Name : LAL.Defining_Name;
   --     end case;
   --  end record;
   --  --  Represents discriminant constraints. In case the value used to
   --   constrain
   --  --  a discriminant isn't static, fill it with

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

   type Shape_Arr is array (Positive range <>) of Shape;

   ---  WIP
   --  type Constraint_Value_Kind is (Static, Discriminant, Non_Static);

   --  type Constraint_Value (Kind : Constraint_Value_Kind := Non_Static) is
   --  record
   --     case Kind is
   --        when Static =>
   --           Int_Val : Integer;
   --           --  The static value of the constraint

   --        when Discriminant =>
   --           Disc_Val : LAL.Defining_Name;
   --           --  The defining name of the discriminant that appears in this
   --           --  context.

   --        when Non_Static =>
   --           null;
   --           --  We don't have any useful info that we can provide here.
   --           --  May be revisited.
   --     end case;
   --  end record;

   type Discriminated_Record_Typ (Num_Shapes : Positive) is
     new Record_Typ with record

      Discriminant_Types : Component_Maps.Map;
      --  Map from discriminant defining names to their type translation

      Shapes : Shape_Arr (1 .. Num_Shapes);
      --  Different (possible or not) shapes for the record, depending on the
      --  discrete choices expressed in the variant parts.

   end record;
   --  Num_Shapes is the number of distinct shapes the record can have,
   --  depending on the discrete choices expressed in each variant part.
   --
   --  The component Component_Types of a Discriminated_Record_Typ is the full
   --  set of components that are present in the type declaration
   --  (excluding discriminants which have their own map), but they are not
   --  all present for a given set of discriminant values.

   package Discriminant_Values_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => Integer,
      Hash            => Hash_Defining_Name,
      Equivalent_Keys => Equivalent_Keys);
   --  Maps to represent discriminant constraints. Each entry specifies the
   --  value given to the discriminant of which the defining name is used as
   --  key. For enumeration types used as discriminants, this is the 'Pos value
   --  of the corresponding literal.

   function Components
     (Self                : Discriminated_Record_Typ;
      Discriminant_Values : Discriminant_Values_Maps.Map)
      return Component_Maps.Map;
   --  Given a set of Discriminant_Values for the discriminants of Self, return
   --  the set of components that are actually present in the record.

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
