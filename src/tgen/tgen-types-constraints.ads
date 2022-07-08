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
--
--  Type representation for type constraints and anonymous subtypes, along with
--  generation strategies

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Unchecked_Deallocation;

with TGen.Context;    use TGen.Context;
with TGen.Strategies; use TGen.Strategies;

package TGen.Types.Constraints is

   type Constraint is abstract tagged null record;

   function Image (Self : Constraint) return String is abstract;

   function Static (Self : Constraint) return Boolean is abstract;
   --  Whether the constraints are suitable for static (single pass) generation

   type Constraint_Acc is access all Constraint'Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Constraint'Class, Constraint_Acc);

   type Constraint_Value_Kind is (Static, Non_Static, Discriminant);
   --  Constraint kind. Discriminant means that the constraint value is the
   --  value of one of the discriminants of the enclosing record type. Does not
   --  make sense if the constraints are not applied to a component of a
   --  discriminated record type.

   type Discrete_Constraint_Value (Kind : Constraint_Value_Kind := Non_Static)
   is record
      case Kind is
         when Static =>
            Int_Val : Big_Int.Big_Integer;
            --  The Static integer value of the constraint

         when Non_Static =>
            Text : Unbounded_Text_Type;
            --  The textual representation of the constraint

         when Discriminant =>
            Disc_Name : Unbounded_Text_Type;
            --  The defining name of the discriminant that appears in this
            --  context.

      end case;
   end record;
   --  A constraint value for a discrete type

   subtype Real_Constraint_Value_Kind is
     Constraint_Value_Kind range Static .. Non_Static;

   type Real_Constraint_Value (Kind : Real_Constraint_Value_Kind := Non_Static)
   is record
      case Kind is
         when Static =>
            Real_Val : Big_Reals.Big_Real;
            --  The static real value of the constraint

         when Non_Static =>
            Text : Unbounded_Text_Type;
            --  Textual representation of the constraint

      end case;
   end record;
   --  A constraint_Value for a real type

   type Discrete_Range_Constraint is new Constraint with record
      Low_Bound, High_Bound : Discrete_Constraint_Value;
   end record;
   --  A discrete range constraint. In case the range is not static, and the
   --  range expression is not in the form Low_Bound .. High_Bound
   --  (for instance a range attribute reference), then the low bound text is
   --  left empty and the high bound text contains the text for the whole range
   --  constraint. This is to avoid including text that is not present in
   --  in the declaration of the type, which could be unuseable.

   function Image (Self : Discrete_Range_Constraint) return String;

   function Static (Self : Discrete_Range_Constraint) return Boolean is
     (Self.Low_Bound.Kind in Static | Discriminant
      and then Self.High_Bound.Kind in Static | Discriminant);
   --  A discriminant constraint is considered as static because we get to
   --  choose its value.

   type Real_Range_Constraint is new Constraint with record
      Low_Bound, High_Bound : Real_Constraint_Value;
   end record;
   --  A Real range constraint. In case the range is not static, and the
   --  range expression is not in the form Low_Bound .. High_Bound
   --  (for instance a range attribute reference), then the low bound text is
   --  left empty and the high bound text contains the text for the whole range
   --  constraint. This is to avoid including text that is not present in
   --  in the declaration of the type, which could be unuseable.

   function Image (Self : Real_Range_Constraint) return String;

   function Static (Self : Real_Range_Constraint) return Boolean is
     (Self.Low_Bound.Kind = Static and then Self.High_Bound.Kind = Static);

   type Digits_Constraint (Has_Range : Boolean) is new Constraint
   with record
      Digits_Value : Discrete_Constraint_Value;
      case Has_Range is
         when True =>
            Range_Value : Real_Range_Constraint;
         when False =>
            null;
      end case;
   end record;
   --  Digits constraints for a real type. The range is optional.

   function Image (Self : Digits_Constraint) return String;

   function Static (Self : Digits_Constraint) return Boolean is
     (Self.Digits_Value.Kind = Static
      and then (if Self.Has_Range then Self.Range_Value.Static));

   type Index_Constraint (Present : Boolean := False) is record
      case Present is
         when True =>
            Discrete_Range : Discrete_Range_Constraint;
         when others =>
            null;
      end case;
   end record;
   --  Constraints for one index of an array. If the constraints are static
   --  then these contains the discrete range that constrain this index.
   --  If absent is true, this means that the index is constrained on the whole
   --  range of the index type.

   function Static (Self : Index_Constraint) return Boolean is
      (if Self.Present then Self.Discrete_Range.Static);

   type Index_Constraint_Arr is array (Positive range <>) of Index_Constraint;

   type Index_Constraints (Num_Dims : Positive) is new Constraint with record
      Constraint_Array : Index_Constraint_Arr (1 .. Num_Dims);
   end record;
   --  Constraints for a whole array, on constraint per dimension.

   function Image (Self : Index_Constraints) return String;

   function Static (Self : Index_Constraints) return Boolean is
     (for all J in 1 .. Self.Num_Dims => Static (Self.Constraint_Array (J)));

   function Hash_Defining_Name
     (Node : LAL.Defining_Name) return Ada.Containers.Hash_Type is
       (Node.As_Ada_Node.Hash);

   package Discriminant_Constraint_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Discrete_Constraint_Value,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");
   subtype Discriminant_Constraint_Map is Discriminant_Constraint_Maps.Map;
   --  Maps to represent discriminant constraints. Each entry specifies the
   --  value given to the discriminant of which the defining name is used as
   --  key. For enumeration types used as discriminants, this is the 'Pos value
   --  of the corresponding literal.

   type Discriminant_Constraints is new Constraint with record
      Constraint_Map : Discriminant_Constraint_Map;
   end record;
   --  Constraints for a discriminated record type, maps from discriminant
   --  defining name to the value of the discriminant.

   function Image (Self : Discriminant_Constraints) return String;

   function Static (Self : Discriminant_Constraints) return Boolean is
     (for all Val of Self.Constraint_Map => Val.Kind in Static | Discriminant);

   type Anonymous_Typ is new Typ with record
      Named_Ancestor      : SP.Ref;
      Subtype_Constraints : Constraint_Acc;
   end record;

   function Kind (Self : Anonymous_Typ) return Typ_Kind is (Anonymous_Kind);

   function As_Anonymous_Typ (Self : SP.Ref) return Anonymous_Typ'Class is
     (Anonymous_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Anonymous_Kind);
   pragma Inline (As_Anonymous_Typ);

   function Image (Self : Anonymous_Typ) return String;

   function As_Named_Typ (Self : Anonymous_Typ) return SP.Ref;
   --  Return a copy of Self.Ancestor_Type but with the constraints applied
   --  to it. The returned type will have the name of the ancestor.
   --  This operation performs lots of map copies for record types so it may be
   --  relatively slow.

   function Supports_Static_Gen (Self : Anonymous_Typ) return Boolean is
     (Self.Named_Ancestor.Get.Supports_Static_Gen
      and then Self.Subtype_Constraints.Static);
   --  Wether values for this Typ can be statically generated

   overriding function Generate_Static
     (Self    : Anonymous_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;

   procedure Free_Content (Self : in out Anonymous_Typ);

end TGen.Types.Constraints;
