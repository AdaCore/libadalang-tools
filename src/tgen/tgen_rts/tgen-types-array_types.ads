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
--  Type representation for arrays, and associated generation strategies

with TGen.Strategies;        use TGen.Strategies;
with TGen.Types.Constraints; use TGen.Types.Constraints;

package TGen.Types.Array_Types is

   Unconstrained_Array_Size_Min : constant Natural := 0;
   Unconstrained_Array_Size_Max : constant Natural := 10;
   --  Min / max size of generated unconstrained arrays. Hardcoded at the
   --  moment.

   Low_Bound_Disc_Name : constant Unbounded_String :=
     +String'("Magic_TGen_Low_Bound");
   High_Bound_Disc_Name : constant Unbounded_String :=
     +String'("Magic_TGen_High_Bound");

   type Index_Typ_Arr is array (Positive range <>) of TGen.Types.SP.Ref;

   type Array_Typ (Num_Dims : Positive) is new Composite_Typ with record
      Index_Types : Index_Typ_Arr (1 .. Num_Dims);
      Component_Type : TGen.Types.SP.Ref;
      Static_Gen : Boolean := False;
   end record;
   --  Represents an array type.

   function Supports_Static_Gen (Self : Array_Typ) return Boolean is
     (Self.Static_Gen);
   --  Whether values for this Typ can be statically generated

   function Get_Diagnostics
     (Self   : Array_Typ;
      Prefix : String := "") return String_Vector;

   function Supports_Gen (Self : Array_Typ) return Boolean is
     (Self.Component_Type.Get.Supports_Gen);
   --  Index types are discrete; we can always generate them, only the
   --  component type could prevent us from doing so.

   function As_Array_Typ (Self : SP.Ref)
     return Array_Typ'Class is
     (Array_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
       and then (Self.Get.Kind in Array_Typ_Range);
   pragma Inline (As_Array_Typ);

   type Unconstrained_Array_Typ is new Array_Typ with null record;

   function Image (Self : Unconstrained_Array_Typ) return String;

   function Kind (Self : Unconstrained_Array_Typ) return Typ_Kind is
     (Unconstrained_Array_Kind);

   function Default_Strategy
     (Self : Unconstrained_Array_Typ) return Strategy_Type'Class;
   --  Generate a strategy for static (single pass) generation for an
   --  Unconstrained_Array_Typ

   overriding function Encode
     (Self : Unconstrained_Array_Typ; Val : JSON_Value) return JSON_Value;

   function As_Unconstrained_Array_Typ (Self : SP.Ref)
     return Unconstrained_Array_Typ'Class is
     (Unconstrained_Array_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Unconstrained_Array_Kind);
   pragma Inline (As_Unconstrained_Array_Typ);

   type Constrained_Array_Typ (Num_Dims : Positive) is new
     Array_Typ (Num_Dims) with record
      Index_Constraints : Index_Constraint_Arr (1 .. Num_Dims);
   end record;
   --  Represents a constrained array type

   function Image (Self : Constrained_Array_Typ) return String;

   function Kind (Self : Constrained_Array_Typ) return Typ_Kind is
     (Constrained_Array_Kind);

   overriding function Default_Strategy
     (Self : Constrained_Array_Typ) return Strategy_Type'Class;
   --  Generate a strategy for the generation for a Constrained_Array_Typ

   type Constr_Array_Enum_Strat is new Enum_Strategy_Type with record
      Arr_T : SP.Ref;
      --  Type of the array for which we are generating values

      Comp_Strat : Enum_Strategy_Type_Acc;
      --  Enumerative strategy for the component type

      Has_Generated : Boolean;
      --  There's only a single value to be generated as the size of the array
      --  is fixed, use this flag to determine wether we have already generated
      --  it or not.
   end record;
   --  Strategy to generate a value for a constrained array, using an
   --  enumerative strategy for the component. Computing permutations among the
   --  various elements of the array would result in a combinatorial explosion,
   --  so the strategy only generates a single value for the type, consisting
   --  of an array with the elements generated in sequence from the component
   --  strat, starting over if there are more elements in the array than what
   --  the strategy can generate.

   procedure Init (S : in out Constr_Array_Enum_Strat);

   overriding function Has_Next (S : Constr_Array_Enum_Strat) return Boolean;

   overriding function Generate
     (S            : in out Constr_Array_Enum_Strat;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   overriding function Default_Enum_Strategy
     (Self : Constrained_Array_Typ) return Enum_Strategy_Type'Class;
   --  Generate an enumerative strategy for the generation of a
   --  Constrained_Array_Typ. The default enumerative strategy yields a single
   --  array whose components are generated using the component's type default
   --  enumerative strategy. Note that there is no combinatorial enumeration
   --  over the component values: we simply use the component's type strategy
   --  and if we have exhausted all of its elements, we copy an arbitrary
   --  previously generated value.

   type Nat_Array is array (Natural range <>) of Natural;

   type Unconst_Array_Enum_Strat (Num_Sizes : Positive) is
     new Constr_Array_Enum_Strat with record
      Sizes : Nat_Array (1 .. Num_Sizes);
      --  Target length for each of the values to be generated. This is the
      --  same length for each dimension, and is clipped depending on the
      --  actual bounds of the index type.

      Current_Size : Natural;
      --  Index in the above array to keep track of what the next size we will
      --  need to generate is.
   end record;

   procedure Init (S : in out Unconst_Array_Enum_Strat);

   overriding function Has_Next (S : Unconst_Array_Enum_Strat) return Boolean;

   overriding function Generate
     (S            : in out Unconst_Array_Enum_Strat;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   function Make_Unconst_Array_Enum_Strat
     (Arr_T : Unconstrained_Array_Typ;
      Sizes : Nat_Array) return Unconst_Array_Enum_Strat'Class;

   overriding function Default_Enum_Strategy
     (Self : Unconstrained_Array_Typ) return Enum_Strategy_Type'Class;
   --  Generate a enumerative strategy for the generation of a
   --  Unconstrained_Array_Typ. This operates in the same manner as the
   --  constrained array default enumerative strategy, excepts that we generate
   --  arrays of four different sizes: 0, 1, 10 and 1000. This is bounded
   --  by the unconstrained array index type so the maximal-size array can be
   --  less.

   procedure Is_Constrained_By_Variable
     (Self       : Constrained_Array_Typ;
      Var_Name   : Unbounded_String;
      Found      : out Boolean;
      Constraint : out TGen.Types.Constraints.Index_Constraint);
   --  Return whether the type is constrained by the variable named Var_Name.
   --  For instance:
   --
   --  I : Integer := 1;
   --  type A is array (1 .. I) of Integer; --  A is constrained by I

   function As_Constrained_Array_Typ (Self : SP.Ref)
     return Constrained_Array_Typ'Class is
     (Constrained_Array_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Constrained_Array_Kind);
   pragma Inline (As_Constrained_Array_Typ);

end TGen.Types.Array_Types;
