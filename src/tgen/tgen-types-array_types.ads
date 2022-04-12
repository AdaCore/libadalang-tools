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

with TGen.Context;           use TGen.Context;
with TGen.Strategies;        use TGen.Strategies;
with TGen.Types.Constraints; use TGen.Types.Constraints;

package TGen.Types.Array_Types is

   Unconstrained_Array_Size_Min : constant Natural := 0;
   Unconstrained_Array_Size_Max : constant Natural := 10;
   --  Min / max size of generated unconstrained arrays. Hardcoded at the
   --  moment.

   Low_Bound_Disc_Name : constant Unbounded_Text_Type :=
     +String'("Magic_TGen_Low_Bound");
   High_Bound_Disc_Name : constant Unbounded_Text_Type :=
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
   --  Wether values for this Typ can be statically generated

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

   function Generate_Static
     (Self    : Unconstrained_Array_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;
   --  Generate a strategy for static (single pass) generation for an
   --  Unconstrained_Array_Typ

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

   overriding function Generate_Static
     (Self    : Constrained_Array_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;
   --  Generate a strategy for static (single pass) generation for a
   --  Constrained_Array_Typ

   procedure Is_Constrained_By_Variable
     (Self       : Constrained_Array_Typ;
      Var_Name   : Unbounded_Text_Type;
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
