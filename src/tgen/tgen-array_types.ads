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

with TGen.Types; use TGen.Types;

package TGen.Array_Types is

   type Array_Typ is new Composite_Typ with null record;

   type Index_Typ_Arr is array (Positive range <>) of TGen.Types.SP.Ref;

   type Index_Constraints (Present : Boolean := False) is record
      case Present is
         when True =>
            Discrete_Range : Discrete_Range_Constraint;
         when others =>
            null;
      end case;
   end record;

   type Index_Constraint_Arr is array (Positive range <>) of Index_Constraints;

   type Unconstrained_Array_Typ (Num_Dims : Positive) is new
     Array_Typ with record
      Index_Types    : Index_Typ_Arr (1 .. Num_Dims);
      Component_Type : TGen.Types.SP.Ref;
   end record;

   function Image (Self : Unconstrained_Array_Typ) return String;

   function Kind (Self : Unconstrained_Array_Typ) return Typ_Kind is
     (Unconstrained_Array_Kind);

   function As_Unconstrained_Array_Typ (Self : SP.Ref)
     return Unconstrained_Array_Typ'Class is
     (Unconstrained_Array_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Unconstrained_Array_Kind);
   pragma Inline (As_Unconstrained_Array_Typ);

   type Constrained_Array_Typ (Num_Dims : Positive) is new
     Array_Typ with record
      Index_Types : Index_Typ_Arr (1 .. Num_Dims);
      Index_Constraints : Index_Constraint_Arr (1 .. Num_Dims);
      Component_Type : TGen.Types.SP.Ref;
   end record;

   function Image (Self : Constrained_Array_Typ) return String;

   function Kind (Self : Constrained_Array_Typ) return Typ_Kind is
     (Constrained_Array_Kind);

   function As_Constrained_Array_Typ (Self : SP.Ref)
     return Constrained_Array_Typ'Class is
     (Constrained_Array_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Constrained_Array_Kind);
   pragma Inline (As_Constrained_Array_Typ);

end TGen.Array_Types;
