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
with TGen.Int_Types;

package TGen.Array_Types is

   type Array_Typ is new Composite_Typ with null record;

   type Index_Typ_Arr is array (Positive range <>) of TGen.Types.SP.Ref;

   type Index_Constraints
     (Present : Boolean := False; Static : Boolean := False) is record
      case Present is
         when True =>
            case Static is
               when True =>
                  Discrete_Range : TGen.Int_Types.Int_Range;
                  --  Index constraints are supposed to apply to any discrete
                  --  type, not only integers but we'll represent index
                  --  constraints on an enum type by the index of the
                  --  corresponding litterals that appear in the range.
               when others =>
                  null;
            end case;
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

   type Constrained_Array_Typ (Num_Dims : Positive) is new
     Array_Typ with record
      Index_Types : Index_Typ_Arr (1 .. Num_Dims);
      Index_Constraints : Index_Constraint_Arr (1 .. Num_Dims);
      Component_Type : TGen.Types.SP.Ref;
   end record;

   function Image (Self : Constrained_Array_Typ) return String;

end TGen.Array_Types;
