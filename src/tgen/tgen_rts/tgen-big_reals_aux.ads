------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

--  Contains the decimal fixed point conversion package, missing in the
--  Ada.Numerics.Big_Numbers.Big_Reals standard package. Remove when this is
--  implemented in the standard. Note that this is simply a copy of the
--  Ada.Numerics.Big_Numbers.Big_Reals.Fixed_Conversions generic package with
--  a decimal fixed point formal parameter instead.

with TGen.Big_Reals; use TGen.Big_Reals;

package TGen.Big_Reals_Aux is

   generic
      type Num is delta <> digits <>;
   package Decimal_Fixed_Conversions is

      function To_Big_Real (Arg : Num) return Valid_Big_Real
      with Global => null;

      function From_Big_Real (Arg : Big_Real) return Num
      with
        Pre    =>
          In_Range
            (Arg,
             Low  => To_Big_Real (Num'First),
             High => To_Big_Real (Num'Last))
          or else (raise Constraint_Error),
        Global => null;

   end Decimal_Fixed_Conversions;

end TGen.Big_Reals_Aux;
