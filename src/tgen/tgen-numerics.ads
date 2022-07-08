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
--  Numeric utility functions for Big integers

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;

with Ada.Numerics.Generic_Elementary_Functions;

package TGen.Numerics is

   package Big_Int renames Ada.Numerics.Big_Numbers.Big_Integers;

   package Big_Reals renames Ada.Numerics.Big_Numbers.Big_Reals;

   function From_Universal_Image (Num, Den : String) return Big_Reals.Big_Real
   is
     (Big_Reals."/" (Big_Int.From_String (Num), Big_Int.From_String (Den)));

   package LF_Conversions is
     new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions (Long_Float);
   --  We use Long_Float instead of Long_Long_Float for random generation
   --  purposes because there is a bug Float_Conversions which results in
   --  Storage_Errors when using the package instantiated with Long_Long_Float.

   package LF_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   package LLLI_Conversions is
     new Big_Int.Signed_Conversions (Long_Long_Long_Integer);

   package Nat_Conversions is
      new Big_Int.Signed_Conversions (Natural);

   function Log
     (N    : Big_Int.Big_Integer;
      Base : Long_Float) return Integer is
     (Integer
        (LF_Functions.Log
             (Long_Float
                  (LLLI_Conversions.From_Big_Integer (N)),
              Base)));

end TGen.Numerics;
