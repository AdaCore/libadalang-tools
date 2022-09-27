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
--  Utility functions around random number generators

with Interfaces; use Interfaces;

with GNAT.Random_Numbers;

with TGen.Numerics; use TGen.Numerics;

package TGen.Random is
   pragma Elaborate_Body;

   Generator_Instance : GNAT.Random_Numbers.Generator;

   function Draw_Bits (N : Positive) return Unsigned_128
     with Pre => N <= Unsigned_128'Size;
   --  Return N random Bits as an Unsigned_128, up to a maximum of 128 bits

   function Draw_Bits (N : Positive) return Unsigned_64
     with Pre => N <= Unsigned_64'Size;
   --  Return N random Bits as an Unsigned_64, up to a maximum of 64 bits

   function Biased_Coin (P_True : Float) return Boolean;
   --  Return True with probability P

   type Many_Type is tagged private;

   function Many
     (Min_Size, Max_Size, Average_Size : Natural) return Many_Type;
   --  Create a Many_Type which after calls to More until it returns False
   --  will have a value in range Min_Size .. Max_Size distributed according to
   --  a Bernoulli law.

   function Count (Elements : Many_Type) return Natural;
   --  Return the value associated with Elements

   function More (Self : in out Many_Type) return Boolean;
   --  Returns whether another value can be drawn from Self

   function Rand_Float return Float;
   --  Return a random number between 0 and 1

   function Rand_Int (Min, Max : Integer) return Integer;
   --  Returns a random integer

   function Rand_LLLI
     (Min, Max : Long_Long_Long_Integer) return Long_Long_Long_Integer;

   function Rand_BI
     (Min, Max : Big_Int.Big_Integer) return Big_Int.Big_Integer is
      (LLLI_Conversions.To_Big_Integer
         (Rand_LLLI
              (LLLI_Conversions.From_Big_Integer (Min),
               LLLI_Conversions.From_Big_Integer (Max))));
   --  Assumes that Min / Max fit on a Long_Long_Long_Integer

private
   type Many_Type is tagged record
      Min_Size, Max_Size : Natural;
      Count              : Natural;
      Stopping_Value     : Float;
   end record;

   function Count (Elements : Many_Type) return Natural is (Elements.Count);

end TGen.Random;
