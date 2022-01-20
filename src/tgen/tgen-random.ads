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

with Interfaces; use Interfaces;

with GNAT.Random_Numbers;

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

end TGen.Random;
