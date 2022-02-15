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

with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with TGen.Random;
with GNAT.Random_Numbers;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Random_Numbers;
pragma Warnings (On, "* is an internal GNAT unit");

package body TGen.Strategies is

   Generator_Instance : GNAT.Random_Numbers.Generator;

   package body Random_Strategy_Wrapper_Generic is
      procedure Gen
        (Strat  : in out Random_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Output (Stream, Gen);
      end Gen;
   end Random_Strategy_Wrapper_Generic;

   package body State_Strategy_Wrapper_Generic is
      procedure Gen
        (Strat  : in out State_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class) is
      begin
         T1'Output (Stream, Gen (Strat.S));
      end Gen;
   end State_Strategy_Wrapper_Generic;

   package body Dispatching_Strategy_Wrapper_Generic is

      procedure Gen
        (Strat  : in out Dispatching_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class) is
      begin
         if TGen.Random.Rand_Float <= Bias then
            Strat.S1.Gen (Stream);
         else
            Strat.S2.Gen (Stream);
         end if;
      end Gen;

   end Dispatching_Strategy_Wrapper_Generic;

begin
   GNAT.Random_Numbers.Reset (Generator_Instance);
end TGen.Strategies;
