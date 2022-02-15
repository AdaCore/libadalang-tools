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

with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Streams; use Ada.Streams;

with TGen.Types; use TGen.Types;

package TGen.Strategies is

   type Strategy_Wrapper_Type is abstract new Ada.Finalization.Controlled
   with null record;

   type Strategy_Wrapper_Acc is access Strategy_Wrapper_Type'Class;

   procedure Gen
     (Strat  : in out Strategy_Wrapper_Type;
      Stream : access Root_Stream_Type'Class)
   is abstract;

   type Random_Strategy_Wrapper_Type is
     abstract new Strategy_Wrapper_Type with null record;

   generic
      type T (<>) is private;
      with function Gen return T is <>;
   package Random_Strategy_Wrapper_Generic is
      type Random_Strategy_Wrapper_Generic_Type is
        new Random_Strategy_Wrapper_Type with null record;
      overriding procedure Gen
        (Strat  : in out Random_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Strategy_Wrapper_Generic_Type;
   end Random_Strategy_Wrapper_Generic;

   generic
      type T1 (<>) is private;
      type State is new Ada.Finalization.Controlled with private;
      with function Gen (S : in out State) return T1 is <>;
   package State_Strategy_Wrapper_Generic is
      type State_Strategy_Wrapper_Generic_Type is
        new Strategy_Wrapper_Type with record
         S : State;
      end record;
      overriding procedure Gen
        (Strat  : in out State_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased State_Strategy_Wrapper_Generic_Type;
   end State_Strategy_Wrapper_Generic;

   generic
      type S1 is new Strategy_Wrapper_Type with private;
      type S2 is new Strategy_Wrapper_Type with private;

      Bias : Float;
      --  Probability for picking the first strategy (between 0 and 1)

   package Dispatching_Strategy_Wrapper_Generic is

      type Dispatching_Strategy_Wrapper_Generic_Type is new
        Strategy_Wrapper_Type with record
         S1 : Strategy_Wrapper_Acc;
         S2 : Strategy_Wrapper_Acc;
      end record;

      overriding procedure Gen
        (Strat  : in out Dispatching_Strategy_Wrapper_Generic_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Dispatching_Strategy_Wrapper_Generic_Type;

   end Dispatching_Strategy_Wrapper_Generic;

end TGen.Strategies;
