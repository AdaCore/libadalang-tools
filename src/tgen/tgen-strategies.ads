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

with Ada.Containers.Vectors;
with Ada.Streams; use Ada.Streams;

with TGen.Types; use TGen.Types;

package TGen.Strategies is

   type Strategy_Type is interface;

   procedure Gen
     (Strat : Strategy_Type; Stream : access Root_Stream_Type'Class)
   is abstract;

   type Random_Strategy_Type is abstract new Strategy_Type with null record;

   generic
      type T is private;
      with function Gen return T is <>;
   package Random_Strategy_Generic is
      type Random_Strategy_Generic_Type is
        new Random_Strategy_Type with null record;
      overriding procedure Gen
        (Strat : Random_Strategy_Generic_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Strategy_Generic_Type;
   end Random_Strategy_Generic;

   generic
      type T is (<>);
   function Random_Discrete_Gen return T;

   generic
      type T is (<>);
   package Random_Discrete_Strategy is
      type Random_Discrete_Strategy_Type is
        new Random_Strategy_Type with null record;
      overriding procedure Gen
        (Strat : Random_Discrete_Strategy_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Discrete_Strategy_Type;
   end Random_Discrete_Strategy;

   generic
      type T is digits <>;
   package Random_Float_Strategy is
      type Random_Float_Strategy_Type is
        new Random_Strategy_Type with null record;
      overriding procedure Gen
        (Strat : Random_Float_Strategy_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Float_Strategy_Type;
   end Random_Float_Strategy;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function Gen return Element_Type is <>;
   function Random_Constrained_Array_Gen
        (LB : Index_Type; UB : Index_Type) return Array_Type;

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function Gen return Element_Type is <>;

   package Random_Unconstrained_Array_Strategy is
      type Random_Unconstrained_Array_Strategy_Type is
        new Random_Strategy_Type with null record;
      overriding procedure Gen
        (Strat : Random_Unconstrained_Array_Strategy_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Unconstrained_Array_Strategy_Type;
   end Random_Unconstrained_Array_Strategy;

   type Strategy_Acc is access all Strategy_Type'Class;

   package Strategy_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => Strategy_Acc);

   generic
      Strategies : Strategy_Vectors.Vector;
   package Random_Record_Strategy is
      type Random_Record_Strategy_Type is
        new Random_Strategy_Type with null record;
      overriding procedure Gen
        (Strat : Random_Record_Strategy_Type;
         Stream : access Root_Stream_Type'Class);
      Strat : aliased Random_Record_Strategy_Type;
   end Random_Record_Strategy;

end TGen.Strategies;
