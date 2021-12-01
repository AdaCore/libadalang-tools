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

with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Random_Numbers;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Random_Numbers;
pragma Warnings (On, "* is an internal GNAT unit");

package body TGen.Strategies is

   Generator_Instance : GNAT.Random_Numbers.Generator;

   package body Random_Strategy_Generic is
      procedure Gen
        (Strat : Random_Strategy_Generic_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Write (Stream, Gen);
      end Gen;
   end Random_Strategy_Generic;

   function Random_Discrete_Gen return T
   is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (T);
   begin
      return Rand (Generator_Instance);
   end Random_Discrete_Gen;

   procedure Gen_Strategy_Impl
     (T : Typ;
      Strat_Fun : Identifier)
   is
   begin
      null;
   end Gen_Strategy_Impl;

   package body Random_Discrete_Strategy is

      function Rand is new GNAT.Random_Numbers.Random_Discrete (T);

      function Gen return T;

      function Gen return T is
      begin
         return Rand (Generator_Instance);
      end Gen;

      procedure Gen
        (Strat : Random_Discrete_Strategy_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Write (Stream, Gen);
      end Gen;
   end Random_Discrete_Strategy;

   package body Random_Float_Strategy is

      function Gen_Fractional return T;

      function Gen_Fractional return T is
         function Rand_Float is new
           GNAT.Random_Numbers.Random_Float (Long_Float);
         A : constant Long_Float :=
           Long_Float'(GNAT.Random_Numbers.Random (Generator_Instance));
         E : Long_Float := Long_Float (T'Last);
         F : Long_Float :=
           (Long_Float (T'Last) - Long_Float (T'First))
             * Rand_Float (Generator_Instance) + Long_Float (T'First);
      begin
         Put_Line ("F last is " & T'Image (T'Last));
         Put_Line ("F digits is " & Integer'Image (T'Digits));
         Put_Line ("Cast value is " & Long_Float'Image (E));
         Put_Line ("Generated value is " & Long_Float'Image (F));
         return T'Machine (T (F));
      end Gen_Fractional;

      procedure Gen
        (Strat : Random_Float_Strategy_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Write (Stream, Gen_Fractional);
      end Gen;

   end Random_Float_Strategy;

   function Random_Constrained_Array_Gen
     (LB : Index_Type; UB : Index_Type) return Array_Type is
      Res : Array_Type (LB .. UB);
   begin
      for I in LB .. UB loop
         Res (I) := Gen;
      end loop;
      return Res;
   end Random_Constrained_Array_Gen;

   package body Random_Unconstrained_Array_Strategy is

      function Pull_Element return Boolean is (False);

      function Gen return Array_Type;

      function Gen return Array_Type is
         Last : Index_Type := Index_Type'First;
      begin
         while Last < Index_Type'Last and then Pull_Element loop
            Last := Index_Type'Succ (Last);
         end loop;

         --  Now declare the returned array: start at the lower bound of the
         --  type.

         --  Check first if we have a zero-sized array. If we were to do
         --  Index_Type'Pred on Index_Type'First, it will overflow.

         if Last = Index_Type'First then
            declare
               --  We make the assumption here that the Index_Type is not a
               --  singleton, otherwise we are returning an unitialized
               --  singleton array.

               Res : Array_Type (Index_Type'Last .. Index_Type'First);
            begin
               return Res;
            end;
         else
            declare
               Res : Array_Type (Index_Type'First .. Index_Type'Pred (Last));
            begin
               for I in Res'Range loop
                  Res (I) := Gen;
               end loop;
               return Res;
            end;
         end if;
      end Gen;

      procedure Gen
        (Strat : Random_Unconstrained_Array_Strategy_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         Array_Type'Output (Stream, Gen);
      end Gen;
   end Random_Unconstrained_Array_Strategy;

   package body Random_Record_Strategy is

      procedure Gen
        (Strat : Random_Record_Strategy_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         for Strategy of Strategies loop
            Strategy.Gen (Stream);
         end loop;
      end Gen;
   end Random_Record_Strategy;

begin
   GNAT.Random_Numbers.Reset (Generator_Instance);
end TGen.Strategies;
