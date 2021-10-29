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

package body TGen.Strategies is

   package body Random_Strategy_Generic is
      procedure Gen
        (Strat : Random_Strategy_Generic_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Write (Stream, Gen);
      end Gen;
   end Random_Strategy_Generic;

   package body Random_Discrete_Strategy is

      function Gen return T;

      function Gen return T is
         package Rand_Int is new Ada.Numerics.Discrete_Random (T);
         use Rand_Int;
         Gen : Generator;
      begin
         Reset (Gen);
         return Random (Gen);
      end Gen;

      procedure Gen
        (Strat : Random_Discrete_Strategy_Type;
         Stream : access Root_Stream_Type'Class)
      is
      begin
         T'Write (Stream, Gen);
      end Gen;
   end Random_Discrete_Strategy;

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

end TGen.Strategies;
