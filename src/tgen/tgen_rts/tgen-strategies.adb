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

with Ada.Containers; use Ada.Containers;
with Ada.Tags;       use Ada.Tags;

with TGen.Random; use TGen.Random;

package body TGen.Strategies is

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Dispatching_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      Rnd : constant Float := Rand_Float;
   begin
      if Rnd <= S.Bias then
         return S.S1.Generate_Static_Value (Disc_Context);
      else
         return S.S2.Generate_Static_Value (Disc_Context);
      end if;
   end Generate_Static_Value;

   ----------------------------
   -- Make_Dispatching_Strat --
   ----------------------------

   function Make_Dispatching_Strat
     (S1, S2 : Static_Strategy_Type'Class;
      Bias   : Float := 0.5) return Dispatching_Static_Strategy_Type
   is
      Strat : Dispatching_Static_Strategy_Type;
   begin
      Strat.Bias := Bias;
      Strat.S1 := new Static_Strategy_Type'Class'(S1);
      Strat.S2 := new Static_Strategy_Type'Class'(S2);
      return Strat;
   end Make_Dispatching_Strat;

   package body Equivalence_Classes_Strategy_Package is

      ---------------------------
      -- Generate_Static_Value --
      ---------------------------

      function Generate_Static_Value
        (S            : in out Equivalence_Class_Strategy_Type;
         Disc_Context : Disc_Value_Map) return Static_Value'Class
      is
         Idx : constant Integer := Rand_Int (1, Integer (S.Classes.Length));
      begin
         return S.Draw (S.T, S.Classes.Element (Idx));
      end Generate_Static_Value;

   end Equivalence_Classes_Strategy_Package;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Strategy_Type'Class) return Boolean is
   begin

      --  TODO: code properly this function

      if L'Tag = R'Tag then
         if L'Tag = Dispatching_Static_Strategy_Type'Tag then
            if Dispatching_Static_Strategy_Type (L).S1.all <
              Dispatching_Static_Strategy_Type (R).S1.all
            then
               return True;
            else
               return Dispatching_Static_Strategy_Type (L).S1.all <
                 Dispatching_Static_Strategy_Type (R).S1.all;
            end if;
         elsif L'Tag = Basic_Static_Strategy_Type'Tag then
            return Basic_Static_Strategy_Type (L).T <
              Basic_Static_Strategy_Type (R).T;
         else
            return False;
         end if;
      else
         return Expanded_Name (L'Tag) < Expanded_Name (R'Tag);
      end if;
   end "<";

end TGen.Strategies;
