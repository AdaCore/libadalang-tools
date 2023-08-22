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

with TGen.Random; use TGen.Random;

package body TGen.Strategies is

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Strategy_Type'Class) return Boolean
   is
      pragma Unreferenced (L);
      pragma Unreferenced (R);
   begin
      return False;
   end "=";

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Commented_Out_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value is
   begin
      return Create ("--  Replace this comment by a valid value");
   end Generate;

   --------------
   -- Generate --
   --------------

   function Generate
     (S            : in out Dispatching_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
      Rnd : constant Float := Rand_Float;
   begin
      if Rnd <= S.Bias then
         if S.S1.Has_Next then
            return S.S1.Generate (Disc_Context);
         end if;
      end if;
      return S.S2.Generate (Disc_Context);
   end Generate;

   ----------------------------
   -- Make_Dispatching_Strat --
   ----------------------------

   function Make_Dispatching_Strat
     (S1, S2 : Strategy_Type'Class;
      Bias   : Float := 0.5) return Dispatching_Strategy_Type
   is
      Strat : Dispatching_Strategy_Type;
   begin
      Strat.Bias := Bias;
      Strat.S1 := new Strategy_Type'Class'(S1);
      Strat.S2 := new Strategy_Type'Class'(S2);
      return Strat;
   end Make_Dispatching_Strat;

   package body Equivalence_Classes_Strategy_Package is

      --------------
      -- Generate --
      --------------

      function Generate
        (S            : in out Equivalence_Class_Strategy_Type;
         Disc_Context : Disc_Value_Map) return JSON_Value
      is
         Idx : constant Integer := Rand_Int (1, Integer (S.Classes.Length));
      begin
         return S.Draw (S.T, S.Classes.Element (Idx));
      end Generate;

   end Equivalence_Classes_Strategy_Package;

end TGen.Strategies;
