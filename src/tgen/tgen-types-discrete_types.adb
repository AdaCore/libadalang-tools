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

with GNAT.Random_Numbers;

with TGen.Context;
with TGen.Random;  use TGen.Random;

package body TGen.Types.Discrete_Types is

   --  TODO: pick a value in the intervals and not the first one

   ----------
   -- Draw --
   ----------

   function Draw (Intervals : Alternatives_Set) return Big_Integer is
   begin
      return Intervals.First_Element.Min;
   end Draw;


   ---------------
   -- Lit_Image --
   ---------------

   function Lit_Image
     (Self : Discrete_Typ; Lit : Big_Integer) return String is
     (Big_Int.To_String (Lit));

   ---------------
   -- Low_Bound --
   ---------------

   function Low_Bound (Self : Discrete_Typ) return Big_Integer is
     (Big_Zero);

   ----------------
   -- High_Bound --
   ----------------

   function High_Bound (Self : Discrete_Typ) return Big_Integer is
     (Big_Zero);

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Discrete_Static_Value) return String is
      T_Classwide : Typ'Class := Self.T.Get;
      T_Discrete  : Discrete_Typ'Class := Discrete_Typ'Class (T_Classwide);
   begin
      return T_Discrete.Lit_Image (Self.Value);
   end To_String;

   --  Sampling strategy: draw an arbitrary value from an arbitrary sample in
   --  a list of samples.

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (S            : in out Sample_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
      Result        : Discrete_Static_Value;
      Picked_Index  : Positive :=
        Positive (Rand_Int (1, Integer (S.Samples.Length)));
      I             : Integer          := 1;
      Picked_Sample : Alternatives_Set := S.Samples.Element (Picked_Index);
      T_Classwide : Typ'Class := S.T.Get;
   begin
      SP.From_Element (Result.T, T_Classwide'Unrestricted_Access);
      Result.Value := Draw (Picked_Sample);
      return Result;
   end Generate_Static_Value;

   --------------------------------
   -- Generate_Sampling_Strategy --
   --------------------------------

   function Generate_Sampling_Strategy
     (Self    : Discrete_Typ;
      Samples : Alternatives_Set_Vector) return Static_Strategy_Type'Class
   is
      Strat : Sample_Static_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.Samples := Samples;
      return Strat;
   end Generate_Sampling_Strategy;

   generic
      type T is (<>);
   function Gen return T;

   ---------
   -- Gen --
   ---------

   function Gen return T is
      function Rand is new
        GNAT.Random_Numbers.Random_Discrete (T, T'First);
   begin
      return Rand (Generator_Instance);
   end Gen;

   function Generate_Static_Value_Random
     (Ty : Typ'Class) return Static_Value'Class;

   ----------------------------------
   -- Generate_Static_Value_Random --
   ----------------------------------

   function Generate_Static_Value_Random
     (Ty : Typ'Class) return Static_Value'Class
   is
      Result : Discrete_Static_Value;

      Self : Discrete_Typ'Class := Discrete_Typ'Class (Ty);
      package LLI_Conversions is
        new Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      --  TODO??? Work around until V307-012 is fixed (can't convert to values
      --  outside of Long_Long_Integer bounds).

      type T is new Long_Long_Integer range
        LLI_Conversions.From_Big_Integer
          (Big_Int.Max
             (Self.Low_Bound,
              LLI_Conversions.To_Big_Integer (Long_Long_Integer'First + 1)))
          ..
            LLI_Conversions.From_Big_Integer
              (Big_Int.Min
                 (Self.High_Bound,
                  LLI_Conversions.To_Big_Integer
                    (Long_Long_Integer'Last - 1)));

      function Rand is new Gen (T);
   begin
      SP.From_Element (Result.T, Ty'Unrestricted_Access);
      Result.Value :=
        LLI_Conversions.To_Big_Integer (Long_Long_Integer (Rand));
      return Result;
   end Generate_Static_Value_Random;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Discrete_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Static_Value_Random'Access;
      Context.Strategies.Include (Strat);
      return Strat;
   end Generate_Static;

end TGen.Types.Discrete_Types;
