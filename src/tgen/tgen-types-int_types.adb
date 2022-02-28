------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Random_Numbers;

with TGen.Context; use TGen.Context;
with TGen.Strings; use TGen.Strings;
with TGen.Random; use TGen.Random;

package body TGen.Types.Int_Types is

   function Image (Self : Signed_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Signed Integer"
         & (if Self.Is_Static
            then " range " & Big_Int.To_String (Self.Range_Value.Min) & " .."
                 & Big_Int.To_String (Self.Range_Value.Max)
            else " (non static)"));
   end Image;

   function Low_Bound (Self : Signed_Int_Typ) return Big_Integer is
     (Self.Range_Value.Min);

   function High_Bound (Self : Signed_Int_Typ) return Big_Integer is
     (Self.Range_Value.Max);

   function Image (Self : Mod_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Modular Integer"
         & (if Self.Is_Static
            then " mod" & Big_Int.To_String (Self.Mod_Value)
            else "(non static)"));
   end Image;

   function High_Bound (Self : Mod_Int_Typ) return Big_Integer is
     (Self.Mod_Value);

   function Generate_Static_Value_Mod_Typ (Ty : Typ'Class) return Static_Value;

   -----------------------------------
   -- Generate_Static_Value_Mod_Typ --
   -----------------------------------

   function Generate_Static_Value_Mod_Typ (Ty : Typ'Class) return Static_Value
   is
      Self : Mod_Int_Typ := Mod_Int_Typ (Ty);
      package LLLI_Conversions is
        new Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      type T is new Long_Long_Integer
      range 0 .. LLLI_Conversions.From_Big_Integer (Self.High_Bound);

      function Rand is new Gen (T);
   begin
      return Long_Long_Integer'Image (Long_Long_Integer (Rand));
   end Generate_Static_Value_Mod_Typ;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Mod_Int_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Static_Value_Mod_Typ'Access;
      Context.Strategies.Include (Strat);
      return Strat;
   end Generate_Static;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self    : Int_Typ;
      Context : in out Generation_Context) return Strategy_Type'Class
   is
      Result : Dynamic_Strategy_Type
        (Kind => Random_Kind, Constrained => False);
      F_Body : Unbounded_String;
      Indent : Natural := 0;
   begin
      Write_Line (F_Body, "declare", Indent);
      Indent := @ + 3;
      Write_Line
        (F_Body,
         "function Gen is new TGen.Types.Int_Types.Gen ("
         & (+Self.Fully_Qualified_Name) & ");",
         Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "begin", Indent);
      Indent := @ + 3;
      Write_Line (F_Body, "return Gen;", Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "end;", Indent);

      Result.Strategy_Function := Self.Random_Strategy_Function;
      Result.Strategy_Body := +(+F_Body);
      return Result;
   end Generate_Random_Strategy;

   ------------------------------
   -- Generate_Sample_Strategy --
   ------------------------------

   --  function Generate_Sample_Strategy
   --    (Self   : Int_Typ;
   --     GC     : Generation_Context;
   --     Initialize_F_Name : String;
   --     F_Name : String;
   --     Sample : Alternatives_Set) return String is
   --     Result : Unbounded_String;
   --     Indent : Natural := 0;
   --  begin
   --
   --     --  Write the Initialize function
   --
   --     Write_Line
   --       (Result,
   --        "procedure " & Initialize_F_Name
   --        & " (State : in out Alternatives_Set) ",
   --       Indent);
   --     Write_Line (Result, "is", Indent);
   --     Write_Line (Result, "begin", Indent);
   --     Indent := Indent + 3;
   --     Write_Line
   --       (Result, "State := " & Alternatives_Set'Image (Sample), Indent);
   --     Indent := Indent - 3;
   --     Write_Line ("end;");
   --
   --     Write_Line
   --       (Result,
   --        "function " & F_Name & " (State : Alternatives_Set) return "
   --        & (+Self.Fully_Qualified_Name),
   --        Indent);
   --     Indent := @ + 3;
   --     Write_Line (Result, "is", Indent);
   --     Indent := @ + 3;
   --     Write_Line
   --       (Result,
   --        "function Gen is new TGen.Types.Int_Types.Gen ("
   --        & (+Self.Fully_Qualified_Name) & ");",
   --        Indent);
   --     Indent := @ - 3;
   --     Write_Line (Result, "begin", Indent);
   --     Indent := @ + 3;
   --     Write_Line (Result, "return Gen;", Indent);
   --     Indent := @ - 3;
   --     Write_Line (Result, "end " & F_Name & ";", Indent);
   --     return +Result;
   --  end Generate_Sample_Strategy;

   function Gen return T is
      function Rand is new
        GNAT.Random_Numbers.Random_Discrete (T, T'First);
   begin
      return Rand (Generator_Instance);
   end Gen;

   function Generate (Ty : Typ'Class) return Static_Value;

   --------------
   -- Generate --
   --------------

   function Generate (Ty : Typ'Class) return Static_Value
   is
      Self : Signed_Int_Typ := Signed_Int_Typ (Ty);
      package LLLI_Conversions is
        new Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      type T is new Long_Long_Integer
      range LLLI_Conversions.From_Big_Integer (Self.Range_Value.Min) ..
        LLLI_Conversions.From_Big_Integer (Self.Range_Value.Max);

      function Rand is new Gen (T);
   begin
      return Long_Long_Integer'Image (Long_Long_Integer (Rand));
   end Generate;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Signed_Int_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      --  TODO: use Long_Long_Long_Integer (as it is the biggest possible type
      --  for which ranges can be defined), and add support to it in
      --  GNATCOLL.JSON.

      Type_Ref : SP.Ref;
      Strat : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate'Access;
      Context.Strategies.Include (Strat);
      return Strat;
   end Generate_Static;

   ---------------------------
   -- Generate_Static_Value --
   ---------------------------

   function Generate_Static_Value
     (Strat : in out Static_Array_Constraint_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value
   is
      package N_Conversions is
        new Big_Int.Signed_Conversions (Int => Natural);
      use N_Conversions;

      use Big_Int;

      Picked_Size : Natural := 0;
      Elements    : Many_Type :=
        Many
          (0,
           From_Big_Integer
             (Strat.T.Range_Value.Max - Strat.T.Range_Value.Min),
           Strat.Avg_Size);
   begin
      while Elements.More loop
         null;
      end loop;
      return Natural'Image (Elements.Count);
   end Generate_Static_Value;

   ----------------------------------------
   -- Generate_Array_Constraint_Strategy --
   ----------------------------------------

   function Generate_Array_Constraint_Strategy
     (Self : Signed_Int_Typ) return Static_Array_Constraint_Strategy_Type'Class
   is
   begin
      case Self.Is_Static is
         when True =>
            declare
               T : Static_Array_Constraint_Strategy_Type :=
                 (T => Self, Avg_Size => 5);
            begin
               return T;
            end;
         when others =>
            return raise Program_Error with "unsupported non static type";
      end case;
   end Generate_Array_Constraint_Strategy;

end TGen.Types.Int_Types;
