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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Random_Numbers;

with TGen.Strings; use TGen.Strings;
with TGen.Random;  use TGen.Random;

package body TGen.Types.Real_Types is

   function Image (Self : Float_Typ) return String is
     (Typ (Self).Image & ": Real Type"
      & (if Self.Is_Static
         then " digits " & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range " & Big_Reals.To_String (Self.Range_Value.Min)
                      & " .. " & Big_Reals.To_String (Self.Range_Value.Max)
                 else "")
         else " (non static)"));

   function Image (Self : Ordinary_Fixed_Typ) return String is
     (Typ (Self).Image & ": Ordinary Fixed Point"
      & (if Self.Is_Static
         then " delta " & Big_Reals.To_String (Self.Delta_Value) & " range "
              & Big_Reals.To_String (Self.Range_Value.Min) & " .. "
              & Big_Reals.To_String (Self.Range_Value.Max)
         else " (non static)"));

   function Image (Self : Decimal_Fixed_Typ) return String is
     (Typ (Self).Image & ": Decimal Fixed Point"
      & (if Self.Is_Static
         then " delta " & Big_Reals.To_String (Self.Delta_Value) & " digits"
              & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range " & Big_Reals.To_String (Self.Range_Value.Min)
                      & " .. " & Big_Reals.To_String (Self.Range_Value.Max)
                 else "")
         else " (non static)"));

   function Low_Bound_Or_Default (Self : Float_Typ) return Big_Real is
     (if Self.Has_Range and then Big_Reals.Is_Valid (Self.Range_Value.Min)
      then Self.Range_Value.Min
      else LF_Conversions.To_Big_Real (Long_Float'First));

   function High_Bound_Or_Default (Self : Float_Typ) return Big_Real is
     (if Self.Has_Range and then Big_Reals.Is_Valid (Self.Range_Value.Max)
      then Self.Range_Value.Max
      else LF_Conversions.To_Big_Real (Long_Float'Last));

   function Low_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   is (Self.Range_Value.Min);

   function High_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   is (Self.Range_Value.Max);

   function Low_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   is
      use Big_Reals;
   begin
      return (if Self.Has_Range
              then Self.Range_Value.Min
              else -(To_Real (10) ** Self.Digits_Value - To_Real (1))
                    * Self.Delta_Value);
   end Low_Bound_Or_Default;

   function High_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   is
      use Big_Reals;
   begin
      return (if Self.Has_Range
              then Self.Range_Value.Max
              else (To_Real (10) ** Self.Digits_Value - To_Real (1))
                   * Self.Delta_Value);
   end High_Bound_Or_Default;

   function Gen return T is
      function Rand is new GNAT.Random_Numbers.Random_Float (T);

      --  TODO: the random number generator generates number between 0 and 1:
      --  enhance that to generate number over the whole type span.

   begin
      return Rand (Generator_Instance);
   end Gen;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy
     (Self    : Float_Typ;
      Context : in out Generation_Context) return Strategy_Type'Class
   is
      Res : Dynamic_Strategy_Type (Kind => Random_Kind, Constrained => False);
      F_Body : Unbounded_String;
      Indent : Natural := 0;
   begin
      Write_Line (F_Body, "declare", Indent);
      Indent := @ + 3;
      Write_Line
        (F_Body,
         "function Gen is new TGen.Types.Real_Types.Gen ("
         & Self.Fully_Qualified_Name & ");",
         Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "begin", Indent);
      Indent := @ + 3;
      Write_Line (F_Body, "return Gen;", Indent);
      Indent := @ - 3;
      Write_Line (F_Body, "end;", Indent);

      Res.Strategy_Function := Self.Random_Strategy_Function;
      Res.Strategy_Body := +(+F_Body);
      return Res;
   end Generate_Random_Strategy;

   function Generate_Float_Typ (Ty : Typ'Class) return Static_Value'Class;

   --------------
   -- Generate --
   --------------

   function Generate_Float_Typ (Ty : Typ'Class) return Static_Value'Class
   is
      Self : constant Float_Typ := Float_Typ (Ty);

      LB : constant Big_Real := Self.Low_Bound_Or_Default;

      HB : constant Big_Real := Self.High_Bound_Or_Default;

      type T is new Long_Float
      range LF_Conversions.From_Big_Real (LB)
        .. LF_Conversions.From_Big_Real (HB);

      function Rand is new Gen (T);
   begin
      return Base_Static_Value'
        (Value => +Long_Float'Image (Long_Float (Rand)));
   end Generate_Float_Typ;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Float_Typ;
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
      Strat.F := Generate_Float_Typ'Access;
      return Strat;
   end Generate_Static;

   function Generate_Ordinary_Fixed_Typ
     (Ty : Typ'Class) return Static_Value'Class;

   function Generate_Decimal_Fixed_Typ
     (Ty : Typ'Class) return Static_Value'Class;

   ---------------------------------
   -- Generate_Ordinary_Fixed_Typ --
   ---------------------------------

   function Generate_Ordinary_Fixed_Typ
     (Ty : Typ'Class) return Static_Value'Class
   is
      use Big_Reals;
      Self : constant Ordinary_Fixed_Typ := Ordinary_Fixed_Typ (Ty);

      High_Bound : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer
          (LF_Conversions.From_Big_Real
             (Self.Range_Value.Max / Self.Delta_Value));
      Low_Bound  : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer
          (LF_Conversions.From_Big_Real
             (Self.Range_Value.Min / Self.Delta_Value));

      Rand_Val : constant Long_Long_Long_Integer :=
        Rand_LLLI (Low_Bound, High_Bound);
   begin
      return Base_Static_Value'
        (Value => +(
           To_String (Self.Delta_Value)
           & " * "
           & (if Rand_Val >= 0
              then Rand_Val'Image
              else "(" & Rand_Val'Image & ")")));
   end Generate_Ordinary_Fixed_Typ;

   --------------------------------
   -- Generate_Decimal_Fixed_Typ --
   --------------------------------

   function Generate_Decimal_Fixed_Typ
     (Ty : Typ'Class) return Static_Value'Class
   is
      use Big_Reals;
      Self : constant Decimal_Fixed_Typ := Decimal_Fixed_Typ (Ty);

      --  TODO: Using High/Low_Bound_Or_Default ignores the digits value, which
      --  may not play nice with the digits value if it is too low. We may need
      --  to generate some "sparse" integer ranges to ensure we do not go
      --  beyond the specified precision.

      High_Bound : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer
          (LF_Conversions.From_Big_Real
             (Self.High_Bound_Or_Default / Self.Delta_Value));
      Low_Bound  : constant Long_Long_Long_Integer :=
        Long_Long_Long_Integer
          (LF_Conversions.From_Big_Real
             (Self.Low_Bound_Or_Default / Self.Delta_Value));

      Rand_Val : constant Long_Long_Long_Integer :=
        Rand_LLLI (Low_Bound, High_Bound);
   begin
      return Base_Static_Value'
        (Value => +(
           Self.Delta_Value'Image
           & " * "
           & (if Rand_Val >= 0
              then Rand_Val'Image
              else "(" & Rand_Val'Image & ")")));
   end Generate_Decimal_Fixed_Typ;

   overriding function Generate_Static
     (Self    : Ordinary_Fixed_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Ordinary_Fixed_Typ'Access;
      return Strat;
   end Generate_Static;

   overriding function Generate_Static
     (Self    : Decimal_Fixed_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class
   is
      Type_Ref : SP.Ref;
      Strat    : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Type_Ref, Self'Unrestricted_Access);
      Strat.T := Type_Ref;
      Strat.F := Generate_Decimal_Fixed_Typ'Access;
      return Strat;
   end Generate_Static;

end TGen.Types.Real_Types;
