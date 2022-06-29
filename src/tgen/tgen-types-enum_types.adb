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

with TGen.Numerics; use TGen.Numerics;
with TGen.Random;   use TGen.Random;
with TGen.Strings;  use TGen.Strings;

package body TGen.Types.Enum_Types is

   function Image (Self : Bool_Typ) return String is
   begin
      return Typ (Self).Image & ": Boolean";
   end Image;

   function Lit_Image (Self : Bool_Typ; Lit : Big_Integer) return String is
      (if Big_Int."=" (Lit, Big_Zero) then "False" else "True");
   --  This isn't strictly correct, but this function should only be called
   --  with values of Lit being 0 or 1, given that these are the values that
   --  LAL will return when evaluating boolean static values.

   function High_Bound (Self : Bool_Typ) return Big_Integer is
     (Big_Int.To_Big_Integer (1));
   --  1 is the value representing True for boolean in the LAL static
   --  expression evaluator so use this value for now.

   function Supports_Static_Gen (Self : Bool_Typ) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Supports_Static_Gen;

   function Image (Self : Char_Typ) return String is
      Res : Unbounded_String := +Typ (Self).Image & ": Char";
   begin
      if Self.Has_Range then
         Res := Res & " range ";
         case Self.Range_Value.Low_Bound.Kind is
            when Static =>
               Res := Res & Self.Lit_Image
                 (Self.Range_Value.Low_Bound.Int_Val);
            when Discriminant =>
               Append (Res, +Self.Range_Value.Low_Bound.Disc_Name);
            when Non_Static =>
               Append (Res, +Self.Range_Value.Low_Bound.Text);
         end case;
         Res := Res & " .. ";
         case Self.Range_Value.High_Bound.Kind is
            when Static =>
               Res := Res & Self.Lit_Image
                 (Self.Range_Value.High_Bound.Int_Val);
            when Discriminant =>
               Append (Res, +Self.Range_Value.High_Bound.Disc_Name);
            when Non_Static =>
               Append (Res, +Self.Range_Value.High_Bound.Text);
         end case;
      end if;
      return To_String (Res);
   end Image;

   function Lit_Image (Self : Char_Typ; Lit : Big_Integer) return String is
      Res : constant String :=
        [1 => Character'Val (Long_Long_Integer'Value
                               (Big_Int.To_String (Lit)))];
      --  Wide_Wide_Character is 32 bits wide so we should be fine with
      --  Long_Long_Integer
   begin
      return "'" & Res & "'";
   end Lit_Image;

   function High_Bound (Self : Char_Typ) return Big_Integer is
   (if Self.Has_Range
    then Self.Range_Value.High_Bound.Int_Val
    else Big_Int.To_Big_Integer (Character'Pos ('~')));
   --  Although Char_Typ represents Character, Wide_Character and
   --  Wide_Wide_Character, we'll conservatively use ~ (last printable ASCII
   --  character) as the high bound.

   function Low_Bound (Self : Char_Typ) return Big_Integer is
     (if Self.Has_Range
      then Self.Range_Value.Low_Bound.Int_Val
      else Big_Int.To_Big_Integer (Character'Pos (' ')));
   --  The space is not the first element of Character but we won't generate
   --  non printable characters as they need to be unparsable in sources.

   function Generate_Static_Value_Char_Typ
     (Ty : Typ'Class) return Static_Value'Class;

   ------------------------------------
   -- Generate_Static_Value_Char_Typ --
   ------------------------------------

   function Generate_Static_Value_Char_Typ
     (Ty : Typ'Class) return Static_Value'Class
   is
      Result : Discrete_Static_Value;

      --  Let's use only the standard characters in the ASCII table. Others
      --  can't be represented as Character literals. Improvements TODO.

      use Big_Int;

      LB : constant Natural :=
        (if Char_Typ (Ty).Has_Range
         then Nat_Conversions.From_Big_Integer
           (Char_Typ (Ty).Range_Value.Low_Bound.Int_Val)
         else Character'Pos (' '));

      HB : constant Natural :=
        (if Char_Typ (Ty).Has_Range
         then Nat_Conversions.From_Big_Integer
           (Char_Typ (Ty).Range_Value.High_Bound.Int_Val)
         else Character'Pos ('~'));

      Lit : constant Integer :=
        Rand_Int (Min => LB, Max => HB);
   begin
      SP.From_Element (Result.T, Ty'Unrestricted_Access);
      Result.Value := To_Big_Integer (Lit);
      return Result;
   end Generate_Static_Value_Char_Typ;

   ---------------------
   -- Generate_Static --
   ---------------------

   function Generate_Static
     (Self    : Char_Typ;
      Context : in out Generation_Context)
      return Static_Strategy_Type'Class
   is
      Strat : Basic_Static_Strategy_Type;
   begin
      SP.From_Element (Strat.T, Self'Unrestricted_Access);
      Strat.F := Generate_Static_Value_Char_Typ'Access;
      return Strat;
   end Generate_Static;

   function Image (Self : Other_Enum_Typ) return String is
   begin
      return
        Typ (Self).Image & ": Enum"
        & (if Self.Is_Static
           then " range "
                & (+Self.Literals.First_Element)
                & " .. " & (+Self.Literals.Last_Element)
           else " (non static)");
   end Image;

   function Lit_Image
     (Self : Other_Enum_Typ; Lit : Big_Integer) return String is
      (+Self.Literals.Element (Lit));

   function Low_Bound (Self : Other_Enum_Typ) return Big_Integer is
     (Self.Literals.First_Key);

   function High_Bound (Self : Other_Enum_Typ) return Big_Integer is
     (Self.Literals.Last_Key);

end TGen.Types.Enum_Types;
