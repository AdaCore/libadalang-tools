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
with TGen.Random; use TGen.Random;

package body TGen.Types.Real_Types is

   function Image (Self : Float_Typ) return String is
     (Typ (Self).Image & ": Real Type"
      & (if Self.Is_Static
         then " digits" & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range" & Self.Range_Value.Min'Image & " .."
                       & Self.Range_Value.Max'Image
                 else "")
         else " (non static)"));

   function Image (Self : Ordinary_Fixed_Typ) return String is
     (Typ (Self).Image & ": Ordinary Fixed Point"
      & (if Self.Is_Static
         then " delta " & Self.Delta_Value'Image & " range"
              & Self.Range_Value.Min'Image & " .." & Self.Range_Value.Max'Image
         else " (non static)"));

   function Image (Self : Decimal_Fixed_Typ) return String is
     (Typ (Self).Image & ": Decimal Fixed Point"
      & (if Self.Is_Static
         then " delta" & Self.Delta_Value'Image & " digits"
              & Self.Digits_Value'Image
              & (if Self.Has_Range
                 then " range" & Self.Range_Value.Min'Image & " .."
                      & Self.Range_Value.Max'Image
                 else "")
         else " (non static)"));

   function Gen return T is
      function Rand is new GNAT.Random_Numbers.Random_Float (T);
   begin
      return Rand (Generator_Instance);
   end Gen;

   ------------------------------
   -- Generate_Random_Strategy --
   ------------------------------

   function Generate_Random_Strategy (Self : Float_Typ) return String
   is
      Result : Unbounded_String;
      F_Name : constant String := Self.Gen_Random_Function_Name;
      Indent : Natural := 0;
   begin
      Write_Line
        (Result,
         "function " & F_Name & " return " & (+Self.Fully_Qualified_Name),
         Indent);
      Indent := @ + 3;
      Write_Line (Result, "is", Indent);
      Indent := @ + 3;
      Write_Line
        (Result,
         "function Gen is new TGen.Types.Real_Types.Gen ("
         & (+Self.Fully_Qualified_Name) & ");",
         Indent);
      Indent := @ - 3;
      Write_Line (Result, "begin", Indent);
      Indent := @ + 3;
      Write_Line (Result, "return Gen;", Indent);
      Indent := @ - 3;
      Write_Line (Result, "end " & F_Name & ";", Indent);
      return +Result;
   end Generate_Random_Strategy;

   function Generate_Static (Self : Float_Typ) return String is
   begin
      if not Self.Is_Static or not Self.Has_Range then
         raise Program_Error with "Cannot generate values for non static type "
           & Image (Self);
      end if;
      declare
         type Float_Type is new Long_Float
           range Self.Range_Value.Min .. Self.Range_Value.Max;
         function Rand_Value is new Gen (Float_Type);
      begin
         return Long_Float'Image (Long_Float (Rand_Value));
      end;
   end Generate_Static;

end TGen.Types.Real_Types;
