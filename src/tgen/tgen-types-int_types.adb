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

with GNAT.Random_Numbers;

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

   function Generate_Random_Strategy (Self : Int_Typ) return String
   is
   begin
      return "function Gen_"
        & Qualified_To_Unique_Name (Self.Type_Name)
        & " is new TGen.Int_Types.Gen ( " & (+Self.Fully_Qualified_Name)
        & ");";
   end Generate_Random_Strategy;

   function Gen return T is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (T);
   begin
      return Rand (Generator_Instance);
   end Gen;

   function Generate_Static (Self : Signed_Int_Typ) return JSON_Value
   is
      --  TODO: use Long_Long_Long_Integer (as it is the biggest possible type
      --  for which ranges can be defined), and as support to it in
      --  GNATCOLL.JSON.

      package LLLI_Conversions is
        new Big_Int.Signed_Conversions (Int => Long_Long_Integer);

      type T is new Long_Long_Integer
      range LLLI_Conversions.From_Big_Integer (Self.Range_Value.Min) ..
        LLLI_Conversions.From_Big_Integer (Self.Range_Value.Max);

      function Rand is new Gen (T);
   begin
      return Create (Long_Long_Integer (Rand));
   end Generate_Static;

end TGen.Types.Int_Types;
