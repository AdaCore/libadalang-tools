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

with TGen.Strings; use TGen.Strings;
with TGen.Random; use TGen.Random;

package body TGen.Int_Types is

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

   function Gen return T
   is
      function Rand is new GNAT.Random_Numbers.Random_Discrete (T);
   begin
      return Rand (Generator_Instance);
   end Gen;

   function "+" (Text : Unbounded_Text_Type) return String is
     (To_UTF8 (To_Text (Text)));

   function "+" (Text : Text_Type) return String is
     (To_UTF8 (Text));

   function Generate_Random_Strategy (Self : Int_Typ) return String
   is
   begin
      return "function "
        & Qualified_To_Unique_Name (Self.Type_Name)
        & " is new TGen.Int_Types.Gen;";
   end Generate_Random_Strategy;

end TGen.Int_Types;
