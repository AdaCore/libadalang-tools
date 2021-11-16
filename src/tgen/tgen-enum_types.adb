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

with Langkit_Support.Text;

package body TGen.Enum_Types is

   package Text renames Langkit_Support.Text;

   function Image (Self : Bool_Typ) return String is
   begin
      return Typ (Self).Image & ": Boolean";
   end Image;

   function Lit_Image (Self : Bool_Typ; Lit : Integer) return String is
      (if Lit = 0 then "False" else "True");
   --  This isn't strictly correct, but this function should only be called
   --  with values of Lit being 0 or 1, given that these are the values that
   --  LAL will return when evaluating boolean static values.

   function High_Bound (Self : Bool_Typ) return Integer is (1);
   --  1 is the value representing True for boolean in the LAL static
   --  expression evaluator so use this value for now.

   function Image (Self : Char_Typ) return String is
   begin
      return Typ (Self).Image & ": Char";
   end Image;

   function Lit_Image (Self : Char_Typ; Lit : Integer) return String is
      Res : constant String := (1 => Character'Val (Lit));
   begin
      return Res;
   end Lit_Image;

   function High_Bound (Self : Char_Typ) return Integer is
     (Character'Pos (Character'Last));
   --  Although Char_Typ represents Character, Wide_Character and
   --  Wide_Wide_Character, we'll conservatively use Character'Last as the high
   --  bound.

   function Image (Self : Other_Enum_Typ) return String is
   begin
      return
        Typ (Self).Image & ": Enum"
        & (if Self.Is_Static
           then " range " & Text.Image (Self.Literals.First_Element.Text)
                & " .. " & Text.Image (Self.Literals.Last_Element.Text)
           else " (non static)");
   end Image;

   function Lit_Image (Self : Other_Enum_Typ; Lit : Integer) return String is
      (Text.Image (Self.Literals.Element (Lit).Text));

   function Low_Bound (Self : Other_Enum_Typ) return Integer is
     (Self.Literals.First_Key);

   function High_Bound (Self : Other_Enum_Typ) return Integer is
     (Self.Literals.Last_Key);

end TGen.Enum_Types;
