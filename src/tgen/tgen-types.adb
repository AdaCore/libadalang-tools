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

package body TGen.Types is

   package Text renames Langkit_Support.Text;

   function Image (Self : Typ) return String is
   begin
      return Text.Image (Self.Name.Text);
   end Image;

   function Image (Self : Signed_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Signed Integer range" & Self.Rang.Min'Image &
         " .." & Self.Rang.Max'Image);
   end Image;

   function Image (Self : Mod_Int_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Modular Integer mod" & Self.Mod_Value'Image);
   end Image;

   function Image (Self : Float_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Real Type digits" & Self.Precision'Image &
         (if Self.Has_Range then
            " range" & Self.Rang.Min'Image & " .." & Self.Rang.Max'Image
          else ""));
   end Image;

   function Image (Self : Ordinary_Fixed_Typ) return String is
   begin
      return
        (Typ (Self).Image & ": Ordinary Fixed Point delta" &
         Self.Delta_Value'Image & " range" & Self.Rang.Min'Image & " .." &
         Self.Rang.Max'Image);
   end Image;

   function Image (Self : Decimal_Fixed_Typ) return String is
   begin
      return
        Typ (Self).Image & ": Decimal Fixed Point delta" &
        Self.Delta_Value'Image & " digits" & Self.Digits_Value'Image &
        (if Self.Has_Range then
           " range" & Self.Rang.Min'Image & " .." & Self.Rang.Max'Image
         else "");
   end Image;

   function Image (Self : Bool_Typ) return String is
   begin
      return Typ (Self).Image & ": Boolean";
   end Image;

   function Image (Self : Char_Typ) return String is
   begin
      return Typ (Self).Image & ": Char";
   end Image;

   function Image (Self : Other_Enum_Typ) return String is
   begin
      return
        Typ (Self).Image & ": Enum range "
        & Text.Image (Self.Literals.First_Element.Text)
        & " .. " & Text.Image (Self.Literals.Last_Element.Text);
   end Image;

end TGen.Types;
