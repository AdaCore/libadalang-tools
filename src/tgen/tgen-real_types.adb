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

package body TGen.Real_Types is

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

end TGen.Real_Types;
