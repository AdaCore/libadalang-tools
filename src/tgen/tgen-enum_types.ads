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

with Ada.Containers.Ordered_Maps;

with TGen.Types;        use TGen.Types;

package TGen.Enum_Types is

   type Enum_Typ is new Discrete_Typ with null record;

   type Char_Typ is new Enum_Typ with null record;

   function Image (Self : Char_Typ) return String;

   type Bool_Typ is new Enum_Typ with null record;

   function Image (Self : Bool_Typ) return String;

   function "=" (Left, Right : LAL.Defining_Name) return Boolean is
     (Left.Text = Right.Text);

   package Enum_Literal_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Natural, Element_Type => LAL.Defining_Name);

   type Other_Enum_Typ (Is_Static : Boolean) is new
     Enum_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Literals : Enum_Literal_Maps.Map;
         when others =>
            null;
      end case;
   end record;

   function Image (Self : Other_Enum_Typ) return String;

end TGen.Enum_Types;
