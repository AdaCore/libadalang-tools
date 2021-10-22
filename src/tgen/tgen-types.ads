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

with Libadalang.Analysis;

with Ada.Unchecked_Deallocation;

with Ada.Containers.Ordered_Maps;

package TGen.Types is

   package LAL renames Libadalang.Analysis;

   type Int_Range is record
      Min, Max : Integer;
   end record;

   type Float_Range is record
      Min, Max : Long_Float;
   end record;

   type Typ is tagged record
      Name : LAL.Defining_Name;
   end record;

   function Image (Self : Typ) return String;

   type Scalar_Typ is new Typ with null record;

   type Discrete_Typ is new Scalar_Typ with null record;

   type Enum_Typ is new Discrete_Typ with null record;

   type Char_Typ is new Enum_Typ with null record;

   function Image (Self : Char_Typ) return String;

   type Bool_Typ is new Enum_Typ with null record;

   function Image (Self : Bool_Typ) return String;

   function "=" (Left, Right : LAL.Defining_Name) return Boolean is
     (Left.Text = Right.Text);

   package Enum_Literal_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Natural, Element_Type => LAL.Defining_Name);

   type Other_Enum_Typ is new Enum_Typ with record
      Literals : Enum_Literal_Maps.Map;
   end record;

   function Image (Self : Other_Enum_Typ) return String;

   type Int_Typ is new Discrete_Typ with null record;

   type Signed_Int_Typ is new Int_Typ with record
      Range_Value : Int_Range;
   end record;

   function Image (Self : Signed_Int_Typ) return String;

   type Mod_Int_Typ is new Int_Typ with record
      Mod_Value : Integer;
   end record;

   function Image (Self : Mod_Int_Typ) return String;

   type Real_Typ is new Scalar_Typ with null record;

   type Float_Typ (Has_Range : Boolean) is new Real_Typ with record
      Digits_Value : Natural;
      case Has_Range is
         when True =>
            Range_Value : Float_Range;
         when False =>
            null;
      end case;
   end record;

   function Image (Self : Float_Typ) return String;

   type Ordinary_Fixed_Typ is new Real_Typ with record
      Delta_Value : Long_Float;
      Range_Value : Float_Range;
   end record;

   function Image (Self : Ordinary_Fixed_Typ) return String;

   type Decimal_Fixed_Typ (Has_Range : Boolean) is new Real_Typ with record
      Delta_Value  : Long_Float;
      Digits_Value : Natural;
      case Has_Range is
         when True =>
            Range_Value : Float_Range;
         when others =>
            null;
      end case;
   end record;

   function Image (Self : Decimal_Fixed_Typ) return String;

   type Access_Typ is new Typ with null record;

   type Composite_Typ is new Typ with null record;

   type Typ_Acc is access Typ'Class;

   procedure Free is new Ada.Unchecked_Deallocation (Typ'Class, Typ_Acc);

end TGen.Types;
