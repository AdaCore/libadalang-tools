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

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with TGen.Context;              use TGen.Context;
with TGen.Strategies;           use TGen.Strategies;
with TGen.Types;                use TGen.Types;
with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;
with TGen.Types.Int_Types;      use TGen.Types.Int_Types;

package TGen.Types.Enum_Types is

   type Enum_Typ is new Discrete_Typ with null record;

   type Char_Typ is new Enum_Typ with null record;

   function Image (Self : Char_Typ) return String;

   function Lit_Image (Self : Char_Typ; Lit : Big_Integer) return String;

   function High_Bound (Self : Char_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function Kind (Self : Char_Typ) return Typ_Kind is (Char_Kind);

   function Generate_Static
     (Self    : Char_Typ;
      Context : in out Generation_Context)
      return Static_Strategy_Type'Class;

   function As_Char_Typ (Self : SP.Ref) return Char_Typ'Class is
     (Char_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Char_Kind);
   pragma Inline (As_Char_Typ);

   type Bool_Typ is new Enum_Typ with null record;

   function Image (Self : Bool_Typ) return String;

   function Lit_Image (Self : Bool_Typ; Lit : Big_Integer) return String;

   function High_Bound (Self : Bool_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function Kind (Self : Bool_Typ) return Typ_Kind is (Bool_Kind);

   function As_Bool_Typ (Self : SP.Ref) return Bool_Typ'Class is
     (Bool_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Bool_Kind);
   pragma Inline (As_Bool_Typ);

   package Enum_Literal_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Big_Integer,
      Element_Type => Unbounded_Text_Type,
       "<"         => Big_Int."<");

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

   function Lit_Image (Self : Other_Enum_Typ; Lit : Big_Integer) return String;

   function Low_Bound (Self : Other_Enum_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function High_Bound (Self : Other_Enum_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function Kind (Self : Other_Enum_Typ) return Typ_Kind is (Enum_Kind);

   function As_Other_Enum_Typ (Self : SP.Ref) return Other_Enum_Typ'Class is
     (Other_Enum_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Enum_Kind);
   pragma Inline (As_Other_Enum_Typ);

end TGen.Types.Enum_Types;
