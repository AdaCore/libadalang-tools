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
--
--  Type representation for integer types, and associated generation functions

with TGen.Types.Discrete_Types; use TGen.Types.Discrete_Types;
with TGen.Strategies;           use TGen.Strategies;

package TGen.Types.Int_Types is

   type Int_Typ is new Discrete_Typ with null record;

   function Encode (Self : Int_Typ; Val : JSON_Value) return JSON_Value
   is (Create (Self.Lit_Image (Val.Get)));

   type Signed_Int_Typ (Is_Static : Boolean) is
     new Int_Typ (Is_Static => Is_Static)
   with record
      case Is_Static is
         when True =>
            Range_Value : Int_Range;

         when others =>
            null;
      end case;
   end record;

   function Supports_Static_Gen (Self : Signed_Int_Typ) return Boolean
   is (Self.Is_Static);
   --  Whether values for this Typ can be statically generated

   function Low_Bound (Self : Signed_Int_Typ) return Big_Integer
   with Pre => Self.Is_Static;

   function High_Bound (Self : Signed_Int_Typ) return Big_Integer
   with Pre => Self.Is_Static;

   function Image (Self : Signed_Int_Typ) return String;

   function Kind (Self : Signed_Int_Typ) return Typ_Kind
   is (Signed_Int_Kind);

   function As_Signed_Int_Typ (Self : Typ_Access) return Signed_Int_Typ'Class
   is (Signed_Int_Typ'Class (Self.all))
   with Pre => Self /= null and then Self.all.Kind in Signed_Int_Kind;
   pragma Inline (As_Signed_Int_Typ);

   type Mod_Int_Typ (Is_Static : Boolean) is
     new Int_Typ (Is_Static => Is_Static)
   with record
      case Is_Static is
         when True =>
            Range_Value : Int_Range := (0, 0);
            Mod_Value   : Big_Integer;

         when others =>
            null;
      end case;
   end record;

   function Supports_Static_Gen (Self : Mod_Int_Typ) return Boolean
   is (Self.Is_Static);
   --  Whether values for this Typ can be statically generated

   function Image (Self : Mod_Int_Typ) return String;

   function Low_Bound (Self : Mod_Int_Typ) return Big_Integer
   with Pre => Self.Is_Static;

   function High_Bound (Self : Mod_Int_Typ) return Big_Integer
   with Pre => Self.Is_Static;

   function Kind (Self : Mod_Int_Typ) return Typ_Kind
   is (Mod_Int_Kind);

   function As_Mod_Int_Typ (Self : Typ_Access) return Mod_Int_Typ'Class
   is (Mod_Int_Typ'Class (Self.all))
   with Pre => Self /= null and then Self.all.Kind in Mod_Int_Kind;
   pragma Inline (As_Mod_Int_Typ);

   generic
      type T is (<>);
   function Gen return T;

   overriding
   function Default_Strategy
     (Self : Signed_Int_Typ) return Strategy_Type'Class;
   --  Generate a strategy to statically generate (in one pass) values for Self

end TGen.Types.Int_Types;
