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

with TGen.Types; use TGen.Types;

package TGen.Int_Types is

   type Int_Range is record
      Min, Max : Big_Integer;
   end record;

   type Int_Typ is new Discrete_Typ with null record;

   type Signed_Int_Typ (Is_Static : Boolean) is new
     Int_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Range_Value : Int_Range;
         when others =>
            null;
      end case;
   end record;

   function Low_Bound (Self : Signed_Int_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function High_Bound (Self : Signed_Int_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function Image (Self : Signed_Int_Typ) return String;

   function Kind (Self : Signed_Int_Typ) return Typ_Kind is (Signed_Int_Kind);

   function As_Signed_Int_Typ (Self : SP.Ref)
     return Signed_Int_Typ'Class is
     (Signed_Int_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Signed_Int_Kind);
   pragma Inline (As_Signed_Int_Typ);

   type Mod_Int_Typ (Is_Static : Boolean) is new
     Int_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Mod_Value : Big_Integer;
         when others =>
            null;
      end case;
   end record;

   function Image (Self : Mod_Int_Typ) return String;

   function High_Bound (Self : Mod_Int_Typ) return Big_Integer with
     Pre => Self.Is_Static;

   function Kind (Self : Mod_Int_Typ) return Typ_Kind is (Mod_Int_Kind);

   function As_Mod_Int_Typ (Self : SP.Ref) return Mod_Int_Typ'Class is
     (Mod_Int_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Mod_Int_Kind);
   pragma Inline (As_Mod_Int_Typ);

end TGen.Int_Types;
