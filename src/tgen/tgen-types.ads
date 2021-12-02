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

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

limited with Tgen.Int_Types;
limited with Tgen.Enum_Types;
limited with Tgen.Array_Types;
limited with Tgen.Record_Types;
limited with TGen.Real_Types;
package TGen.Types is

   package LAL renames Libadalang.Analysis;
   type Typ is tagged record
      Name : LAL.Defining_Name;
   end record;

   type Typ_Kind is (Invalid_Kind,
                     Signed_Int_Kind,
                     Mod_Int_Kind,
                     Bool_Kind,
                     Char_Kind,
                     Enum_Kind,
                     Float_Kind,
                     Fixed_Kind,
                     Decimal_Kind,
                     Ptr_Kind,
                     Unconstrained_Array_Kind,
                     Constrained_Array_Kind,
                     Disc_Record_Kind,
                     Non_Disc_Record_Kind);

   subtype Discrete_Typ_Range is Typ_Kind range Signed_Int_Kind .. Enum_Kind;

   subtype Real_Typ_Range is Typ_Kind range Float_Kind .. Fixed_Kind;

   subtype Array_Typ_Range is
     Typ_Kind range Unconstrained_Array_Kind .. Constrained_Array_Kind;

   subtype Record_Typ_Range
     is Typ_Kind range Disc_Record_Kind .. Non_Disc_Record_Kind;

   function Image (Self : Typ) return String;

   function Kind (Self : Typ) return Typ_Kind;

   type Scalar_Typ (Is_Static : Boolean) is new Typ with null record;

   type Discrete_Typ is new Scalar_Typ with null record;

   function Low_Bound (Self : Discrete_Typ) return Integer with
     Pre => Self.Is_Static;

   function High_Bound (Self : Discrete_Typ) return Integer with
     Pre => Self.Is_Static;

   function Lit_Image (Self : Discrete_Typ; Lit : Integer) return String;
   --  Returns the image of the Litteral whose "position" is Lit. For integer
   --  types, this is simply Lit'Image, for enum types, this correponds to
   --  the image of the enum litteral at position Lit.

   type Access_Typ is new Typ with null record;

   function Image (Self : Access_Typ) return String;

   type Composite_Typ is new Typ with null record;

   package SP is new Shared_Pointers (Element_Type => Typ'Class);

   function As_Discrete_Typ (Self : SP.Ref) return Discrete_Typ'Class is
     (Discrete_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Discrete_Typ_Range);
   pragma Inline (As_Discrete_Typ);

   --  As_<Target>_Typ functions are useful to view a certain Tobjetc of type
   --  Typ'Class wrapped in a smart pointer as a <Target>_Typ, and thus be able
   --  to access the components and primitives defined for that particular
   --  type. The return value is the object encapsulated in the smart pointer,
   --  so under no circumstances should it be freed.

   type Constraint_Value_Kind is (Static, Discriminant, Non_Static);
   --  Constraint kind. Discriminant means that the constraint value is the
   --  value of one of the discriminants of the enclosing record type. Does not
   --  make sense if the constraints are not applied to a component of a
   --  discriminated record type.

   type Constraint_Value (Kind : Constraint_Value_Kind := Non_Static) is
   record
      case Kind is
         when Static =>
            Int_Val : Integer;
            --  The static integer value of the constraint

         when Discriminant =>
            Disc_Name : LAL.Defining_Name;
            Enclosing_Typ : SP.Ref;
            --  The defining name of the discriminant that appears in this
            --  context, as well as a reference to the enclosing record type
            --  in which the discriminant is defined.

         when Non_Static =>
            null;
            --  We don't have any useful info that we can provide here.
            --  May be revisited.
      end case;
   end record;

   type Discrete_Range_Constraint is record
      Low_Bound, High_Bound : Constraint_Value;
   end record;

end TGen.Types;
