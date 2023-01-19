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
--
--  Internal type representation and associated generation functions.
--  Specialized representations and generation functions are avalaible in the
--  various TGen.Types.XXX_Type units.

with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;     use GNATCOLL.JSON;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;

limited with TGen.Strategies;
with TGen.Numerics;    use TGen.Numerics;
with TGen.Strings; use TGen.Strings;

package TGen.Types is

   use type Ada_Qualified_Name;

   type Typ is tagged record
      Name : Ada_Qualified_Name;
      --  Fully qualified name of the type

      Last_Comp_Unit_Idx : Positive;
      --  Index, in Name, of the last identifier of the compilation unit in
      --  which this type is declared.

   end record;

   function Template return String is ("");

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
                     Non_Disc_Record_Kind,
                     Anonymous_Kind,
                     Unsupported);

   subtype Discrete_Typ_Range is Typ_Kind range Signed_Int_Kind .. Enum_Kind;

   subtype Real_Typ_Range is Typ_Kind range Float_Kind .. Fixed_Kind;

   subtype Array_Typ_Range is
     Typ_Kind range Unconstrained_Array_Kind .. Constrained_Array_Kind;

   subtype Record_Typ_Range
     is Typ_Kind range Disc_Record_Kind .. Non_Disc_Record_Kind;

   subtype Big_Integer is Big_Int.Big_Integer;

   subtype Big_Real is Big_Reals.Big_Real;

   function Image (Self : Typ) return String;

   function Is_Anonymous (Self : Typ) return Boolean;
   --  Whether Self represents an anonymous type

   function Fully_Qualified_Name (Self : Typ) return String is
     (To_Ada (Self.Name));
   --  Return the FQN of Self

   function Type_Name (Self : Typ) return String is
     (if Ada_Identifier_Vectors.Is_Empty (Self.Name)
      then "anonymous"
      else +Unbounded_String (Self.Name.Last_Element));
   --  Return the name of Self

   function Gen_Random_Function_Name
     (Self : Typ) return String is
     ("Gen_" & To_Ada (Self.Name));
   --  Return the name of the function generation random values for self in the
   --  sources for dynamic (two pass) generation

   function "<" (L : Typ'Class; R : Typ'Class) return Boolean is
     (To_Ada (L.Name) < To_Ada (R.Name));

   function Slug (Self : Typ) return String;
   --  Return a unique identifier for the type

   function Package_Name (Self : Typ) return String;
   function Package_Name (Self : Typ) return Ada_Qualified_Name;
   --  Return the package name this type belongs to

   function Compilation_Unit_Name (Self : Typ) return String;
   function Compilation_Unit_Name (Self : Typ) return Ada_Qualified_Name;
   --  Return the name of the compilation unit this type belongs to

   function Is_Constrained (Self : Typ) return Boolean is (False);
   --  An array type with indefinite bounds must be constrained, a discriminant
   --  record type must be constrained.

   function Supports_Static_Gen (Self : Typ) return Boolean is (False);
   --  Wether values for this Typ can be statically generated

   function Default_Strategy (Self : Typ)
      return TGen.Strategies.Strategy_Type'Class;
   --  Return a strategy generating elements of the given type

   function Kind (Self : Typ) return Typ_Kind;

   procedure Free_Content (Self : in out Typ) is null;
   --  Helper for shared pointers

   type Scalar_Typ (Is_Static : Boolean) is new Typ with null record;

   type Composite_Typ is new Typ with null record;

   procedure Free_Content_Wide (Self : in out Typ'Class);
   --  Helper for shared pointers

   package SP is new Shared_Pointers
     (Element_Type => Typ'Class, Release => Free_Content_Wide);

   function "<" (L, R : SP.Ref) return Boolean is
     (To_Ada (L.Get.Name) < To_Ada (R.Get.Name));

   function "=" (L, R : SP.Ref) return Boolean is
     ((L.Is_Null and then R.Is_Null)
       or else ((not L.Is_Null)
                and then (not R.Is_Null)
                and then L.Get.Name = R.Get.Name));

   function Try_Generate_Static
     (Self    : SP.Ref)
      return TGen.Strategies.Strategy_Type'Class;
   --  Return a static strategy if the type supports it, otherwise return
   --  a Commented_Out_Strategy or raise program error depending on the
   --  behavior specified in the context.

   --  As_<Target>_Typ functions are useful to view a certain object of type
   --  Typ'Class wrapped in a smart pointer as a <Target>_Typ, and thus be able
   --  to access the components and primitives defined for that particular
   --  type. The return value is the object encapsulated in the smart pointer,
   --  so under no circumstances should it be freed.

   Big_Zero : constant Big_Integer :=
     Ada.Numerics.Big_Numbers.Big_Integers.To_Big_Integer (0);

   Big_Zero_F : constant Big_Reals.Big_Real := Big_Reals.To_Real (0);

   type Unsupported_Typ is new Typ with null record;

   function Kind (Self : Unsupported_Typ) return Typ_Kind is (Unsupported);

   type Access_Typ is new Unsupported_Typ with null record;

   function Image (Self : Access_Typ) return String;

   type Formal_Typ is new Unsupported_Typ with null record;

end TGen.Types;
