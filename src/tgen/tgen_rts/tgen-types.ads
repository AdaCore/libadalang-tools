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
--  Specialized representations and generation functions are available in the
--  various TGen.Types.XXX_Type units.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

limited with TGen.Strategies;
with TGen.Big_Int;   use TGen.Big_Int;
with TGen.Big_Reals; use TGen.Big_Reals;
with TGen.JSON;      use TGen.JSON;
pragma Warnings (Off);
with TGen.Numerics;  use TGen.Numerics;
pragma Warnings (On);
with TGen.Strings;   use TGen.Strings;

package TGen.Types is

   use type Ada_Qualified_Name;

   type Typ is tagged record
      Name : Ada_Qualified_Name;
      --  Fully qualified name of the type

      Last_Comp_Unit_Idx : Positive;
      --  Index, in Name, of the last identifier of the compilation unit in
      --  which this type is declared.

      Fully_Private : Boolean;
      --  Wether this type has no public declaration whatsoever, i.e. it cannot
      --  be referenced outside of the private part of its declaration package,
      --  or outside of the private part of child packages of its declaration
      --  package.

      Private_Extension : Boolean;
      --  Whether this type has a private extension. Note that if Fully_Private
      --  is True, this field will be False.

      Top_Level_Generic : Boolean := False;
      --  If the type comes from a top level generic instantiation.

      Is_Generic : Boolean := False;
      --  If the type is the result of a generic package instantiation

   end record
   with Dynamic_Predicate =>
      --  A top level generic instantion is a generic itself (Top_Level_Generic
      --  implies Is_Generic).
      (if Top_Level_Generic
       then Top_Level_Generic and Is_Generic
       else True);

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
                     Function_Kind,
                     Anonymous_Kind,
                     Instance_Kind,
                     Unsupported);

   subtype Discrete_Typ_Range is Typ_Kind range Signed_Int_Kind .. Enum_Kind;

   subtype Real_Typ_Range is Typ_Kind range Float_Kind .. Fixed_Kind;

   subtype Array_Typ_Range is
     Typ_Kind range Unconstrained_Array_Kind .. Constrained_Array_Kind;

   subtype Record_Typ_Range
     is Typ_Kind range Disc_Record_Kind .. Function_Kind;

   subtype Big_Integer is Big_Int.Big_Integer;

   subtype Big_Real is Big_Reals.Big_Real;

   function Image (Self : Typ) return String;

   function Slug (Self : Typ; Top_Level_Generic : Boolean := False)
      return String;
   --  Return a unique identifier for the type

   function Is_Anonymous (Self : Typ) return Boolean;
   --  Whether Self represents an anonymous type

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

   function Package_Name (Self : Typ) return Ada_Qualified_Name;
   --  Return the package name this type belongs to

   function Compilation_Unit_Name (Self : Typ) return String;
   function Compilation_Unit_Name (Self : Typ) return Ada_Qualified_Name;
   --  Return the name of the compilation unit this type belongs to

   function FQN
      (Self : Typ;
       No_Std : Boolean := False;
       Top_Level_Generic : Boolean := False) return String;
   --  Return the fully qualified name for the type.
   --
   --  If No_Std is True, remove the Standard prefix for entities from the
   --  standard packaged.
   --  The Top_Level_Generic flag can be used to return the generic wrapper
   --  package name instead of the package instantiation one.

   function Is_Constrained (Self : Typ) return Boolean is (False);
   --  An array type with indefinite bounds must be constrained, a discriminant
   --  record type must be constrained.

   function Supports_Static_Gen (Self : Typ) return Boolean is (False);
   --  Whether values for this Typ can be statically generated

   function Default_Strategy (Self : Typ)
      return TGen.Strategies.Strategy_Type'Class;
   --  Return a strategy generating elements of the given type

   function Default_Enum_Strategy
     (Self : Typ) return TGen.Strategies.Enum_Strategy_Type'Class;
   --  Return an enumerative strategy generating elements of the given type

   function Kind (Self : Typ) return Typ_Kind;

   function Encode (Self : Typ; Val : JSON_Value) return JSON_Value;
   --  Encore Val so that all internal representations get turned into actual
   --  Ada values. i.e. Enum positions gets turned into enum literals.

   function Get_Diagnostics
     (Self   : Typ;
      Prefix : String := "") return String_Vector is
     (String_Vectors.To_Vector
        (+To_Ada (Self.Name)
         & ": Non specialized type translation not supported",
        1));
   --  Return a diagnostic string detailing the reason why Self is / depends on
   --  an unsupported type.
   --
   --  Return an empty string if there are no unsupported types in the
   --  transitive closure of Self.

   function Supports_Gen (Self : Typ) return Boolean is (True);
   --  WHether this type supports value generation. False if this type is
   --  an unsupported type (tagged type, access type) or if any of its
   --  components is (transitively) of an unsupported type.

   function JSON_Kind (Kind : Typ_Kind) return JSON_Value_Type is
     (case Kind is
         when Signed_Int_Kind | Mod_Int_Kind | Enum_Kind | Char_Kind =>
            JSON_Int_Type,
         when Bool_Kind =>
            JSON_Boolean_Type,
         when Float_Kind | Fixed_Kind | Decimal_Kind =>
            JSON_Float_Type,
         when Unconstrained_Array_Kind | Constrained_Array_Kind =>
            JSON_Array_Type,
         when others =>
            JSON_Object_Type);

   procedure Free_Content (Self : in out Typ) is null;
   --  Helper for shared pointers

   function Simple_Name (Self : Typ) return String is
     (+Unbounded_String (Self.Name.Last_Element));
   --  Return the simple name for the type

   type Scalar_Typ (Is_Static : Boolean) is new Typ with null record;

   function Get_Diagnostics
     (Self   : Scalar_Typ;
      Prefix : String := "") return String_Vector is
     (String_Vectors.Empty_Vector);
   --  If we get a successful translation to a Scalar_Typ descendent, we should
   --  be able to generate anything.

   type Composite_Typ is new Typ with null record;

   procedure Free_Content_Wide (Self : in out Typ'Class);
   --  Helper for shared pointers

   package SP is new Shared_Pointers
     (Element_Type => Typ'Class, Release => Free_Content_Wide);

   function "<" (L, R : SP.Ref) return Boolean is
     (To_Ada (L.Get.Name) < To_Ada (R.Get.Name));

   function "=" (L, R : SP.Ref) return Boolean is
     ((L.Is_Null and then R.Is_Null)
       or else (not L.Is_Null
                and then not R.Is_Null
                and then L.Get.Name = R.Get.Name));

   function Try_Generate_Static
     (Self : SP.Ref) return TGen.Strategies.Strategy_Type'Class;
   --  Return a static strategy if the type supports it, otherwise return
   --  a Commented_Out_Strategy or raise program error depending on the
   --  behavior specified in the context.

   --  As_<Target>_Typ functions are useful to view a certain object of type
   --  Typ'Class wrapped in a smart pointer as a <Target>_Typ, and thus be able
   --  to access the components and primitives defined for that particular
   --  type. The return value is the object encapsulated in the smart pointer,
   --  so under no circumstances should it be freed.

   Big_Zero : constant Big_Integer :=
     TGen.Big_Int.To_Big_Integer (0);

   Big_Zero_F : constant Big_Reals.Big_Real :=
     TGen.Big_Reals.To_Real (0);

   type Unsupported_Typ is new Typ with record
      Reason : Unbounded_String;
      --  Why this type is not supported.

   end record;

   function Get_Diagnostics
     (Self   : Unsupported_Typ;
      Prefix : String := "") return String_Vector;

   function Supports_Gen (Self : Unsupported_Typ) return Boolean is (False);
   --  Unsupported types are unsupported for generation.

   function Kind (Self : Unsupported_Typ) return Typ_Kind is (Unsupported);

   type Access_Typ is new Unsupported_Typ with null record;

   function Image (Self : Access_Typ) return String;

   type Formal_Typ is new Unsupported_Typ with null record;

   type Unsupported_Types is new Typ with record
      Diags : String_Vectors.Vector;
      --  Why this type is not supported.

   end record;

   function Get_Diagnostics
     (Self   : Unsupported_Types;
      Prefix : String := "") return String_Vector
   is (Self.Diags);

   function Kind (Self : Unsupported_Types) return Typ_Kind is (Unsupported);

private

   function Generic_Package_Instance_Name (Pack_Name : Ada_Qualified_Name)
      return Ada_Qualified_Name;
   --  Return the fully qualified name of a generic instance.

end TGen.Types;
