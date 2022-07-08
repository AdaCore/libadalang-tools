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
--  Type representation for real types, and associated generation functions

with TGen.Context;    use TGen.Context;
with TGen.Strategies; use TGen.Strategies;

package TGen.Types.Real_Types is

   type Float_Range is record
      Min, Max : Big_Real;
   end record;

   type Real_Typ is new Scalar_Typ with null record;

   function Low_Bound_Or_Default (Self : Real_Typ) return Big_Real is
     (LF_Conversions.To_Big_Real (Long_Float'First));
   --  Return the low bound for Self if it has one explicitly defined, or
   --  Long_Float'First otherwise.

   function High_Bound_Or_Default (Self : Real_Typ) return Big_Real is
     (LF_Conversions.To_Big_Real (Long_Float'Last));
   --  Return the high bound for Self if it has one explicitly defined, or
   --  Long_Float'Last otherwise.

   type Float_Typ (Is_Static, Has_Range : Boolean) is new
     Real_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Digits_Value : Natural;
            case Has_Range is
               when True =>
                  Range_Value : Float_Range;
               when False =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end record;

   function Supports_Static_Gen (Self : Float_Typ) return Boolean is
     (Self.Is_Static);
   --  Wether values for this Typ can be statically generated

   function Low_Bound_Or_Default (Self : Float_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the low bound for Self if it has one explicitly defined, or
   --  Long_Float'First otherwise.

   function High_Bound_Or_Default (Self : Float_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the high bound for Self if it has one explicitly defined, or
   --  Long_Float'Last otherwise.

   function Image (Self : Float_Typ) return String;

   function Kind (Self : Float_Typ) return Typ_Kind is (Float_Kind);

   overriding function Generate_Static
     (Self    : Float_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;
   --  Generate a strategy to statically generate (in one pass) values for Self

   overriding function Generate_Random_Strategy
     (Self    : Float_Typ;
      Context : in out Generation_Context) return Strategy_Type'Class;
   --  Generate a strategy for dynamic (two pass) generation to generate
   --  uniformly distributed values.

   function As_Float_Typ (Self : SP.Ref) return Float_Typ'Class is
     (Float_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Float_Kind);
   pragma Inline (As_Float_Typ);

   type Ordinary_Fixed_Typ (Is_Static : Boolean) is new
     Real_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Delta_Value : Big_Real;
            Range_Value : Float_Range;
         when others =>
            null;
      end case;
   end record;

   function Supports_Static_Gen (Self : Ordinary_Fixed_Typ) return Boolean is
     (Self.Is_Static);
   --  Wether values for this Typ can be statically generated

   overriding function Generate_Static
     (Self    : Ordinary_Fixed_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;
   --  Generate a strategy to statically generate (in one pass) values for Self

   function Low_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the low bound for Self if it has one explicitly defined, or
   --  Long_Float'First otherwise.

   function High_Bound_Or_Default (Self : Ordinary_Fixed_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the high bound for Self if it has one explicitly defined, or
   --  Long_Float'Last otherwise.

   function Image (Self : Ordinary_Fixed_Typ) return String;

   function Kind (Self : Ordinary_Fixed_Typ) return Typ_Kind is (Fixed_Kind);

   function As_Ordinary_Fixed_Typ (Self : SP.Ref)
     return Ordinary_Fixed_Typ'Class is
     (Ordinary_Fixed_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Fixed_Kind);
   pragma Inline (As_Ordinary_Fixed_Typ);

   type Decimal_Fixed_Typ (Is_Static, Has_Range : Boolean) is new
     Real_Typ (Is_Static => Is_Static) with record
      case Is_Static is
         when True =>
            Delta_Value  : Big_Real;
            Digits_Value : Natural;
            case Has_Range is
               when True =>
                  Range_Value : Float_Range;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end record;

   function Supports_Static_Gen (Self : Decimal_Fixed_Typ) return Boolean is
     (Self.Is_Static);
   --  Wether values for this Typ can be statically generated

   overriding function Generate_Static
     (Self    : Decimal_Fixed_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;

   function Image (Self : Decimal_Fixed_Typ) return String;

   function Kind (Self : Decimal_Fixed_Typ) return Typ_Kind is (Decimal_Kind);

   function Low_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the low bound for Self if it has one explicitly defined, or
   --  Long_Float'First otherwise.

   function High_Bound_Or_Default (Self : Decimal_Fixed_Typ) return Big_Real
   with Pre => Self.Is_Static;
   --  Return the high bound for Self if it has one explicitly defined, or
   --  Long_Float'Last otherwise.

   function As_Decimal_Fixed_Typ (Self : SP.Ref)
     return Decimal_Fixed_Typ'Class is
     (Decimal_Fixed_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Decimal_Kind);
   pragma Inline (As_Decimal_Fixed_Typ);

   generic
      type T is digits <>;
   function Gen return T;
   --  Generate a random value for T, between 0.0 and 1.0 for the moment

end TGen.Types.Real_Types;
