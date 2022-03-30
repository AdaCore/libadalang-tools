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
--  Common functions for manipulation and generation of discrete types

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

with TGen.Context;    use TGen.Context;
with TGen.Strategies; use TGen.Strategies;

package TGen.Types.Discrete_Types is

   type Discrete_Typ is new Scalar_Typ with null record;

   type Int_Range is record
      Min, Max : Big_Integer;
   end record;

   function To_String (Rang : Int_Range) return String is
     ("[" & Big_Int.To_String (Rang.Min) & ", "
      & Big_Int.To_String (Rang.Max) & "]");

   function "<" (L, R : Int_Range) return Boolean is
     (if Big_Int."=" (L.Min, R.Min)
      then Big_Int."<" (L.Max, R.Max)
      else Big_Int."<" (L.Min, R.Min));

   function "=" (L, R : Int_Range) return Boolean is
     (Big_Int."=" (L.Min, R.Min) and then Big_Int."=" (L.Max, R.Max));

   --  Utilities for interval sets

   package Alternatives_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Int_Range,
      "<"          => "<",
      "="          => "=");
   subtype Alternatives_Set is Alternatives_Sets.Set;

   package Alternatives_Set_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Alternatives_Set,
      "="          => Alternatives_Sets."=");
   subtype Alternatives_Set_Vector is Alternatives_Set_Vectors.Vector;

   function Draw (Intervals : Alternatives_Set) return Big_Integer;
   --  Draw a value within one of the intervals defined in Intervals

   function Low_Bound (Self : Discrete_Typ) return Big_Integer with
     Pre => Self.Is_Static;
   --  Return the lowest bound of Self

   function High_Bound (Self : Discrete_Typ) return Big_Integer with
     Pre => Self.Is_Static;
   --  Return the highest bound of Self

   function Lit_Image (Self : Discrete_Typ; Lit : Big_Integer) return String;
   --  Returns the image of the Litteral whose "position" is Lit. For integer
   --  types, this is simply Lit'Image, for enum types, this correponds to
   --  the image of the enum litteral at position Lit.

   overriding function Generate_Static
     (Self    : Discrete_Typ;
      Context : in out Generation_Context) return Static_Strategy_Type'Class;
   --  Generate a strategy to statically generate (in one pass) values for Self

   type Discrete_Static_Value is new Static_Value with record
      T     : SP.Ref;
      Value : Big_Integer;
   end record;

   overriding function To_String (Self : Discrete_Static_Value) return String;

   type Sample_Static_Strategy_Type is new Static_Strategy_Type
     with record
      T       : SP.Ref;
      Samples : Alternatives_Set_Vector;
   end record;

   overriding function Generate_Static_Value
     (S            : in out Sample_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class;
   --  Given a static sampling strategy, generate one single value from it

   function Generate_Sampling_Strategy
     (Self    : Discrete_Typ;
      Samples : Alternatives_Set_Vector) return Static_Strategy_Type'Class;
   --  Generate a static (single pass generation) sampling strategy for Self

   function As_Discrete_Typ (Self : SP.Ref) return Discrete_Typ'Class is
     (Discrete_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Discrete_Typ_Range);
   pragma Inline (As_Discrete_Typ);

end TGen.Types.Discrete_Types;
