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
--  Random value generation strategies. For the moment this concerns only
--  static generation strategies, but will also be applicable to dynamic
--  generation later on.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with TGen.Numerics;    use TGen.Numerics;
with TGen.Strings;    use TGen.Strings;
with TGen.Types;      use TGen.Types;

package TGen.Strategies is

   type Generation_Context is tagged null record;
   --  Context for the generation of parameter values. For now, no information
   --  is needed as generation is not contextual.

   type Strategy_Kind is
     (Random_Kind, State_Kind, Dispatching_Kind, Wrapping_Kind);

   type Strategy_Type is abstract tagged null record;

   function "<" (L, R : Strategy_Type'Class) return Boolean;
   --  TODO: implement this function properly

   type Unimplemented_Strategy_Type is new Strategy_Type with null record;

   type Static_Value is abstract tagged null record;

   function To_String (Self : Static_Value) return String is abstract;

   type Base_Static_Value is new Static_Value with
      record
         Value : Unbounded_String;
      end record;

   overriding function To_String
     (Self : Base_Static_Value) return String is
     (+Self.Value);

   package Disc_Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Big_Integer,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => Big_Int."=");
   subtype Disc_Value_Map is Disc_Value_Maps.Map;

   function "<" (L, R : Static_Value'Class) return Boolean is
     (L.To_String < R.To_String);

   function "=" (L, R : Static_Value'Class) return Boolean is
      (L.To_String = R.To_String);

   package Static_Value_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => Static_Value'Class);
   subtype Static_Value_Set is Static_Value_Sets.Set;

   type Static_Strategy_Type is abstract new Strategy_Type with
     null record;
   function Generate_Static_Value
     (S : in out Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
      is abstract;
   --  A static strategy returns a string representation of the generated
   --  value, as we can't have a type that is generic enough to represent
   --  all of the Ada values (we could probably use the yeison data
   --  structure defined here https://github.com/mosteo/yeison, but it would
   --  mean giving up the named notation for record initialization).
   --
   --  Note that in the future, this should be enhance as some objects
   --  require statement sequences to be initialized (context objects, or
   --  even containers before the aggregate notation was introduced for
   --  them). TODO in the future.

   type Static_Strategy_Acc is access all Static_Strategy_Type'Class;

   type Unimplemented_Static_Strategy_Type is new Static_Strategy_Type with
     null record;
   overriding function Generate_Static_Value
     (S : in out Unimplemented_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
     (raise Program_Error with "Unimplemented static strategy");

   type Commented_Out_Strategy_Type is new Static_Strategy_Type with
     null record;
   --  Strategy that generates comments stating that the user should fill in
   --  a valid value.

   overriding function Generate_Static_Value
     (S : in out Commented_Out_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class is
     (Base_Static_Value'(Value => To_Unbounded_String
        ("--  Replace this comment by a valid value")));

   type Basic_Static_Strategy_Type is new Static_Strategy_Type with record
      T : SP.Ref;
      F : access function (T : Typ'Class) return Static_Value'Class;
   end record;

   overriding function Generate_Static_Value
     (S : in out Basic_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class
   is
     (S.F (S.T.Get));

   type Dispatching_Static_Strategy_Type is new Static_Strategy_Type with
      record
         Bias : Float;
         S1, S2 : Static_Strategy_Acc;
      end record;
   overriding function Generate_Static_Value
     (S : in out Dispatching_Static_Strategy_Type;
      Disc_Context : Disc_Value_Map) return Static_Value'Class;

   function Make_Dispatching_Strat
     (S1, S2 : Static_Strategy_Type'Class;
      Bias   : Float := 0.5) return Dispatching_Static_Strategy_Type;

   generic
      type Equivalence_Class_Type is private;
      with package Equivalence_Classes_Vectors is
        new Ada.Containers.Vectors
          (Index_Type => Positive, Element_Type => Equivalence_Class_Type);
   package Equivalence_Classes_Strategy_Package is
      type Equivalence_Class_Strategy_Type is new Static_Strategy_Type with
         record
            T       : SP.Ref;
            Classes : Equivalence_Classes_Vectors.Vector;
            Draw : access function
              (T     : SP.Ref;
               Class : Equivalence_Class_Type) return Static_Value'Class;
         end record;
      overriding function Generate_Static_Value
        (S : in out Equivalence_Class_Strategy_Type;
         Disc_Context : Disc_Value_Map) return Static_Value'Class;
   end Equivalence_Classes_Strategy_Package;

   package Strategy_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => Strategy_Type'Class);
   subtype Strategy_Set is Strategy_Sets.Set;

end TGen.Strategies;
