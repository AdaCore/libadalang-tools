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
with Ada.Strings.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Langkit_Support.Text; use Langkit_Support.Text;

with TGen.Numerics;    use TGen.Numerics;
with TGen.Subprograms; use TGen.Subprograms;
with TGen.Strings;    use TGen.Strings;
with TGen.Types;      use TGen.Types;

package TGen.Strategies is

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

   function Hash (Name : LAL.Defining_Name) return Ada.Containers.Hash_Type is
     (Ada.Strings.Wide_Wide_Hash (Name.Text));

   function Equivalent_Keys (Left, Right : LAL.Defining_Name) return Boolean is
     (Left.Text = Right.Text);

   package Disc_Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Big_Integer,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
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

   type Dynamic_Strategy_Type is tagged;
   type Dynamic_Strategy_Type_Acc is access all Dynamic_Strategy_Type;

   type Dynamic_Strategy_Type
     (Kind        : Strategy_Kind;
      Constrained : Boolean) is new Strategy_Type with
      record
         Generated : Boolean := True;
         --  Whether the strategy is generated or user-defined (and thus,
         --  requires no generation of code but only checking that the right
         --  functions are defined for the strategy kind). For now, we only
         --  support generated strategies.

         Strategy_Function : Subprogram_Data;
         --  Textual representation of the strategy specification

         Strategy_Body : Unbounded_Text_Type;
         --  Textual representation of the strategy body. Should be left empty
         --  when Generated is False.

         Constrained_Strategy_Function : Dynamic_Strategy_Type_Acc := null;
         --  For constrained types, we also need a separate generation function
         --  that takes care of generating the type once the constraints are
         --  fixed. TODO: rethink about how this should be implemented.

         case Kind is
            when State_Kind =>
               --  Stateful strategies come with an initialization function (to
               --  get the initial state).

               Initialize_Function : Subprogram_Data;
               Initialize_Body : Unbounded_Text_Type;
               --  Textual representation of the initialize procedure body.
               --  Should be left empty when Generated is False;

            when Dispatching_Kind =>
               --  Dispatching strategies delegate the generation to one of the
               --  underlying strategies, randomly picking one of them each
               --  time.

               S1, S2 : Dynamic_Strategy_Type_Acc;
               --  Wrapped strategies

               Bias   : Float;
               --  Probability to pick the first strategy

            when others =>
               null;
         end case;
      end record;

   function "<" (L, R : Dynamic_Strategy_Type) return Boolean is
     (L.Strategy_Function.Name < R.Strategy_Function.Name);

   function Image_Body (Strat : Dynamic_Strategy_Type) return String
     with Pre => Strat.Generated;
   --  Return the full body image of the strategy. Raise an error if this is
   --  not a generated strat. Also returns the Initialize procedure body if
   --  this is a stateful strategy.

   function Image_Spec (Strat : Dynamic_Strategy_Type) return String
     with Pre => Strat.Generated;
   --  Return the spec image of the strategy. Also return the Initialize
   --  procedure spec if this is a stateful strategy.

   function Package_Name (Strat : Dynamic_Strategy_Type) return String is
     (+Strat.Strategy_Function.Parent_Package);

   package Fully_Qualified_Name_To_Strat_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_Text_Type,
        Element_Type    => Strategy_Type'Class,
        Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => Ada.Strings.Wide_Wide_Unbounded."=");
   subtype Fully_Qualified_Name_To_Strat_Map is
     Fully_Qualified_Name_To_Strat_Maps.Map;

   package Strategy_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => Strategy_Type'Class);
   subtype Strategy_Set is Strategy_Sets.Set;

end TGen.Strategies;
