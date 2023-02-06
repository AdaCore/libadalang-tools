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
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with TGen.JSON;    use TGen.JSON;
with TGen.Strings; use TGen.Strings;
with TGen.Types;   use TGen.Types;

package TGen.Strategies is

   type Generation_Context is tagged null record;
   --  Context for the generation of parameter values. For now, no information
   --  is needed as generation is not contextual.

   type Strategy_Kind is
     (Random_Kind, State_Kind, Dispatching_Kind, Wrapping_Kind);

   package Disc_Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => JSON_Value,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => TGen.JSON."=");
   subtype Disc_Value_Map is Disc_Value_Maps.Map;

   type Strategy_Type is abstract tagged null record;
   function Generate
     (S            : in out Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
      is abstract;

   type Strategy_Acc is access all Strategy_Type'Class;

   type Unimplemented_Strategy_Type is new Strategy_Type with
     null record;

   overriding function Generate
     (S            : in out Unimplemented_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
     (raise Program_Error with "Unimplemented static strategy");

   type Commented_Out_Strategy_Type is new Strategy_Type with
     null record;
   --  Strategy that generates comments stating that the user should fill in
   --  a valid value.

   overriding function Generate
     (S            : in out Commented_Out_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   type Basic_Strategy_Type is new Strategy_Type with record
      T : SP.Ref;
      --  A generic strategy will always need to store the type it references,
      --  to do introspection and know, e.g. the bounds of the type if it is a
      --  scalar type.

      F : access function (T : Typ'Class) return JSON_Value;
      --  The actual generation function. See below the instantiation of the
      --  Generate function.
   end record;

   overriding function Generate
     (S            : in out Basic_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value
   is
     (S.F (S.T.Get));

   type Dispatching_Strategy_Type is new Strategy_Type with
      record
         Bias   : Float;
         S1, S2 : Strategy_Acc;
      end record;
   --  This strategy dispatches the generation to two strategies, according
   --  to a certain bias. This is useful when mixing two strategies, e.g.
   --  a random one and an enumerative one.

   overriding function Generate
     (S            : in out Dispatching_Strategy_Type;
      Disc_Context : Disc_Value_Map) return JSON_Value;

   function Make_Dispatching_Strat
     (S1, S2 : Strategy_Type'Class;
      Bias   : Float := 0.5) return Dispatching_Strategy_Type;

   generic
      type Equivalence_Class_Type is private;
      with package Equivalence_Classes_Vectors is
        new Ada.Containers.Vectors
          (Index_Type => Positive, Element_Type => Equivalence_Class_Type);
   package Equivalence_Classes_Strategy_Package is
      type Equivalence_Class_Strategy_Type is new Strategy_Type with
         record
            T       : SP.Ref;
            Classes : Equivalence_Classes_Vectors.Vector;
            Draw : access function
              (T     : SP.Ref;
               Class : Equivalence_Class_Type) return JSON_Value;
         end record;
      --  This strategy implements equivalence classes strategy, i.e.
      --  strategies where we will pick in an equivalence class each time
      --  we are generating a value. Note that we can pick several time a value
      --  from the same equivalence class. TODO: fix this?

      overriding function Generate
        (S            : in out Equivalence_Class_Strategy_Type;
         Disc_Context : Disc_Value_Map) return JSON_Value;
   end Equivalence_Classes_Strategy_Package;

end TGen.Strategies;
