------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

with Libadalang.Analysis; use Libadalang.Analysis;

with TGen.Types.Record_Types; use TGen.Types.Record_Types;

--  This is a proof of concept for specification of strategies and by no
--  means should be considered a full-fledged implementation. It supports
--  only custom strategies for scalar types for now. The way to specify a
--  strategy right now is best shown on an example:
--
--  function My_Random return Integer;
--  procedure Foo (I : Integer)
--     with Generation => (Strategies => (I => My_Random));
--
--  In the example above, we specify a custom strategy for I configuring the
--  tool to use the My_Random function to generate values for I.
--
--  This strategy specification scheme should be extended to all of the
--  supported types, and should also support built-in tgen strategies that are
--  not the default.

package TGen.Parse_Strategy is

   --  A strategy can be specified as an aspect at different spots.
   --
   --    * As an aspect for a type declaration in which case it will apply
   --      to all of the type's references.
   --
   --    * As an aspect for a function declaration,  for a subset of the
   --      parameters type. This overrides the previous setting.
   --
   --    * As a parameter of the Test_Case aspect. This overrides the
   --      previous settings.
   --
   --  A strategy can be of different kinds:
   --
   --    * Random kind: an arbitrary number of values can be generated from
   --      the strategy.
   --
   --    * Enumerative kind: a specific number of values can be generated
   --      from the strategy. This includes pairwise-testing strategies which
   --      enumerate in a way that does not come with a combinartorial
   --      explosion.

   type Strategy_Kind is (Custom, Predefined);
   type Parsed_Strategy (Kind : Strategy_Kind := Predefined) is record
      case Kind is
         when Custom =>
            --  When this is a custom strategy, we are specifying a function
            --  name. A TGen strategy will be generated from this function
            --  name.

            Generate_Name : Unbounded_String;

         when Predefined =>
            Predefined_Str : Unbounded_String;
      end case;
   end record;

   Strategy_Parsing_Error : exception;
   --  Exception raised when a strategy expression is malformed

   package FQN_To_Parsed_Strat_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Parsed_Strategy,
        Hash            => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);
   subtype FQN_To_Parsed_Strat_Map is FQN_To_Parsed_Strat_Maps.Map;

   procedure Parse_Strategy
     (Fct_Typ    : in out Function_Typ'Class;
      Aspect     : Libadalang.Analysis.Aspect_Assoc;
      Strategies : out FQN_To_Parsed_Strat_Maps.Map);
   --  Parse a strategy aspect. For each strategy that is specified for a
   --  parameter / parameter component, put an entry in the Strategies map.
   --
   --  Also returns a function type where every parameter / parameter component
   --  type to which a specified strategies applies to has been replaced by
   --  an Instance_Typ, to identify it by its fully qualified name in the
   --  specific function context.

end TGen.Parse_Strategy;
