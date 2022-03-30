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
--  Defines a global context type for value generation purposes

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Langkit_Support.Text; use Langkit_Support.Text;

with TGen.Strategies;  use TGen.Strategies;
with TGen.Strings;     use TGen.Strings;
with TGen.Subprograms; use TGen.Subprograms;
with TGen.Templates;
with TGen.Types;       use TGen.Types;

package TGen.Context is

   package Fully_Qualified_Name_To_Type_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Text_Type,
      Element_Type => SP.Ref,
      "<"          => Ada.Strings.Wide_Wide_Unbounded."<",
      "="          => SP."=");
   subtype Fully_Qualified_Name_To_Type_Map is
     Fully_Qualified_Name_To_Type_Maps.Map;

   package Unit_To_JSON_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Text_Type,
      Element_Type => JSON_Array,
      "<"          => Ada.Strings.Wide_Wide_Unbounded."<");
   subtype Unit_To_JSON_Map is Unit_To_JSON_Maps.Map;

   package Typ_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => SP.Ref,
      "=" => SP."=");
   subtype Typ_Set is Typ_Sets.Set;

   package Type_Vectors_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_Text_Type,
      Element_Type => Typ_Set,
      "<"          => Ada.Strings.Wide_Wide_Unbounded."<",
      "="          => Typ_Sets."=");

   subtype Type_Vectors_Map is Type_Vectors_Maps.Map;

   type Unsupported_Behavior_Kind is (No_Test, Commented_Out);

   type Generation_Context is new TGen.Templates.Context with record

      Test_Vectors : Unit_To_JSON_Map;
      --  JSON holding the generated test vectors, one for each unit

      Codegen_Required : Boolean := False;
      --  Whether generation of some type values requires a dynamic-validation
      --  step (for non-static types, and for subprograms with a precondition).

      Type_Translations : Fully_Qualified_Name_To_Type_Map;
      --  Contains all the type translations

      Type_And_Param_Strategies :
         TGen.Strategies.Fully_Qualified_Name_To_Strat_Map;
      --  The strategies to use for each type / component of type / parameter /
      --  god knows what.

      Strategies : Strategy_Set;
      --  Aggregation of all the strategies that have been asked for generation

      Packages_Data : Package_Data_Set;
      --  Data for all subprograms we want to test

      Required_Type_Strategies : Type_Vectors_Map;
      --  Targeted types for generation

      Unsupported_Type_Behavior : Unsupported_Behavior_Kind := No_Test;
      --  Behavior of the test generation when encountering an unsupported
      --  type. No_Test means that no test will be generated for the concerned
      --  subprogram, Commented_Out means that tests will be generated, but
      --  commented out, and with some parameter values that will need to be
      --  filed in by the user.

   end record;
   --  General purpose context for test value generation purposes

end TGen.Context;
