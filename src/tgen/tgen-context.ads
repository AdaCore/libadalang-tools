------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Langkit_Support.Text; use Langkit_Support.Text;

with TGen.JSON;        use TGen.JSON;
with TGen.Strategies;  use TGen.Strategies;
with TGen.Strings;     use TGen.Strings;
with TGen.Subprograms; use TGen.Subprograms;
with TGen.Types;       use TGen.Types;

package TGen.Context is

   package Fully_Qualified_Name_To_Type_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_Text_Type,
        Element_Type => Typ_Access,
        "<"          => Ada.Strings.Wide_Wide_Unbounded."<",
        "="          => TGen.Types."=");
   subtype Fully_Qualified_Name_To_Type_Map is
     Fully_Qualified_Name_To_Type_Maps.Map;

   package Unit_To_JSON_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_Text_Type,
        Element_Type => JSON_Value,
        "<"          => Ada.Strings.Wide_Wide_Unbounded."<");
   subtype Unit_To_JSON_Map is Unit_To_JSON_Maps.Map;

   package Typ_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Typ_Access,
        "="          => TGen.Types."=");
   subtype Typ_Set is Typ_Sets.Set;

   package Typ_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Element_Type => Typ_Access);
   subtype Typ_List is Typ_Lists.List;

   package Type_Vectors_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Unbounded_Text_Type,
        Element_Type => Typ_Set,
        "<"          => Ada.Strings.Wide_Wide_Unbounded."<",
        "="          => Typ_Sets."=");
   subtype Type_Vectors_Map is Type_Vectors_Maps.Map;

   type Unsupported_Behavior_Kind is (No_Test, Commented_Out);

   package Fully_Qualified_Name_To_Strat_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Unbounded_Text_Type,
        Element_Type    => Strategy_Type'Class,
        Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => Ada.Strings.Wide_Wide_Unbounded."=");
   subtype Fully_Qualified_Name_To_Strat_Map is
     Fully_Qualified_Name_To_Strat_Maps.Map;

   type Generation_Context is record
      Output_Dir : Unbounded_String;
      --  Directory for generated artifacts

      Test_Vectors : Unit_To_JSON_Map;
      --  JSON holding the generated test vectors, one for each unit.
      --  The keys are the simple filenames in which the test vectors will be
      --  written.

      Codegen_Required : Boolean := False;
      --  Whether generation of some type values requires a dynamic-validation
      --  step (for non-static types, and for subprograms with a precondition).

      Type_And_Param_Strategies : Fully_Qualified_Name_To_Strat_Map;
      --  The strategies to use for each type / component of type / parameter /
      --  god knows what.

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
   --  General purpose context for test value generation purposes. TODO:
   --  merge this with TGen.Libgen.Libgen_Context.

end TGen.Context;
