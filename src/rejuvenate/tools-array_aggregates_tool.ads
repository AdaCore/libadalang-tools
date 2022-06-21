------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
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
--  Rejuvenate array aggregates tool

with Ada.Strings.Unbounded;
with Libadalang.Analysis; use Libadalang.Analysis;
with Laltools.Refactor;
with Ada.Containers.Indefinite_Ordered_Maps;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with VSS.Text_Streams;

package Tools.Array_Aggregates_Tool is
   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Array Aggregates");

   function "<" (L, R : Aggregate) return Boolean;

   package Aggregates_To_Edit_Text is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Aggregate,
        Element_Type => Laltools.Refactor.Text_Edit_Ordered_Set,
        "<"          => "<",
        "="          => Laltools.Refactor.Text_Edit_Ordered_Sets."=");

   function Find_Arrays_To_Aggregate
     (Unit_Array : Analysis_Unit_Array)
      return Aggregates_To_Edit_Text.Map;

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Source is new Parse_Option
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   procedure Run
     (Units  : Analysis_Unit_Array;
      Stream : in out VSS.Text_Streams.Output_Text_Stream'Class);
   --  Array_Aggregates_Tool main procedure

end Tools.Array_Aggregates_Tool;
