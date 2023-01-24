------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2022-2023, AdaCore                    --
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
--  Lint array aggregates tool

with Ada.Containers.Indefinite_Ordered_Maps;

with Libadalang.Analysis;

with Laltools.Refactor;

package Lint.Tools.Array_Aggregates_Tool is

   function "<" (L, R : Libadalang.Analysis.Aggregate) return Boolean;
   --  First compares the Aggregate Analysis_Unit filename and then their
   --  Source_Location.

   package Aggregates_To_Text_Edit_Ordered_Set_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Libadalang.Analysis.Aggregate,
        Element_Type => Laltools.Refactor.Text_Edit_Ordered_Set,
        "<"          => "<",
        "="          => Laltools.Refactor.Text_Edit_Ordered_Sets."=");

   subtype Aggregate_Edits is Aggregates_To_Text_Edit_Ordered_Set_Maps.Map;

   function Upgrade_Array_Aggregates
     (Units : Libadalang.Analysis.Analysis_Unit_Array)
      return Aggregate_Edits;
   --  Runs the array aggregates tool on Units. This is the main entry point
   --  of this tool when used in library mode.

   function Upgrade_Array_Aggregates
     (Units : Libadalang.Analysis.Analysis_Unit_Array)
      return Laltools.Refactor.Text_Edit_Map;
   --  Runs the array aggregates tool on Units. This is the main entry point
   --  of this tool when used in library mode.

   procedure Run;
   --  Runs the array aggregates tool.
   --  This procedure should by a driver only. User should use the library mode
   --  equivalent Upgrade_Array_Aggregates defined above.

end Lint.Tools.Array_Aggregates_Tool;
