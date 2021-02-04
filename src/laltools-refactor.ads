------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
--  Common refactoring utilities

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Sets;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Common; use Laltools.Common;

package Laltools.Refactor is

   type Edit is
      record
         Location : Source_Location_Range;
         Text     : Unbounded_String;
      end record;

   function "<" (L, R : Edit) return Boolean is (L.Location < R.Location);
   --  Checks if L is < than R, first based on the line number and then on
   --  the column number.

   package Edit_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Edit,
      "<"          => "<",
      "="          => "=");

   subtype Edit_Ordered_Set is Edit_Ordered_Sets.Set;

   package Edit_Ordered_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type        => String,
      Element_Type    => Edit_Ordered_Set,
      "<"             => "<",
      "="             => Edit_Ordered_Sets."=");

   subtype Edit_Map is Edit_Ordered_Maps.Map;

   procedure Insert
     (Edits    : in out Edit_Map;
      Filename : String;
      Location : Source_Location_Range;
      Text     : Unbounded_String);
   --  Insert a new edit in the map

   procedure Merge
     (Source : in out Edit_Map;
      Target : Edit_Map);
   --  Merges two maps

   procedure Print (M : Edit_Map);
   --  Print an Edit_Map in an human readable format to the standart output

   type Refactoring_Tool is limited interface;

   function Refactor
     (Self           : Refactoring_Tool;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Edit_Map is abstract;
   --  Runs the refactoring analysis and return a map with all the needed edits

end Laltools.Refactor;
