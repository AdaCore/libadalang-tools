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

   type Text_Edit is
      record
         Location : Source_Location_Range;
         Text     : Unbounded_String;
      end record;

   function "<" (L, R : Text_Edit) return Boolean is (L.Location < R.Location);
   --  Checks if L is < than R, first based on the line number and then on
   --  the column number.

   package Text_Edit_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Text_Edit,
      "<"          => "<",
      "="          => "=");

   subtype Text_Edit_Ordered_Set is Text_Edit_Ordered_Sets.Set;

   package Text_Edit_Ordered_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type        => String,
      Element_Type    => Text_Edit_Ordered_Set,
      "<"             => "<",
      "="             => Text_Edit_Ordered_Sets."=");

   subtype Text_Edit_Map is Text_Edit_Ordered_Maps.Map;

   package Unbounded_String_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String,
      "<"          => "<",
      "="          => "=");

   subtype File_Deletion_Ordered_Set is Unbounded_String_Ordered_Sets.Set;

   type File_Creation is
      record
         Filepath : Unbounded_String;
         Content  : Unbounded_String;
      end record;

   function "<" (L, R : File_Creation) return Boolean is
     (L.Filepath < R.Filepath);

   package File_Creation_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => File_Creation,
      "<"          => "<",
      "="          => "=");

   subtype File_Creation_Ordered_Set is File_Creation_Ordered_Sets.Set;

   type File_Rename is
      record
         Filepath : Unbounded_String;
         New_Name : Unbounded_String;
      end record;

   function "<" (L, R : File_Rename) return Boolean is
     (L.Filepath < R.Filepath);

   package File_Rename_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => File_Rename,
      "<"          => "<",
      "="          => "=");

   subtype File_Rename_Ordered_Set is File_Rename_Ordered_Sets.Set;

   type Refactoring_Edits is
      record
         Text_Edits     : Text_Edit_Map;
         File_Creations : File_Creation_Ordered_Set;
         File_Deletions : File_Deletion_Ordered_Set;
         File_Renames   : File_Rename_Ordered_Set;
      end record;

   procedure Insert
     (Edits    : in out Text_Edit_Map;
      Filename : String;
      Location : Source_Location_Range;
      Text     : Unbounded_String);
   --  Insert a new edit in the map

   procedure Merge
     (Source : in out Text_Edit_Map;
      Target : Text_Edit_Map);
   --  Merges two maps

   procedure Print (M : Text_Edit_Map);
   --  Print an Edit_Map in an human readable format to the standart output

   type Refactoring_Tool is limited interface;

   function Refactor
     (Self           : Refactoring_Tool;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is abstract;
   --  Runs the refactoring analysis and return a map with all the needed edits

end Laltools.Refactor;
