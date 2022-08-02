------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Traces;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;

with Laltools.Common; use Laltools.Common;

package Laltools.Refactor is

   Refactor_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("LALTOOLS.REFACTOR", GNATCOLL.Traces.Off);

   type Refactoring_Diagnotic is interface;

   function Filename (Self : Refactoring_Diagnotic) return String is abstract;
   --  Returns the filename of the analysis unit where Self happens.

   function Location
     (Self : Refactoring_Diagnotic)
      return Source_Location_Range is abstract;
   --  Return a location in the file where Self happens.

   function Info (Self : Refactoring_Diagnotic) return String is abstract;
   --  Returns a human readable message with the description of Self.

   package Refactoring_Diagnotic_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Natural,
        Element_Type => Refactoring_Diagnotic'Class);

   subtype Refactoring_Diagnotic_Vector is
     Refactoring_Diagnotic_Vectors.Vector;

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

   procedure Safe_Insert
     (Edits : in out Text_Edit_Ordered_Set;
      Edit  : Text_Edit);
   --  Checks if Edits already contains Edit and if not, inserts it.

   subtype File_Name_Type is String;

   package Text_Edit_Ordered_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type        => File_Name_Type,
      Element_Type    => Text_Edit_Ordered_Set,
      "<"             => "<",
      "="             => Text_Edit_Ordered_Sets."=");

   subtype Text_Edit_Map is Text_Edit_Ordered_Maps.Map;

   function Contains
     (Edits     : Laltools.Refactor.Text_Edit_Map;
      File_Name : Laltools.Refactor.File_Name_Type;
      Edit      : Laltools.Refactor.Text_Edit)
         return Boolean;
   --  Returns True if `Edits` already contains `Edit` for `File_Name`

   procedure Safe_Insert
     (Edits     : in out Text_Edit_Map;
      File_Name : File_Name_Type;
      Edit      : Text_Edit);
   --  If Edits does not contain a File_Name key, then a Text_Edit_Ordered_Set
   --  is created with Edit, and inserted into Edits.
   --  Otherwise, checks if Edits.Element (File_Name) already contains Edit and
   --  if not, inserts it.

   package Unbounded_String_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String,
      "<"          => "<",
      "="          => "=");

   package File_Deletion_Ordered_Sets renames Unbounded_String_Ordered_Sets;

   subtype File_Deletion_Ordered_Set is File_Deletion_Ordered_Sets.Set;

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
         Diagnostics    : Refactoring_Diagnotic_Vector;
      end record;

   No_Refactoring_Edits : constant Refactoring_Edits :=
     Refactoring_Edits'
       (Text_Edits     => Text_Edit_Ordered_Maps.Empty_Map,
        File_Creations => File_Creation_Ordered_Sets.Empty_Set,
        File_Deletions => File_Deletion_Ordered_Sets.Empty_Set,
        File_Renames   => File_Rename_Ordered_Sets.Empty_Set,
        Diagnostics    => Refactoring_Diagnotic_Vectors.Empty_Vector);

   procedure Merge
     (Source : in out Text_Edit_Map;
      Target : Text_Edit_Map);
   --  Merges two maps

   procedure Print (M : Text_Edit_Map);
   --  Print an Edit_Map in an human readable format to the standart output

   procedure Print (S : File_Creation_Ordered_Set);
   --  Print a File_Creation_Ordered_Set in an human readable format to the
   --  standard output

   procedure Print (S : File_Deletion_Ordered_Set);
   --  Print a File_Deletion_Ordered_Set in an human readable format to the
   --  standart output

   procedure Print (S : File_Rename_Ordered_Set);
   --  Print a File_Rename_Ordered_Set in an human readable format to the
   --  standart output

   procedure Print (E : Refactoring_Edits);
   --  Print a Refactoring_Edits in an human readable format to the standart
   --  output.

   type Refactoring_Tool is limited interface;

   function Refactor
     (Self           : Refactoring_Tool;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits is abstract;
   --  Runs the refactoring analysis and return all the needed edits.
   --  No_Refactoring_Edits shall be returned when an error occured.

private

   function Is_Refactoring_Tool_Available_Default_Error_Message
     (Tool_Name : String)
      return String
   is ("Failed to check if the " & Tool_Name & " refactor is available");
   --  Default error message for when Is_<Refactoring_Tool>_Available functions
   --  fail.

   function Refactoring_Tool_Refactor_Default_Error_Message
     (Tool_Name : String)
      return String
   is ("Failed to execute the " & Tool_Name & " refactor");
   --  Default error message for when <Refactoring_Tool>.Refactor functions
   --  fail.

end Laltools.Refactor;
