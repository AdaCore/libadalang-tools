------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2011-2021, AdaCore                    --
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

--  This package defines the source file table - the table containing the
--  information about the source files to be processed and the state of their
--  processing. The short file name is used as a key of the table and as an
--  argument of all access/update routines.

package Test.Harness.Source_Table is

   type SF_Status is
     (Bad_Content,
      --  The source file contains a compilable yet unusable code, such as
      --  subprogram declarations.

      Bad_Source,
      --  The file does not contain the compilable source or ASIS context
      --  cannot be opened for a corresponding tree file.

      Bad_Inheritance,
      --  Indicates that a generic package, corresponding to the given
      --  package instatiation was not successfully processed.

      Missing_Body,
      --  The unit containing corrsponding body is not among argument packages

      Not_A_Test,
      --  The unit in the file is compilable and of the right unit type, but
      --  doesn't contain a Test type

      Pending,
      --  Only files containing package instatiations can have this status.
      --  Used to postpone the processing of package instantiations, thus
      --  simplifying corresponding checkings.

      Pending_For_Body,
      --  Files that contain package specs which require body have this status
      --  until proper body is processed.

      Processed_In_Vain,
      --  The unit does have a test type, but does not have any Tests at all
      --  or th test tipe is abstract. No code is generated for such file.

      Processed,
      --  The unit in the source file does have a T-type and one or more
      --  Tests. Such source file cause some code generation.

      Waiting);
      --  Source file hasn't been processed yet

   procedure Add_Source_To_Process (Fname : String);
   --  Adds source to the source table, if there is no source file with same
   --  short name yet. Otherwise ignores it.

   function SF_Table_Empty return Boolean;
   --  Checks if the source table is empty

   function Next_Non_Processed_Source return String;
   --  Returns the name of the next source file stored in the file table
   --  which has not been processed at all yet. If there are no such source
   --  files, returns the name of the next pending source file. If there are no
   --  unprocessed neither pending sources returns empty string.

   function Source_Present (Source_Name : String) return Boolean;
   --  Checks if there is such source name

   ------------------------------------------
   --  Source file access/update routines  --
   ------------------------------------------

   procedure Set_Source_Status (Source_Name : String; New_Status : SF_Status);
   --  Sets status of given source

   function Get_Source_Status          (Source_Name : String) return SF_Status;
   --  Gets source status
   function Get_Source_Full_Name       (Source_Name : String) return String;
   --  Gets source full file name
   function Get_Source_Suffixless_Name (Source_Name : String) return String;
   --  Gets source base name without suffix

   ---------------------------------
   --  Source File location info  --
   ---------------------------------

   procedure Reset_Location_Iterator;
   --  Sets the iterator of source locations list to the initial position

   function Next_Source_Location return String;
   --  Returns the next source path or an emty string if it's the end of list

end Test.Harness.Source_Table;
