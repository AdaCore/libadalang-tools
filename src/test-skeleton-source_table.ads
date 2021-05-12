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

with Test.Common; use Test.Common;

with GNATCOLL.Projects;          use GNATCOLL.Projects;

package Test.Skeleton.Source_Table is

   type SF_Status is
     (Bad_Content,
      --  The source file contains a compilable yet unusable code, such as
      --  subprogram declarations.

      Bad_Source,
      --  The file does not contain the compilable source or ASIS context
      --  cannot be opened for a corresponding tree file.

      Bad_Inheritance,
      --  The unit depends via containing tagged type on a unit marked as
      --  Multiple_Tagged_Records.
      --  In case of generic instatiations, that means that the corresponding
      --  generic package is either not a test package or not an argument
      --  package.

      Pending,
      --  Only files containing package instatiations can have this status.
      --  Used to postpone the processing of package instantiations, thus
      --  simplifying corresponding checkings.

      To_Stub_Body,
      Stubbed_Body,
      --  Used in stub mode to store bodies.

      Body_Reference,
      --  Used in stub mode to store non-argument specs.

      Processed_In_Vain,
      --  The unit does have a test type, but does not have any Tests at all
      --  or th test tipe is abstract. No code is generated for such file.

      Processed,
      --  The unit in the source file does have a T-type and one or more
      --  Tests. Such source file cause some code generation.

      Waiting);
      --  Source file hasn't been processed yet.

   procedure Add_Source_To_Process (Fname : String);
   --  Adds source to the source table, if there is no source file with same
   --  short name yet. Otherwise ignores it.

   procedure Add_Body_To_Process
     (Fname : String; Pname : String; Uname : String);
   --  Adds body sources for stub mode.

   procedure Add_Body_Reference (Fname : String);
   --  In stub mode we need references from non-argument sources to their
   --  bodies in case we'd need to stub any of them.

   function SF_Table_Empty return Boolean;
   --  Checks if the source table is empty

   function Next_Non_Processed_Source return String;
   --  Returns the name of the next source file stored in the file table
   --  which has not been processed at all yet. If there are no such source
   --  files, returns the name of the next pending source file. If there are no
   --  unprocessed neither pending sources returns empty string.

   function Get_Current_Source_Spec return String;
   --  Returns the name of the current source file stored in the file table
   --  which is bein been processed at the moment.

   function Source_Present (Source_Name : String) return Boolean;
   --  Checks if there is such source name

   ------------------------------------------
   --  Source file access/update routines  --
   ------------------------------------------
   procedure Set_Source_Status (Source_Name : String; New_Status : SF_Status);

   function Get_Source_Status          (Source_Name : String) return SF_Status;
   function Get_Source_Suffixless_Name (Source_Name : String) return String;
   function Get_Source_Output_Dir      (Source_Name : String) return String;
   function Get_Source_Stub_Dir        (Source_Name : String) return String;
   function Get_Source_Stub_Data_Spec  (Source_Name : String) return String;
   function Get_Source_Stub_Data_Body  (Source_Name : String) return String;
   function Get_Source_Body            (Source_Name : String) return String;
   function Get_Source_Project_Name    (Source_Name : String) return String;
   function Get_Source_Unit_Name       (Source_Name : String) return String;

   procedure Mark_Sourse_Stubbed (Source_Name : String);
   function Source_Stubbed (Source_Name : String) return Boolean;

   procedure Reset_Source_Iterator;
   --  Sets the iterator of source files list in the initial position.

   function Next_Source_Name return String;
   --  Returns the name of the next source file stored in the file table.

   ---------------------------------
   --  Source File location info  --
   ---------------------------------

   procedure Reset_Location_Iterator;
   --  Sets the iterator of source locations list to the initial position.

   function Next_Source_Location return String;
   --  Returns the next source path or an emty string if it's the end of list.

   --------------------
   --  Output Setup  --
   --------------------

   procedure Set_Subdir_Output;

   procedure Set_Separate_Root (Max_Common_Root : String);

   procedure Set_Direct_Output;

   procedure Set_Direct_Stub_Output;

   procedure Set_Output_Dir (Source_Name : String; Output_Dir : String);

   -------------------------
   --  Project File Info  --
   -------------------------

   procedure Initialize_Project_Table (Source_Project_Tree : Project_Tree);
   procedure Mark_Projects_With_Stubbed_Sources;

   function Get_Project_Path (Project_Name : String) return String;

   function Get_Project_Stub_Dir (Project_Name : String) return String;

   function Get_Imported_Projects (Project_Name : String)
                                   return List_Of_Strings.List;
   --  Returns a list of directly imported projects' names.

   function Get_Importing_Projects (Project_Name : String)
                                    return List_Of_Strings.List;
   --  Returns a list of names of all projects importing given project.

   function Project_Extended (Project_Name : String) return Boolean;

   function Project_Is_Library (Project_Name : String) return Boolean;

   procedure Enforce_Project_Extention
     (Prj_Name              : String;
      Subroot_Stub_Prj      : String;
      Current_Project_Infix : String);
   procedure Enforce_Custom_Project_Extention
     (File_Name            : String;
      Subroot_Stub_Prj     : String;
      Current_Source_Infix : String);

end Test.Skeleton.Source_Table;
