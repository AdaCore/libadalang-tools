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
--  This unit provides file managment utilities

with Ada.Directories;

with TGen.Context;   use TGen.Context;
with TGen.Strings;   use TGen.Strings;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package TGen.Files is

   function Get_Output_Dir
     (Context : Generation_Context) return Virtual_File
   is (GNATCOLL.VFS.Create (Filesystem_String (+Context.Output_Dir)));

   function Get_JSON_Name
     (Context   : Generation_Context;
      Unit_Name : String) return Virtual_File
     is (Get_Output_Dir (Context) / Filesystem_String (Unit_Name & ".json"));
   --  Return the name of the JSON file that will hold the generation results
   --  for unit Unit_Name.

   function "/" (Dir, Name : String) return String is
     (Ada.Directories.Compose (Dir, Name));
   --  Likewise, without the "dir shouldn't be empty" constraint but
   --  checking that the path components are valid when not empty.

   procedure Prepare_Output_Dirs (Context : Generation_Context);
   --  Create directories for the output if needed

   function Project_Output_Dir (Project : Project_Type) return String;
   --  Return the path to the output dir of Project

   function Gen_File
     (Ctx : Generation_Context; File : String) return Virtual_File is
     (Get_Output_Dir (Ctx) / Filesystem_String (File));

end TGen.Files;
