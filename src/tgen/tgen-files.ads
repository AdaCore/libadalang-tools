------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Directories;
with Ada.Environment_Variables;

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

package TGen.Files is

   Prj_Tree : Project_Tree_Access;

   Strat_ADB        : constant Filesystem_String :=
     "strat.adb";

   Strat_Template_ADB : constant Filesystem_String :=
     "strat.adb.tmpl";

   Type_Strat_Template_ADB : constant Filesystem_String :=
     "type-strat.adb.tmpl";

   Test_Proc_Template_ADB : constant Filesystem_String :=
     "test_procedure.adb.tmpl";

   function Get_Tmpl_Directory return Virtual_File;

   function Get_Template_Strat_ADB return Virtual_File is
     (Get_Tmpl_Directory / "strat.adb.tmpl");

   function Get_Template_Test_ADB return Virtual_File is
      (Get_Tmpl_Directory / Test_Proc_Template_ADB);

   function Get_Template_Type_Strat_ADB return Virtual_File is
     (Get_Tmpl_Directory / Type_Strat_Template_ADB);

   function "/" (Dir, Name : String) return String is
     (Ada.Directories.Compose (Dir, Name));
   --  Likewise, without the "dir shouldn't be empty" constraint but
   --  checking that the path components are valid when not empty.

end TGen.Files;
