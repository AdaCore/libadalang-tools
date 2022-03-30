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

with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Ada.Text_IO;

package body TGen.Files is

   function Get_Tmpl_Directory return Virtual_File is
   begin
      if not Ada.Environment_Variables.Exists ("TGEN_TEMPLATE_PATH") then
         raise Program_Error
           with "Set the TGEN_TEMPLATE_PATH environment variable to point to"
           & " tgen templates directory.";
      end if;
      return GNATCOLL.VFS.Create
        (Filesystem_String
           (Ada.Environment_Variables.Value ("TGEN_TEMPLATE_PATH")));
   end Get_Tmpl_Directory;

   -------------------------
   -- Prepare_Output_Dirs --
   -------------------------

   procedure Prepare_Output_Dirs (Context : Generation_Context) is
      Output_Dir : constant String :=
        +(Context.Output_Dir);
   begin
      if not Ada.Directories.Exists (Output_Dir)
      then
         Ada.Text_IO.Put_Line ("Creating " & Output_Dir);
         Ada.Directories.Create_Path (Output_Dir);
      end if;
   end Prepare_Output_Dirs;

   ------------------------
   -- Project_Output_Dir --
   ------------------------

   function Project_Output_Dir (Project : Project_Type) return String is
      Obj_Dir : constant String := +Project.Object_Dir.Full_Name;
   begin
      if Obj_Dir'Length = 0 then
         return "";
      else
         declare
            Prj_Name : constant String :=
               Ada.Characters.Handling.To_Lower (Project.Name);
         begin
            return Obj_Dir / Prj_Name & "-tgen";
         end;
      end if;
   end Project_Output_Dir;

end TGen.Files;
