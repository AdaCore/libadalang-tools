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

with Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
with Utils.String_Utilities; use Utils.String_Utilities;
with Utils.Tool_Names;

package body Utils.Projects.Aggregate is

   use Common_Flag_Switches, Common_String_Switches;

   use String_Access_Sets;
   Aggregated_Projects : String_Access_Set;
   --  The set of aggregated projects that are part of the current aggregate
   --  project. Set by Collect_Aggregated_Projects

   ---------------------------------
   -- Collect_Aggregated_Projects --
   ---------------------------------

   procedure Collect_Aggregated_Projects (P : Project_Type) is
      Aggregated_Prjs : Project_Array_Access :=
        P.Aggregated_Projects (Unwind_Aggregated => True);

      Arg_Prj_Name : constant Filesystem_String :=
        Full_Name (P.Project_Path, Normalize => True);
   begin
      if Debug_Flag_A then
         Ada.Text_IO.Put_Line (String (Arg_Prj_Name));
      end if;

      for Prj of Aggregated_Prjs.all loop
         declare
            VF : constant Virtual_File := Prj.Project_Path;
            pragma Assert (VF /= No_File);
            Prj_Name : constant Filesystem_String :=
              Full_Name (VF, Normalize => True);
            Prj_String : constant String_Access :=
              new String'(String (Prj_Name));
         begin
            Include (Aggregated_Projects, Prj_String);
         end;
      end loop;

      Unchecked_Free (Aggregated_Prjs);

      if Num_Of_Aggregated_Projects = 0 then
         Cmd_Error
           ("aggregate project does not contain anything to process");
      end if;
   end Collect_Aggregated_Projects;

   ----------------------------
   -- Get_Aggregated_Prj_Src --
   ----------------------------

   function Get_Aggregated_Prj_Src return String_Access is
     (Element (First (Aggregated_Projects)));

   --------------------------------
   -- Num_Of_Aggregated_Projects --
   --------------------------------

   function Num_Of_Aggregated_Projects return Natural is
     (Natural (Aggregated_Projects.Length));

   ---------------------------------
   -- Process_Aggregated_Projects --
   ---------------------------------

   Aggregated_Project_File_Switch : aliased String :=
     Switch_Text (Common_Descriptor, To_All (Aggregated_Project_File)).all;

   procedure Process_Aggregated_Projects
     (Cmd : Command_Line; Tool_Package_Name : String)
   is
      Args_Vec : String_Access_Vector;
      use String_Access_Vectors;
   begin
      Append_Text_Args_From_Command_Line (Tool_Package_Name, Args_Vec);
      Append (Args_Vec, Aggregated_Project_File_Switch'Access);
      Append (Args_Vec, null); -- will be replaced with project file name below

      declare
         L : constant Positive := Last_Index (Args_Vec);
         Args : GNAT.OS_Lib.String_List renames Elems_Var (Args_Vec)(1 .. L);
         Prj_Name_Arg : String_Access renames Args (L);
      begin
         for Prj_Name of Aggregated_Projects loop
            if Arg (Cmd, Verbose) then
               Ada.Text_IO.Put_Line ("Processing aggregated project " &
                     Prj_Name.all);
            end if;

            Prj_Name_Arg := Prj_Name;

            if Debug_Flag_C then
               Print_Command_Line
                 (Incremental_Mode => False, Mimic_gcc => False);
            end if;

            declare
               Exit_Code : constant Integer :=
                 Spawn (Tool_Names.Full_Tool_Name, Args);
            begin
               --  If the subprocess failed, then we fail. We could instead
               --  keep going, and collect the exit codes of all subprocesses,
               --  and print something at the end if some failed.

               if Exit_Code /= 0 then
                  OS_Exit (Exit_Code);
               end if;
            end;
         end loop;
      end;
   end Process_Aggregated_Projects;

end Utils.Projects.Aggregate;
