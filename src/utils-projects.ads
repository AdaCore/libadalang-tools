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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Utils.Command_Lines; use Utils.Command_Lines;

package Utils.Projects is

   procedure Process_Command_Line
     (Cmd                             : in out Command_Line;
      Global_Report_Dir               :    out String_Ref;
      The_Project_Tree                :    out not null Project_Tree_Access;
      The_Project_Env                : out not null Project_Environment_Access;
      Preprocessing_Allowed           :        Boolean;
      Tool_Package_Name               :        String;
      Compute_Project_Closure         :        Boolean        := True;
      Callback                        :        Parse_Callback := null;
      Print_Help                      : not null access procedure);
   --  Processes the command line and (if specified on the command line) the
   --  project file.
   --
   --  Global_Report_Dir is set to the directory name in which to place global
   --  tool results, if this information comes from the project file (see
   --  Set_Global_Result_Dirs). Otherwise it is null.
   --
   --  Compiler_Options are options that should be passed to gcc, based on the
   --  content of the project file.
   --
   --  Project_RTS is the value Runtime of the project.
   --
   --  Individual_Source_Options is a mapping from source file names to
   --  switches specified specifically for that source file.
   --
   --  Result_Dirs is a mapping from source file names to file-specific result
   --  directories. Only used if Needs_Per_File_Output is ON.
   --
   --  Tool_Package_Name is the name of the project-file package to use for the
   --  tool.
   --
   --  ????? Use Compiler_Options for more stuff,
   --  where we currently have actions that call Store_GNAT_Option_With_Path
   --  and friends.
   --
   --  Compute_Project_Closure is True if source files should be extracted from
   --  the project if no argument file is specified explicitly. There are at
   --  least two tools that pass False - gnatelim and gnatstub.
   --
   --  Callback is called for each switch, and can be used when some immediate
   --  action is required as soon as the switch is seen.
   --
   --  Tool_Temp_Dir is the name of the directory to which temp files should be
   --  written.
   --
   --  Print_Help is called if --help appears on the command line.

   procedure Read_File_Names_From_File
     (Par_File_Name : String;
      Action        : not null access procedure (File_Name : String));
   --  Read each file name from the named file, and call Action for each
   --  one. This is used to implement the --files and --ignore switches.

   procedure Print_Command_Line (Incremental_Mode, Mimic_gcc : Boolean);
   --  Prints the command line to standard output for debugging

end Utils.Projects;
