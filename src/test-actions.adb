with Ada.Containers; use type Ada.Containers.Count_Type;
with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.VFS;
with GNATCOLL.Projects;

with Libadalang;     use Libadalang;
with LAL_Extensions; use LAL_Extensions;

with Utils.Command_Lines.Common; use Utils; use Utils.Command_Lines.Common;
pragma Unreferenced (Utils.Command_Lines.Common); -- ????
with Utils.Formatted_Output;

with Utils_Debug; use Utils_Debug;

package body Test.Actions is

   use Utils.Formatted_Output;

   ----------
   -- Init --
   ----------

   procedure Init
     (Tool : in out Test_Tool; Cmd : in out Command_Line)
   is
   begin
      null; -- ????
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Test_Tool; Cmd : Command_Line) is
   begin
      null; -- ????
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Test_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Tool, Cmd, File_Name, Input, BOM_Seen); -- ????
   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;
      null; -- ????
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Test_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("usage: gnattest [options] {filename}\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");

      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - process all sources of the argument project\n");
      Put (" -U main          - process the closure of units rooted at unit main\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" --subdirs=dir    - specify subdirectory to place the result files into\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");
      Put ("\n");

      Put (" --verbose    - verbose mode\n");
      Put (" --quiet      - quiet mode\n");
      Put ("\n");

      Put (" --etc ????\n");
      Put ("\n");
      pragma Style_Checks ("M79");
   end Tool_Help;

end Test.Actions;
