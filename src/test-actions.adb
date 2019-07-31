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

with Test.Command_Lines; use Test.Command_Lines;

package body Test.Actions is

   use Utils.Formatted_Output;

   pragma Warnings (Off); -- ????
   --  These use clauses will be necessary later.
   --  At least some of them.

   use Common_Flag_Switches,
       Common_Boolean_Switches,
       Ada_Version_Switches,
       Common_String_Switches,
       Common_String_Seq_Switches,
       Common_Nat_Switches;

   use Test_Boolean_Switches, Test_String_Switches, Test_String_Seq_Switches;
   pragma Warnings (On);

   ----------
   -- Init --
   ----------

   procedure Init
     (Tool : in out Test_Tool; Cmd : in out Command_Line)
   is
      pragma Unreferenced (Tool); -- ????
   begin
      null; -- ????

      --  Here's some code to illustrate how to query the args. The parsed
      --  comand line (including stuff from project files) is passed as Cmd.
      --  Call "Arg (Cmd, <switch_name>)" to get the value specified for the
      --  switch.

      --  First, a Boolean one: "Arg (Cmd, Strict)" will return True if
      --  --strict appears on the command line or in a project file. It
      --  defaults to False (see Test.Command_Line).

      --  You have to "use" all the "..._Switches" packages. Otherwise, you get
      --  incomprehensible error messages about "Arg" not resolving. "Arg" is
      --  heavily overloaded. If a "use" is missing, the compiler doesn't
      --  say "use Test_Boolean_Switches is missing". It says, "Here's 37
      --  different Arg functions with the wrong parameter types."

      if Arg (Cmd, Strict) then
         Put ("--strict\n");
      else
         Put ("--no-strict\n");
      end if;

      --  --harness-dir is a string switch, so Arg returns null or a pointer to
      --  the specified string.

      if Arg (Cmd, Harness_Dir) = null then
         Put ("--harness-dir not specified\n");
      else
         Put ("--harness-dir = \1\n", Arg (Cmd, Harness_Dir).all);
      end if;

      --  --additional-tests is a string sequence, so it can be given multiple
      --  times on the command line, and Arg returns an array of pointers to
      --  strings.

      declare
         Additional : constant String_Ref_Array := Arg (Cmd, Additional_Tests);
      begin
         Put ("Additional_Tests:\n");
         for X of Additional loop
            Put ("    \1\n", X.all);
         end loop;
         Put ("end Additional_Tests\n");
      end;
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
      pragma Unreferenced (Tool, Input, BOM_Seen); -- ????
   begin
      Put ("gnattest is processing \1.\n", File_Name);
      Dump_Cmd (Cmd);

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
