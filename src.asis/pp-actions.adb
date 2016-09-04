with Text_IO;

with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada;
with Ada.Characters.Handling;
with Interfaces; use type Interfaces.Unsigned_16;
with Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang;     use Libadalang;
with Libadalang.Lexer;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL; use LAL_UL.Common;
with LAL_UL.Dbg_Out;
with LAL_UL.Formatted_Output;
with LAL_UL.String_Utilities; use LAL_UL.String_Utilities;
with LAL_UL.Tool_Names;

with ASIS_UL.Debug; use ASIS_UL.Debug;
with ASIS_UL.Vectors;

pragma Warnings (Off);
with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);

package body Pp.Actions is

   function Image (X : Integer) return String
     renames String_Utilities.Image;

   pragma Warnings (Off);
   procedure Stop (Node : Ada_Node; S : Wide_Wide_String);
   --  For setting breakpoints in gdb

   procedure Stop (Node : Ada_Node; S : Wide_Wide_String) is
      P : constant Ada_Node_Array_Access := Parents (Node);
      use LAL_UL.Dbg_Out;
   begin
      if False then
         Put ("Node:\n");
         Print (Node);
         if False then
            for X in P.Items'Range loop
               Put ("Parent \1:\n", Image (X));
               Print (P.Items (X));
            end loop;
         end if;
      end if;
   end Stop;

   procedure knd (X : Ada_Node);
   procedure pp (X : Ada_Node);
   procedure Put_Child_Record (C : Child_Record);
   procedure Put_Children_Array (A : Children_Arrays.Array_Type);
   --  Debugging printouts

   procedure knd (X : Ada_Node) is
      use LAL_UL.Dbg_Out;
   begin
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure pp (X : Ada_Node) is
      use LAL_UL.Dbg_Out;
   begin
      LAL_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X = null then "null" else Short_Image (X)));
--      Print (X);
   end pp;
   pragma Warnings (On);

   procedure Put_Child_Record (C : Child_Record) is
      use LAL_UL.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", Short_Image (C.Node));
         when Trivia =>
            Put ("Trivia: \1 ""\2"" \3\n",
                 C.Trivia.Kind'Img,
                 To_UTF8 (C.Trivia.Text.all),
                 Slocs.Image (C.Trivia.Sloc_Range));
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Arrays.Array_Type) is
      use LAL_UL.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;

   pragma Warnings (Off); -- ???
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Pp_Flag_Switches, Pp_Boolean_Switches,
     Pp_String_Switches, Pp_String_Seq_Switches;
   pragma Warnings (On);

   use LAL_UL.Formatted_Output;

   Empty_Sym : constant Symbol := Intern ("");

   Empty_CU_Sym : constant CU_Symbol := Intern ("");

   function Output_Dir (Cmd : Command_Line) return String;

   function Q (S : String) return String is -- quote
     ("""" & S & """");

   function Output_Dir (Cmd : Command_Line) return String is
   begin
      if Arg (Cmd, Output_Directory) = null then
         return "";
      else
         return Arg (Cmd, Output_Directory).all;
      end if;
   end Output_Dir;

   ----------
   -- Init --
   ----------

   procedure Init (Tool : in out Pp_Tool; Cmd : Command_Line) is

   --  Start of processing for Init

   begin
      null;
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line) is

   --  Start of processing for Final

   begin
      null;
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Cmd);
   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Pp_Tool) is
      pragma Unreferenced (Tool);
   begin
      pragma Style_Checks ("M200"); -- Allow long lines
      Put ("usage: gnatmetric [options] {filename} {-files filename} [-cargs gcc_switches]\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");
      Put (" -Pproject        - Use project file project. Only one such switch can be used\n");
      Put (" -U               - process all sources of the argument project\n");
      Put (" -U main          - process the closure of units rooted at unit main\n");
      Put (" -Xname=value     - specify an external reference for argument project file\n");
      Put (" --subdirs=dir    - specify subdirectory to place the result files into\n");
      Put (" --no_objects_dir - place results into current dir instead of project dir\n");
      Put (" -eL              - follow all symbolic links when processing project files\n");

      Put ("\n");
      Put (" -a    - process sources from RTL\n");
      if False then -- Disable this for now
         Put (" --incremental -- incremental processing on a per-file basis\n");
      end if;
      Put (" -jn   - n is the maximal number of processes\n");
      Put (" -v    - verbose mode\n");
      Put (" -q    - quiet mode\n");
      Put ("\n");

      Put (" -nolocal - do not compute detailed Pp for local program units\n");
      Put ("\n");

      Put ("Options to control Pp to compute. An option --<metric> turns computing\n");
      Put ("the metric ON, the corresponding --no-<metric> option turns computing the\n");
      Put ("metric OFF. If no metric option is specified, all the Pp are computed\n");
      Put ("and reported. If at least one positive option is  specified, only explicitly\n");
      Put ("selected Pp are computed.\n");
      Put ("\n");

      Put ("Contract Pp:\n");
      Put ("  --contract-all        - all contract Pp\n");
      Put ("  --contract            - subprograms with contracts\n");
      Put ("  --post                - subprograms with postconditions\n");
      Put ("  --contract-complete   - subprograms with complete contracts\n");
      Put ("  --contract-cyclomatic - McCabe Cyclomatic Complexity of contracts\n");
      Put ("\n");

      Put ("Complexity Pp:\n");
      Put ("  --complexity-all        - all complexity Pp\n");
      Put ("  --complexity-cyclomatic - McCabe Cyclomatic Complexity\n");
      Put ("  --complexity-essential  - Essential Complexity\n");
      Put ("  --complexity-average    - average McCabe Cyclomatic Complexity of a body\n");
      Put ("  --loop-nesting          - maximal loop nesting level\n");
      Put ("  --no-static-loop        - do not count static loops for cyclomatic complexity\n");
      Put ("  -ne                     - do not consider exit statements as gotos when\n");
      Put ("                            computing Essential Complexity\n");
      Put ("  --extra-exit-points     - extra exit points in subprograms\n");
      Put ("\n");

      Put ("Line Pp:\n");
      Put ("  --lines-all         - all line Pp\n");
      Put ("  --lines             - number of all lines\n");
      Put ("  --lines-code        - number of code lines\n");
      Put ("  --lines-comment     - number of comment lines\n");
      Put ("  --lines-eol-comment - number of code lines also containing comments\n");
      Put ("  --lines-ratio       - comment/code lines percentage\n");
      Put ("  --lines-blank       - number of blank lines\n");
      Put ("  --lines-average     - average number of code lines in a body\n");
      Put ("\n");

      Put (" Syntax element Pp:\n");
      Put ("  --syntax-all         - all syntax element Pp\n");
      Put ("  --declarations       - total number of declarations\n");
      Put ("  --statements         - total number of statements\n");
      Put ("  --public-subprograms - number of public subprograms in a compilation unit\n");
      Put ("  --all-subprograms    - number of subprograms in a compilation unit\n");
      Put ("  --public-types       - number of public types in a compilation unit\n");
      Put ("  --all-types          - number of types in a compilation unit\n");
      Put ("  --unit-nesting       - maximal unit nesting level\n");
      Put ("  --construct-nesting  - maximal construct nesting level\n");
      Put ("\n");

      Put (" Coupling Pp. By default they are disabled, options below enable all or\n");
      Put (" specific coupling Pp, there is no  option to disable coupling Pp\n");
      Put ("  --coupling-all           - all coupling Pp\n");
      Put ("  --tagged-coupling-out    - tagged (class) fan-out coupling\n");
      Put ("  --tagged-coupling-in     - tagged (class) fan-in coupling\n");
      Put ("  --hierarchy-coupling-out - hierarchy (category) fan-out coupling\n");
      Put ("  --hierarchy-coupling-in  - hierarchy (category) fan-in coupling\n");
      Put ("  --unit-coupling-out      - unit fan-out coupling\n");
      Put ("  --unit-coupling-in       - unit fan-in coupling\n");
      Put ("  --control-coupling-out   - control fan-out coupling\n");
      Put ("  --control-coupling-in    - control fan-in coupling\n");
      Put ("\n");

      Put (" output file control:\n");
      Put ("  -d=dirname     - put files with detailed Pp into 'dirname'\n");
      Put ("  -x             - generate XML output\n");
      Put ("  -xs            - generate XML output and corresponding schema file\n");
      Put ("  -nt            - do not generate output in text form, implies '-x'\n");
      Put ("  -o file-suffix - suffix for the file to put detailed Pp for\n");
      Put ("                   a source file into (file suffix should follow OS\n");
      Put ("                   file name conventions and contain '.' or '$' character)\n");
      Put ("  -og filename   - name of the file to put global Pp info into\n");
      Put ("                   (if not set, this info is sent to Stdout),\n");
      Put ("                   ignored if -nt is used\n");
      Put ("  -ox filename   - name of the file to put XML output into, implies '-x'\n");
      Put ("  -sfn           - use short source file name in output\n");
      Put ("\n");
      Put (" filename        - name of Ada source file for which Pp\n");
      Put ("                   should be computed (wildcards are allowed)\n");
      Put (" -files filename - name of the text file containing a list of Ada\n");
      Put ("                   source files for which Pp should be computed\n");

      Put (" gcc_switches    - switches to be passed to gcc called by \1\n", Tool_Names.Tool_Name);
      pragma Style_Checks ("M79");
   end Tool_Help;

   procedure Dump
     (Tool : in out Pp_Tool;
      Message : String := "")
   is
      pragma Unreferenced (Tool);
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
      end if;
   end Dump;

end Pp.Actions;
