with ASIS_UL.Debug; use ASIS_UL.Debug;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines;
with LAL_UL.Drivers;
with LAL_UL.Formatted_Output;
with LAL_UL.Tool_Names; use LAL_UL.Tool_Names;

with METRICS.Actions;
with METRICS.Command_Lines;

procedure METRICS.Main is

   --  Prototype contract-coverage metrics tool.

   --  Command line:
   --  contract_coverage [--verbose] source-files...

   --  Prints out the percentage of subprograms in a package visible part that
   --  have contracts (i.e. Pre, Post, or Contract_Cases aspects).

   --  Library subprograms are ignored. We process only subprograms in the
   --  visible part of a package or generic package. Package bodies and so on
   --  are silently ignored. Pragmas Precondition, Postcondition, and
   --  Contract_Cases are ignored -- only aspects count.

   --  See OB04-010 for requirements.

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Print_Help;
   --  Prints out the brief help

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
      use LAL_UL.Formatted_Output;
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

      Put (" -nolocal - do not compute detailed metrics for local program units\n");
      Put ("\n");

      Put ("Options to control metrics to compute. An option --<metric> turns computing\n");
      Put ("the metric ON, the corresponding --no-<metric> option turns computing the\n");
      Put ("metric OFF. If no metric option is specified, all the metrics are computed\n");
      Put ("and reported. If at least one positive option is  specified, only explicitly\n");
      Put ("selected metrics are computed.\n");
      Put ("\n");

      Put ("Contract metrics:\n");
      Put ("  --contract-all        - all contract metrics\n");
      Put ("  --contract            - subprograms with contracts\n");
      Put ("  --post                - subprograms with postconditions\n");
      Put ("  --contract-complete   - subprograms with complete contracts\n");
      Put ("  --contract-cyclomatic - McCabe Cyclomatic Complexity of contracts\n");
      Put ("\n");

      Put ("Complexity metrics:\n");
      Put ("  --complexity-all        - all complexity metrics\n");
      Put ("  --complexity-cyclomatic - McCabe Cyclomatic Complexity\n");
      Put ("  --complexity-essential  - Essential Complexity\n");
      Put ("  --complexity-average    - average McCabe Cyclomatic Complexity of a body\n");
      Put ("  --loop-nesting          - maximal loop nesting level\n");
      Put ("  --no-static-loop        - do not count static loops for cyclomatic complexity\n");
      Put ("  -ne                     - do not consider exit statements as gotos when\n");
      Put ("                            computing Essential Complexity\n");
      Put ("  --extra-exit-points     - extra exit points in subprograms\n");
      Put ("\n");

      Put ("Line metrics:\n");
      Put ("  --lines-all         - all line metrics\n");
      Put ("  --lines             - number of all lines\n");
      Put ("  --lines-code        - number of code lines\n");
      Put ("  --lines-comment     - number of comment lines\n");
      Put ("  --lines-eol-comment - number of code lines also containing comments\n");
      Put ("  --lines-ratio       - comment/code lines percentage\n");
      Put ("  --lines-blank       - number of blank lines\n");
      Put ("  --lines-average     - average number of code lines in a body\n");
      Put ("\n");

      Put (" Syntax element metrics:\n");
      Put ("  --syntax-all         - all syntax element metrics\n");
      Put ("  --declarations       - total number of declarations\n");
      Put ("  --statements         - total number of statements\n");
      Put ("  --public-subprograms - number of public subprograms in a compilation unit\n");
      Put ("  --all-subprograms    - number of subprograms in a compilation unit\n");
      Put ("  --public-types       - number of public types in a compilation unit\n");
      Put ("  --all-types          - number of types in a compilation unit\n");
      Put ("  --unit-nesting       - maximal unit nesting level\n");
      Put ("  --construct-nesting  - maximal construct nesting level\n");
      Put ("\n");

      Put (" Coupling metrics. By default they are disabled, options below enable all or\n");
      Put (" specific coupling metrics, there is no  option to disable coupling metrics\n");
      Put ("  --coupling-all           - all coupling metrics\n");
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
      Put ("  -d=dirname     - put files with detailed metrics into 'dirname'\n");
      Put ("  -x             - generate XML output\n");
      Put ("  -xs            - generate XML output and corresponding schema file\n");
      Put ("  -nt            - do not generate output in text form, implies '-x'\n");
      Put ("  -o file-suffix - suffix for the file to put detailed metrics for\n");
      Put ("                   a source file into (file suffix should follow OS\n");
      Put ("                   file name conventions and contain '.' or '$' character)\n");
      Put ("  -og filename   - name of the file to put global metrics info into\n");
      Put ("                   (if not set, this info is sent to Stdout),\n");
      Put ("                   ignored if -nt is used\n");
      Put ("  -ox filename   - name of the file to put XML output into, implies '-x'\n");
      Put ("  -sfn           - use short source file name in output\n");
      Put ("\n");
      Put (" filename        - name of Ada source file for which metrics\n");
      Put ("                   should be computed (wildcards are allowed)\n");
      Put (" -files filename - name of the text file containing a list of Ada\n");
      Put ("                   source files for which metrics should be computed\n");

      Put (" gcc_switches    - switches to be passed to gcc called by \1\n", Tool_Name);

      pragma Style_Checks ("M79");
   end Print_Help;

   procedure Callback (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is
      use METRICS.Command_Lines;
      use Metrics_String_Seq_Switches;
      use String_Ref_Vectors;
   begin
      if Phase = Cmd_Line_1 then

         if Metrics_String_Seq_Switches.Valid (Swit.Switch) then
            declare
               Switch : constant Metrics_String_Seqs := From_All (Swit.Switch);
            begin
               case Switch is
                  when Gnatmetric_Debug =>
                     Set_Debug_Options
                       (Swit.Seq_Val (Last_Index (Swit.Seq_Val)).all);
               end case;
            end;
         end if;

         return;
      end if;
   end Callback;

begin
   LAL_UL.Drivers.Driver
     (METRICS.Command_Lines.Cmd,
      Tool_Package_Name     => "metrics",
      Needs_Per_File_Output => True,
      Callback              => Callback'Unrestricted_Access,
      Per_File_Tool_Action  => METRICS.Actions.Per_File_Action'Access,
      Print_Help            => Print_Help'Access);
end METRICS.Main;
