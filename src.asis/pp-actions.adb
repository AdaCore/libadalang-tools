pragma Warnings (Off);
--  ????
with Pp.Buffers;
with Pp.Formatting;
with Pp.Formatting.Dictionaries;
with Pp.Formatting.Tree_Formatting;
with Pp.Scanner;

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
with ASIS_UL.Dbg_Out;
with LAL_UL.Formatted_Output;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
with LAL_UL.Tool_Names;

with ASIS_UL.Debug; use ASIS_UL.Debug;
with ASIS_UL.Vectors;

with LAL_UL.Projects;
with LAL_UL.Drivers;
pragma Warnings (On);
with LAL_UL.Symbols; use LAL_UL.Symbols;

package body Pp.Actions is

   function Image (X : Integer) return String
     renames ASIS_UL.String_Utilities.Image;

   pragma Warnings (Off);
   procedure Stop (Node : Ada_Node; S : W_Str);
   --  For setting breakpoints in gdb

   procedure Stop (Node : Ada_Node; S : W_Str) is
      P : constant Ada_Node_Array_Access := Parents (Node);
      use ASIS_UL.Dbg_Out;
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
   procedure ppp (X : Ada_Node);
   procedure Put_Ada_Node_Array (X : Ada_Node_Array);
   procedure Put_Child_Record (C : Child_Record);
   procedure Put_Children_Array (A : Children_Arrays.Array_Type);
   function Par (X : Ada_Node) return Ada_Node is (Parent (X));
   --  Debugging printouts

   procedure knd (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure pp (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X = null then "null" else Short_Image (X)));
   end pp;

   procedure ppp (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      pp (X);
      Print (X);
   end ppp;

   procedure Put_Ada_Node_Array (X : Ada_Node_Array) is
      use ASIS_UL.Dbg_Out;
   begin
      for N of X loop
         pp (N);
         Put ("----------------\n");
      end loop;
   end Put_Ada_Node_Array;
   pragma Warnings (On);

   procedure Put_Child_Record (C : Child_Record) is
      use ASIS_UL.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", Short_Image (C.Node));
         when Trivia =>
            Put ("Trivia: \1 ""\2"" \3\n",
                 C.Trivia.Kind'Img,
                 To_UTF8 (Text_To_W_Str (C.Trivia.Text.all)),
                 Slocs.Image (C.Trivia.Sloc_Range));
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Arrays.Array_Type) is
      use ASIS_UL.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;

   pragma Warnings (Off); -- ???
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Pp_Flag_Switches,
     Pp_Boolean_Switches,
     Attribute_Casing_Switches,
     Keyword_Casing_Switches,
     Name_Casing_Switches,
     Enum_Casing_Switches,
     Type_Casing_Switches,
     Number_Casing_Switches,
     Pragma_Casing_Switches,
     Pp_String_Switches,
     Pp_Nat_Switches;
   --  , ????Pp_String_Seq_Switches;
   pragma Warnings (On);

   use LAL_UL.Formatted_Output;

   Empty_Sym : constant Symbol := Intern ("");
   pragma Unreferenced (Empty_Sym);

   function Output_Dir (Cmd : Command_Line) return String;
   pragma Unreferenced (Output_Dir);

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
      pragma Unreferenced (Tool, Cmd, File_Name);
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

      Put ("usage: gnatpp [options] {filename} {-files filename} " &
            "[-cargs gcc_switches]\n");
      Put (" options:\n");
      Put (" --version - Display version and exit\n");
      Put (" --help    - Display usage and exit\n");
      Put ("\n");
      Put (" -Pproject     - Use project file project. Only one such switch can be used.\n");
      Put (" -U            - process all sources of the argument project\n");
      Put (" -U main       - process the closure of units rooted at unit main\n");
      Put (" -Xname=value  - specify an external reference for argument project file\n");
      Put (" -eL           - follow all symbolic links when processing project files\n");

      Put (" other options (in alphabetic order):\n");

      Put (" -A(0|1) - set alignment\n");
      Put ("   0 - set alignment OFF\n");
      Put ("   1 - set alignment ON (set as default)\n");

      Put (" -a(L|U|M) - set attribute casing\n");
      Put ("   L - lower case\n");
      Put ("   U - upper case\n");
      Put ("   M - mixed case (set as default)\n");

      Put (" --based-grouping=n  - underscores in based literals every n characters\n");

      Put (" -c(0|1|3|4|5) - comments layout\n");
      Put ("   0 - do not format comments\n");
      Put ("   1 - GNAT style comment line indentation (set as default)\n");
      Put ("   3 - GNAT style comment beginning\n");
      Put ("   4 - fill comment blocks\n");
      Put ("   5 - do not change comments with a special character " &
            "just after --\n");
      Put (" --comments-only - format just the comments\n");

      Put (" -clnnn - indentation level for continuation lines, " &
            "nnn from 1 .. 9\n");

      Put (" -D<file> - set <file> as the dictionary file defining casing " &
            "exceptions\n");
      Put (" -D-      - do not use RM-defined casing for predefined " &
            "names, use casing \n");
      Put ("            defined by -n parameter and dictionary file(s) " &
            "instead\n");

      Put (" --decimal-grouping=n  - underscores in decimal literals every n characters\n");

      Put (" -ff - put Form Feed after a pragma Page\n");
      Put (" -gnatec<path> - the same as GNAT -gnatec option\n");
      Put (" -innn - indentation level, nnn from 1 .. 9, " &
            "the default value is 3\n");

      Put (" -I<dir> - the same as gcc -I option\n");

      Put (" -I-     - the same as gcc -I- option\n");

      Put (" -k(L|U) - set keyword casing\n");
      Put ("   L - lower case (set as default)\n");
      Put ("   U - upper case\n");

      Put (" -Mnnn - set maximum line length, nnn from 32 .. 256, " &
            "the default value is 79\n");

      Put (" -n(D|U|L|M) - set name casing (for both defining and usage " &
            "occurrences)\n");
      Put ("   D - as declared (set as default)\n");
      Put ("   U - all in upper case\n");
      Put ("   L - all in lower case\n");
      Put ("   M - mixed\n");

      Put (" -ne(D|U|L|M) - set enumeration literal casing (for both defining and usage\n");
      Put ("                occurrences), parameters have the same meaning as for -n option\n");
      Put ("                if not set, -n is used to define enumeration literal casing\n");

      Put (" -nt(D|U|L|M) - set casing for names introduced by type and subtype\n");
      Put ("                declarations (both defining and usage occurrences), parameters\n");
      Put ("                have the same meaning as for -n option. If not set, -n is used\n");

      Put (" -nn(D|U|L|M) - set casing for names introduced by number declarations (both\n");
      Put ("                (defining and usage occurrences), parameters have the same\n");
      Put ("                meaning as for -n option. If not set, -n is used\n");

      Put (" -N - no tabulation in comments\n");

      Put (" -p(L|U|M) - set pragma casing\n");
      Put ("   L - lower case\n");
      Put ("   U - upper case\n");
      Put ("   M - mixed case (set as default)\n");

      Put (" --pp-off=xxx - Use ""--xxx"" as the comment string to disable\n");
      Put ("                pretty printing instead of the default " &
              """--!pp off""\n");
      Put (" --pp-on=xxx - Use ""--xxx"" as the comment string to reenable\n");
      Put ("                pretty printing instead of the default " &
              """--!pp on""\n");

      Put (" --RTS=<dir> - the same as gcc --RTS option\n");

      Put (" -q  - quiet mode\n");

      Put (" --no-separate-is        - try not to place 'IS' on a separate " &
            " line in\n");
      Put ("                           a subprogram body\n");
      Put (" --separate-loop-then    - use a separate line for LOOP and " &
            "THEN keywords\n");

      Put (" --no-separate-loop-then - do not use a separate line for LOOP " &
            "and THEN\n");
      Put ("                           keywords, uncompatible with " &
            "--separate-loop-then\n");

      Put (" --use-on-new-line       - use separate lines for USE clauses \n");
      Put ("                           in a context clause\n");

      Put (" --insert-blank-lines    - insert blank lines where appropriate\n");

      Put (" --preserve-blank-lines  - preserve blank lines in the input\n");

      Put (" --split-line-before-op  - operator on next line\n");

      Put (" --RM-style-spacing      - no extra space before " &
            "'(' and ':'\n");

      Put (" --par_threshold=nnn     - if the number of parameter specifications is greater\n");
      Put ("                           than nnn, each specification starts from a new line\n");

      Put (" --call_threshold=nnn    - if the number of parameter associations in a call is\n");
      Put ("                           greater than nnn and there is at least one named\n");
      Put ("                           association, each association starts from a new line\n");

      Put (" --incremental -- incremental processing on a per-file basis\n");
      Put (" -jn - n is the maximal number of processes to carry out\n");
      Put (" -t  - display execution time\n");

      Put (" -v  - verbose mode\n");

      Put (" -dd - progress indicator verbose mode\n");
      Put ("\n");

      Put ("Output file control:\n");
      Put (" -pipe - send the output into Stdout\n");
      Put (" -o output_file - write the output into output_file. Give up " &
            "if output_file\n");
      Put ("                  already exists\n");
      Put (" -of output_file - write the output into output_file, " &
            "overriding the existing \n");
      Put ("                   file\n");
      Put (" --output-dir=dir -- create output files in dir\n");
      Put (" -r   - replace the argument source with the pretty-printed" &
            " source and copy the\n");
      Put ("        argument source into filename.npp" &
            ". Give up if filename.npp\n");
      Put ("        already exists\n");
      Put (" -rf  - replace the argument source with the pretty-printed " &
            "source and copy the\n");
      Put ("        argument source into filename.npp" &
            ", overriding the existing file\n");

      Put (" -rnb - replace the argument source with the pretty-printed " &
            "source and do not\n");
      Put ("        create the back-up copy of the argument source\n");
      Put ("\n");

      Put (" filename - the name of the Ada source file to be reformatted. \n");
      Put ("            Wildcards are allowed\n");
      Put (" -files=filename - the name of a text file containing a list\n");
      Put ("                   of Ada source files to reformat\n");
      Put (" --eol=text_format - sets the format of the gnatpp output " &
        "file(s),\n");
      Put ("                    can not be used together with -pipe option\n");
      Put ("       text_format can be - 'unix' or 'lf'   - lines end with " &
        "LF character\n");
      Put ("                          - 'dos'  or 'crlf' - lines end with " &
        "CRLF characters\n");

      Put (" -W(h|u|s|e|8|b) - sets the wide character encoding of the " &
        "result file\n");
      Put ("    h - Hex ESC encoding\n");
      Put ("    u - Upper half encoding\n");
      Put ("    s - Shift-JIS encoding\n");
      Put ("    e - EUC Encoding\n");
      Put ("    8 - UTF-8 encoding\n");
      Put ("    b - Brackets encoding (this is the default)\n");
      Put ("\n");

      Put (" gcc_switches - switches to be passed to gcc called by \1\n",
            Tool_Names.Tool_Name);

      Put ("\n\nReport bugs to report@adacore.com\n");

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
