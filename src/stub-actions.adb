with Ada.Characters.Handling; use Ada.Characters.Handling;
pragma Unreferenced (Ada.Characters.Handling); -- ???
with Ada.Strings.Fixed;
pragma Unreferenced (Ada.Strings.Fixed); -- ???
with System.WCh_Con;
with Text_IO, Ada.Wide_Text_IO;
pragma Unreferenced (Text_IO); -- ???
pragma Unreferenced (Ada.Wide_Text_IO); -- ???
with Stub.Command_Lines; use Stub.Command_Lines;

with Ada.Directories; use Ada.Directories;
with Interfaces; use type Interfaces.Unsigned_16;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Langkit_Support.Slocs; use Langkit_Support;
with Libadalang;     use Libadalang;
with LAL_Extensions; use LAL_Extensions;

with LAL_UL.Common; use LAL_UL.Common;
with ASIS_UL.Dbg_Out;
with LAL_UL.Formatted_Output;
with LAL_UL.Tool_Names;
with ASIS_UL.Char_Vectors; use ASIS_UL.Char_Vectors;
use ASIS_UL.Char_Vectors.Char_Vectors;
pragma Warnings (Off); -- ???
with ASIS_UL.Generic_Formatted_Output;

with ASIS_UL.Debug; use ASIS_UL.Debug;
with ASIS_UL.Vectors;

with LAL_UL.Environment;

package body Stub.Actions is

   package Slocs renames Langkit_Support.Slocs;

   function Image (X : Integer) return String
     renames ASIS_UL.String_Utilities.Image;

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   use Stub_Flag_Switches,
     Stub_String_Switches,
     Stub_Nat_Switches;
pragma Warnings (On);

   ----------
   -- Init --
   ----------

   procedure Init (Tool : in out Stub_Tool; Cmd : Command_Line) is
      pragma Unreferenced (Tool, Cmd);
   begin
      --  ????Other checks from gnatstub/lal_ul-check_parameters.adb?

      pragma Assert (Environment.Initial_Dir = Current_Directory);
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Stub_Tool; Cmd : Command_Line) is
   begin
      null;
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   --  Debugging printouts:
   --  See also Libadalang.Debug.
   pragma Warnings (Off);
   pragma Style_Checks (Off);
   procedure knd (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Kind (X)'Img);
   end knd;

   procedure psloc (X : Ada_Node) is

      function Lines_String
        (Sloc_Range : Slocs.Source_Location_Range) return String is
         (Image (Integer (Sloc_Range.Start_Line)) & ": " &
          Image (Integer (Sloc_Range.End_Line)));

      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", Lines_String (Sloc_Range (X)));
   end psloc;

   procedure nn (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      ASIS_UL.Dbg_Out.Output_Enabled := True;
      Put ("\1\n", (if X = null then "null" else Short_Image (X)));
   end nn;

   procedure ppp (X : Ada_Node) is
      use ASIS_UL.Dbg_Out;
   begin
      nn (X);
      Print (X);
   end ppp;

   procedure Put_Ada_Node_Array (X : Ada_Node_Array) is
      use ASIS_UL.Dbg_Out;
   begin
      for N of X loop
         nn (N);
         Put ("----------------\n");
      end loop;
   end Put_Ada_Node_Array;

   procedure Put_Child_Record (C : Child_Record) is
      use ASIS_UL.Dbg_Out;
   begin
      case C.Kind is
         when Child =>
            Put ("Child: \1\n", Short_Image (C.Node));
         when Trivia =>
            declare
               Trivia_Data : constant Token_Data_Type := Data (C.Trivia);
            begin
               Put ("Trivia: \1 ""\2"" \3\n",
                    Kind (Trivia_Data)'Img,
                    To_UTF8 (Text_To_W_Str (Text (C.Trivia))),
                    Slocs.Image (Sloc_Range (Trivia_Data)));
            end;
      end case;
   end Put_Child_Record;

   procedure Put_Children_Array (A : Children_Array) is
      use ASIS_UL.Dbg_Out;
   begin
      for I in A'Range loop
         Put ("\1: ", Image (I));
         Put_Child_Record (A (I));
      end loop;
   end Put_Children_Array;

   procedure Dump
     (Tool : in out Stub_Tool;
      Message : String := "")
   is
      pragma Unreferenced (Tool);
      use LAL_UL.Formatted_Output;
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
      end if;
   end Dump;
   pragma Style_Checks (On);
   pragma Warnings (On);

   procedure Per_File_Action
     (Tool : in out Stub_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Tool, File_Name, Input, BOM_Seen);

      use LAL_UL.Formatted_Output;

      Root_Node : constant Ada_Node := Root (Unit);
      pragma Unreferenced (Root_Node);

      WCEM : constant String := Arg (Cmd, Wide_Character_Encoding).all;
      Encoding_Method : constant System.WCh_Con.WC_Encoding_Method :=
        (if WCEM = "h" then
           System.WCh_Con.WCEM_Hex
         elsif WCEM = "u" then
           System.WCh_Con.WCEM_Upper
         elsif WCEM = "s" then
           System.WCh_Con.WCEM_Shift_JIS
         elsif WCEM = "e" then
           System.WCh_Con.WCEM_EUC
         elsif WCEM = "8" then
           System.WCh_Con.WCEM_UTF8
         elsif WCEM = "b" then
           System.WCh_Con.WCEM_Brackets
         else raise Program_Error);
      pragma Unreferenced (Encoding_Method);

      Form_String : constant String := "WCEM=" & WCEM;
      pragma Unreferenced (Form_String);
      --  ????????????????Duplicates pp

      function Get_Output_Name (Resolve_Links : Boolean) return String;
      function Get_Output_Name (Resolve_Links : Boolean) return String is
         pragma Unreferenced (Resolve_Links);
      begin
         return "foo.adb"; -- ????????????????
      end Get_Output_Name;

      Output_Name : constant String := Get_Output_Name (Resolve_Links => True);
      pragma Unreferenced (Output_Name);

   --  Start of processing for Per_File_Action

   begin
      if Debug_Mode then
         Print (Unit);
--         Put ("With trivia\n");
--         PP_Trivia (Unit);
      end if;
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Stub_Tool) is
      pragma Unreferenced (Tool);
      use LAL_UL.Formatted_Output;
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      Put ("Usage: gnatstub [options] filename [-cargs gcc_switches]\n");
      Put ("\n");
      Put ("  filename               Ada source file\n");
      Put ("\n");
      Put ("options:\n");
      Put ("  --version              Display version and exit\n");
      Put ("  --help                 Display usage and exit\n");
      Put ("\n");
      Put ("  -Pproject              Use project file project. Only one such switch.\n");
      Put ("                         can be used.\n");
      Put ("  -Xname=value           specify an external reference for argument\n");
      Put ("                         project file\n");
      Put ("  -eL                    follow all symbolic links when processing\n");
      Put ("                         project files\n");
      Put ("\n");
      Put (" --subunits              generate separate bodies for body stubs\n");
      Put ("\n");
      Put ("  -f                     replace an existing body file (if any), with a body\n");
      Put ("                         sample (is not allowed with '--subunits')\n");
      Put ("  -gnatec<path>          use additional configuration file,\n");
      Put ("                         same meaning as for gcc\n");
      Put ("  -gnatyMnnn             maximum line length in a sample body\n");
      Put ("  -gnatyn                (n in 1 .. 9) number of spaces used for indentation in\n");
      Put ("                         a sample body\n");
      Put ("  -gnatyo                alphabetically order local bodies\n");
      Put ("  -hg                    insert a sample comment header\n");
      Put ("  -hs                    insert the comment header from the spec\n");
      Put ("  --header-file=filename insert the comment header from the specified file\n");
      Put ("  -Idir                  source search dir, has the same meaning as for\n");
      Put ("                         gcc and gnatmake\n");
      Put ("  -I-                    do not look for the sources in the default directory\n");
      Put ("  -in                    same as -gnatyn\n");
      Put ("  -k                     do not remove the tree file\n");
      Put ("  -lnnn                  same as -gnatyMnnn\n");
      Put ("  --no-exception         avoid raising Program_Error in stubs\n");
      Put ("  --no-local-header      no local comment headers for unit stubs\n");
      Put ("  -o body-name           the name of the file to place the body into.\n");
      Put ("  --dir=directory        place generated file(s) into directory\n");
      Put ("  -W(h|u|s|e|8|b)        sets the wide character encoding of the result file\n");
      Put ("                          h - Hex ESC encoding\n");
      Put ("                          u - Upper half encoding\n");
      Put ("                          s - Shift-JIS encoding\n");
      Put ("                          e - EUC Encoding\n");
      Put ("                          8 - UTF-8 encoding\n");
      Put ("                          b - Brackets encoding (this is the default)\n");
      Put ("\n");
      Put ("  -q                     quiet mode\n");
      Put ("  -r                     reuse the tree file (if any) instead of creating it\n");
      Put ("                         (-r also implies -k)\n");
      Put ("  -t                     overwrite the existing tree file\n");
      Put ("  -v                     verbose mode\n");
      Put ("  gcc_switches           switches to be passed to gcc called by \1\n",
           Tool_Names.Tool_Name); -- ???
      Put ("\n");
      Put ("\n");
      Put ("Report bugs to report@adacore.com\n");

      pragma Style_Checks ("M79");
   end Tool_Help;

end Stub.Actions;
