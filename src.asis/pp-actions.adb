pragma Warnings (Off);
--  ????
with Ada.Finalization;
with System.WCh_Con;
with Text_IO, Ada.Wide_Text_IO; use Ada;
with Pp.Buffers; use Pp.Buffers;
with Pp.Formatting; use Pp.Formatting;
with Pp.Formatting.Dictionaries;
with Pp.Formatting.Tree_Formatting;
with Pp.Scanner;

with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada;
with Ada.Characters.Handling;
with Interfaces; use type Interfaces.Unsigned_16;
with Unchecked_Deallocation;

with GNAT.Lock_Files;
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
with ASIS_UL.Char_Vectors; use ASIS_UL.Char_Vectors;
use ASIS_UL.Char_Vectors.Char_Vectors;

with ASIS_UL.Debug; use ASIS_UL.Debug;
with ASIS_UL.Vectors;

with LAL_UL.Projects;
with LAL_UL.Drivers;
with LAL_UL.Symbols; use LAL_UL.Symbols;
with LAL_UL.Environment;
pragma Warnings (On);

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
     Pp_Nat_Switches,
     Pp_String_Seq_Switches;
   pragma Warnings (On);

   File_Name_File_Name : String_Access;
   --  There is a "file name file"; this is its name. ASIS_Processing writes
   --  the output to a temp file, and Finalize moves the temp file to the
   --  actual output file. The file name file is used to pass the names of the
   --  temp and output files from ASIS_Processing to Finalize (both subunits of
   --  ASIS_UL.Source_Table.Processing).
   --
   --  ASIS_Processing is called once for each file, and it writes two lines to
   --  the file name file: the name of the temp file, and then the name of the
   --  output file. Finalize reads pairs of lines from the file name file, and
   --  moves temp --> output.
   --
   --  The reason for passing information via a file is that in
   --  Incremental_Mode, ASIS_Processing and Finalize are running in two
   --  different processes; the inner processes do ASIS_Processing, and need
   --  to pass those file names back to the outer process. The builder is in
   --  between inner and outer, and doesn't know how to cooperate in this
   --  dance.
   --
   --  The reason for doing all the renames at the end (after all
   --  ASIS_Processing is done) is again Incremental_Mode, specifically
   --  Replace_Modes. We don't want to replace the original input with the
   --  output during ASIS_Processing, because that would change timestamps and
   --  confuse the builder.
   --
   --  In Incremental_Mode, the File_Name_File_Name is constructed in the outer
   --  invocation (in Initialize), and passed down to the inner invocations via
   --  the command-line switch --file-name-file=. --file-name-file is not
   --  documented for users; it is for internal use only. In other modes, it is
   --  constructed in Initialize.
   --
   --  We use the file name file even in non-Incremental_Mode, even though it's
   --  not really necessary, just for uniformity/simplicity.
   --
   --  In Replace_Modes, we optimize by not overwriting the output (i.e. the
   --  input) if it didn't change. This is especially important in
   --  Incremental_Mode, because of the way the builder works: it will invoke
   --  gnatpp (in Mimic_gcc mode) on something.adb, which will pretty-print
   --  something.ads. If something.ads didn't need pretty-printing, we don't
   --  want to change its timestamp, causing real (code-generating) builds to
   --  do unnecessary recompiles.

   function Mimic_gcc (Cmd : Command_Line) return Boolean is
      (Arg (Cmd, Outer_Dir) /= null);

   pragma Style_Checks ("M85");

   ----------
   -- Init --
   ----------

   procedure Init (Tool : in out Pp_Tool; Cmd : Command_Line) is
      pragma Unreferenced (Tool);
      File_Name_File : Text_IO.File_Type;

   --  Start of processing for Init

   begin
      pragma Assert (Environment.Initial_Dir = Current_Directory);
      if Mimic_gcc (Cmd) then
         pragma Assert (False);
         pragma Assert (Directories.Exists (File_Name_File_Name.all),
                        File_Name_File_Name.all & " not found");
      else
         File_Name_File_Name := new String'
           (Directories.Compose (Environment.Tool_Temp_Dir.all, "file_names"));

         --  Create an empty file name file, so ASIS_Processing can append to
         --  it. (Small annoyance: the file is not actually empty; it contains
         --  a single blank line, and Finalize has to work around that.)

         Text_IO.Create (File_Name_File,
                         Name => File_Name_File_Name.all);
         Text_IO.Close (File_Name_File);

--         if Incremental_Mode then
--            Append (ASIS_UL.Environment.Extra_Inner_Pre_Args,
--                    String'("-asis-tool-args"));
--            Append (ASIS_UL.Environment.Extra_Inner_Post_Args,
--                    String'("-asis-tool-args"));
--            Append (Extra_Inner_Pre_Args,
--                    String'("--file-name-file=" & File_Name_File_Name.all));
--         end if;
      end if;
   end Init;

   -----------
   -- Final --
   -----------

   procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line) is
      --  If this is the outer process of an incremental build, or it is a
      --  non-incremental build, we move all the temp files to the output files.
      --  We don't need any file locking here, because all the inner processes
      --  that were writing to the File_Name_File have finished.

      pragma Unreferenced (Tool);
      use Text_IO;
      File_Name_File : File_Type;
      Ignored : Boolean;
      Count : Natural := 0; -- number of files moved
   begin
      if not Mimic_gcc (Cmd)
      --  and then not Nothing_To_Do
      then
         Open (File_Name_File, In_File, Name => File_Name_File_Name.all);

         --  The File_Name_File contains an initial blank line, due to Text_IO
         --  weirdness, so we need to discard it.

         declare
            Discard : constant String := Get_Line (File_Name_File);
            pragma Unreferenced (Discard);
         begin
            null;
         end;

         --  Read pairs of lines from the file name file, and do the moves.

         while not End_Of_File (File_Name_File) loop
            Count := Count + 1;
            declare
               Temp_Output_Name : constant String := Get_Line (File_Name_File);
               Output_Name : constant String := Get_Line (File_Name_File);
            begin
               if False then
                  Put_Line ("mv " & Temp_Output_Name & " " & Output_Name);
               end if;
               Move_File (Old_Name => Temp_Output_Name, New_Name => Output_Name);
            end;
         end loop;

         Close (File_Name_File);

         if not Debug_Flag_N then
            GNAT.OS_Lib.Delete_File (File_Name_File_Name.all, Ignored);
            --  No point in complaining on failure
         end if;

--         if Incremental_Mode and then Count = 0 then
--            Put_Line ("files are up to date");
--         end if;
      end if;
   end Final;

   ---------------------
   -- Per_File_Action --
   ---------------------

   pragma Warnings (Off); -- ????????????????

   type Output_Modes is
   --  Defines the where and how gnatpp places the result source.
     (Pipe,
      --  Sends the output into Stderr.
      Output,
      --  Creates the file with the name specified in 'o' option. If the
      --  file with the given name already exists, does not erase it and gives
      --  up.
      Output_Force,
      --  Creates the file with the name specified in 'o' option. If the
      --  file with the given name already exists, erases the old file and
      --  replaces it with the pretty-printed source.
      Replace,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is stored in the file <arg_source>.npp. If the file
      --  with such a name already exists, gnatpp gives up.
      Replace_Force,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is stored in the file <arg_source>.npp. If the file
      --  with such a name already exists, gnatpp overrides it.
      Replace_No_Backup,
      --  Replaces the argument source with the pretty-printed source. The
      --  original source is not stored in any back-up file.
      Default,
      --  Put the result source into <arg_source>.pp, overriding the existing
      --  file if any.
      Output_Directory);
      --  Put the result into <arg_source_simple_name> in directory Out_Dir.

   PP_Suffix : constant String := ".pp";
   NPP_Suffix : constant String := ".npp";
   --  The suffixes for the file names for default result and backup copy
   --  files.

   subtype Create_Modes is Output_Modes with
     Predicate => Create_Modes in Output | Output_Force;
   subtype Replace_Modes is Output_Modes with
     Predicate => Replace_Modes in Replace | Replace_Force | Replace_No_Backup;

   function Get_Output_Mode (Cmd : Command_Line) return Output_Modes;
   function Get_Output_Mode (Cmd : Command_Line) return Output_Modes is
      Result : Output_Modes := Default;
   begin
      if Arg (Cmd, Output_Directory) /= null then
         pragma Assert (Result = Default);
         Result := Output_Directory;
      end if;
      if Arg (Cmd, Pipe) then
         pragma Assert (Result = Default);
         Result := Pipe;
      end if;
      if Arg (Cmd, Replace) then
         pragma Assert (Result = Default);
         Result := Replace;
      end if;
      if Arg (Cmd, Replace_Force) then
         pragma Assert (Result = Default);
         Result := Replace_Force;
      end if;
      if Arg (Cmd, Replace_No_Backup) then
         pragma Assert (Result = Default);
         Result := Replace_No_Backup;
      end if;
      if Arg (Cmd, Output) /= null then
         pragma Assert (Result = Default);
         Result := Output;
      end if;
      if Arg (Cmd, Output_Force) /= null then
         pragma Assert (Result = Default);
         Result := Output_Force;
      end if;
      return Result;
   end Get_Output_Mode;

   ----------------

   function Is_Null (Tree : Ada_Node) return Boolean is (Tree = null);
   function T_Img (Tree : Ada_Node) return String is (Short_Image (Tree));

   package Lines_Data_Pkg is new Generic_Lines_Data
     (Ada_Node, Cmd_Error_No_Tool_Name);
   use Lines_Data_Pkg;
   use Line_Break_Vectors;
   use Tab_Vectors, Tab_In_Line_Vectors, Tab_In_Line_Vector_Vectors;
   Lines_Data : Lines_Data_Rec;

   Out_Buf : Buffer renames Lines_Data.Out_Buf;
   Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
   Next_Line_Break_Unique_Id : Modular
       renames Lines_Data.Next_Line_Break_Unique_Id;
   All_Line_Breaks : Line_Break_Vector renames Lines_Data.All_Line_Breaks;
   Tabs : Tab_Vector renames Lines_Data.Tabs;
   Src_Tokens : Scanner.Token_Vector renames Lines_Data.Src_Tokens;
   Pp_Off_On_Delimiters : Scanner.Pp_Off_On_Delimiters_Rec
       renames Lines_Data.Pp_Off_On_Delimiters;
   Check_Whitespace : Boolean renames Lines_Data.Check_Whitespace;

   procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Unit : Analysis_Unit)
   is
      pragma Unreferenced (Tool);

      use LAL_UL.Formatted_Output;

      Form_String : constant String := "WCEM=8";
      --  ????Should use Set_Form_String
      Write_BOM : Boolean := False;
      --  True if a byte order mark was found in the input file, in which case
      --  we want to write a BOM to the output file.

      Src_Buf : Buffer;
      --  Buffer containing the text of the original source file

      Output_Mode : constant Output_Modes := Get_Output_Mode (Cmd);
      Do_Diff : constant Boolean := Output_Mode in Replace_Modes;

      --  We initially write the output to Temp_Output_Name, then later rename it
      --  to Output_Name (except in Pipe mode). These are full pathnames. If we
      --  are overwriting the Source_Name, and it's a link link-->file, we want to
      --  overwrite file. But we put the temp file in the directory containing
      --  link, in case the directory containing file is not writable.

      function Get_Output_Name (Resolve_Links : Boolean) return String;
      function Get_Output_Name (Resolve_Links : Boolean) return String is
      begin
         pragma Assert (Environment.Initial_Dir = Current_Directory);
         return (case Output_Mode is
           when Pipe => "", -- not used
           when Output => Arg (Cmd, Output).all,
           when Output_Force => Arg (Cmd, Output_Force).all,
           when Replace_Modes => Normalize_Pathname
                                   (File_Name,
                                    Resolve_Links  => Resolve_Links,
                                    Case_Sensitive => True),

           when Default => File_Name & PP_Suffix,
           when Output_Directory =>
             Compose (Arg (Cmd, Output_Directory).all,
                      Simple_Name (File_Name)));
      end Get_Output_Name;

      Output_Name : constant String := Get_Output_Name (Resolve_Links => True);

      Temp_Output_Name : constant String :=
          (if Output_Mode = Pipe then "" -- means standard output
           else Get_Output_Name (Resolve_Links => False) & "__GNATPP-TEMP");

      Output_Written : Boolean := False;
      --  True if Tree_To_Ada wrote the output to Temp_Output_Name. It always
      --  does, except in Replace_Modes if the output would be identical to the
      --  input.

      procedure Write_File_Name_File;
      --  If the Output_Mode /= Pipe, and Output_Written is True, add a pair of
      --  lines to the file name file.

      procedure Write_File_Name_File is
         use Text_IO, GNAT.Lock_Files;
         Lock_File_Name : constant String := File_Name_File_Name.all & ".lock";

         procedure Do_Writes;
         --  Write the two file names to the file name file. This is split out
         --  into a procedure so we can call it with and without file locking, as
         --  appropriate.

         procedure Do_Writes is
            File_Name_File : File_Type;
         begin
            Open (File_Name_File,
                  Mode => Append_File,
                  Name => File_Name_File_Name.all);
            Put_Line (File_Name_File, Temp_Output_Name);
            Put_Line (File_Name_File, Output_Name);
            Close (File_Name_File);
         end Do_Writes;

      --  Start of processing for Write_File_Name_File

      begin
         if Output_Mode /= Pipe then
            --  In -r, -rf, and -rnb modes, if the output was identical to the
            --  input, Output_Written will be False, so there is no
            --  Temp_Output_Name file, so we don't move it in that case. This can
            --  also happen if the exception handler at the end of Tree_To_Ada is
            --  executed.

            pragma Assert
              (if Output_Mode not in Replace_Modes then Output_Written);
            if not Output_Written then
               return;
            end if;

--            if Mimic_gcc and then (Verbose_Mode or else Debug_Flag_V) then
--               Put_Line
--                 ((if Output_Mode in Replace_Modes
--                     then "updating "
--                     else "creating ") &
--                  (if Debug_Flag_V then Short_Source_Name (SF) else Output_Name));
--            end if;

            --  The temp file was created, so write a pair (Temp_Output_Name,
            --  Output_Name) of lines to the file name file, so Finalize will know
            --  to rename temp --> output. This is done under lock, in case this
            --  is an inner process of an incremental build, and the -j switch of
            --  the builder is used to invoke this in parallel.

            if Arg (Cmd, Outer_Parallel) then
               pragma Assert (Mimic_gcc (Cmd));
               Lock_File (Lock_File_Name, Wait => 0.1, Retries => 5 * 60 * 10);
               --  Retry for 5 minutes, every 100 milliseconds.
               declare
                  --  We create a dummy object whose finalization calls
                  --  Unlock_File, so we don't leave stale lock files around even
                  --  in case of unhandled exceptions.

                  type Dummy_Type is new Ada.Finalization.Limited_Controlled with
                    null record;
                  procedure Finalize (Ignore : in out Dummy_Type);
                  procedure Finalize (Ignore : in out Dummy_Type) is
                  begin
                     Unlock_File (Lock_File_Name);
                  end Finalize;

                  Dummy : Dummy_Type;

               begin
                  Do_Writes;
               end;

            --  Otherwise, it's safe to do the writes without any locking. We want
            --  to avoid locking when possible, because it reduces the likelihood
            --  of stale locks left lying around. It's a little more efficient,
            --  too.

            else
               Do_Writes;
            end if;
         end if;
      exception
         when Lock_Error =>
--            ASIS_UL.Output.Error ("cannot create " & Lock_File_Name);
--            ASIS_UL.Output.Error ("delete it by hand if stale");
            raise;
      end Write_File_Name_File;

      procedure Write_Str (Out_Elems : W_Str);
      procedure Write_Out_Buf (Out_Vec : Char_Vector);
      procedure Write_Src_Buf;
      --  Write_Out_Buf writes Out_Buf to the output. This is the normal
      --  case. Write_Src_Buf writes the Src_Buf to the output. Write_Str is the
      --  code common to both Write_Out_Buf and Write_Src_Buf.

      procedure Write_Str (Out_Elems : W_Str) is
         use Wide_Text_IO;
         Out_File : File_Type;
      begin
   --  ???
   --      if False then -- ???Messes up the diff's.
   --         Formatted_Output.Put
   --           ("--  ???Inner_Loop_Count = \1\n",
   --            Image (Inner_Loop_Count));
   --      end if;

         Output_Written := True;
         if Temp_Output_Name /= "" then
            --  If Temp_Output_Name = "", leave Current_Output pointing to
            --  standard output; otherwise point it to the file.
            Create (Out_File, Name => Temp_Output_Name,
                    Form => Form_String & ",Text_Translation=NO");
            Set_Output (Out_File);
         end if;

         --  If a BOM (byte order mark) was found in the input, we want to put it
         --  in the output.

         if Write_BOM then
   --         if Options.Output_Encoding /= System.WCh_Con.WCEM_UTF8 then
   --            raise Program_Error;
   --         end if;
            Put (W_Char'Val (16#FEFF#)); -- BOM as a wide character
         end if;

         --  We must call New_Line for LF's (at least for the last one in the
         --  Out_Elems), because otherwise Wide_Text_IO adds an annoying blank
         --  line to the end of the file. It would probably be better to avoid
         --  Wide_Text_IO altogether, but we're currently using it to do Unicode
         --  encoding transformations. Note that Put(CR) is not guaranteed to work
         --  by the Ada standard, but the GNAT implementation won't molest it.

         for C of Out_Elems loop
            if C = W_LF then
               New_Line;
            else
               Put (C);
            end if;
         end loop;

         if Temp_Output_Name /= "" then
            Close (Out_File);
            Set_Output (Ada.Wide_Text_IO.Standard_Output);
         end if;
      end Write_Str;

      procedure Write_Out_Buf (Out_Vec : Char_Vector) is
         pragma Assert (Point (Out_Buf) = 1);
         Out_Elems : W_Str renames Elems (Out_Vec)
           (2 .. Last_Index (Out_Vec)); -- 2 to skip initial NL
      begin
         --  In Do_Diff mode, don't write the output if it is identical to the
         --  input.

         if Do_Diff then
            declare
               Src_Elems : W_Str renames Elements (Src_Buf)
                 (1 .. Last_Position (Src_Buf));
            begin
               if Out_Elems = Src_Elems then
                  pragma Assert (not Output_Written);
                  return;
               end if;
            end;
         end if;

         Write_Str (Out_Elems);
      end Write_Out_Buf;

      procedure Write_Src_Buf is
         Out_Elems : W_Str renames Elements (Src_Buf)
           (1 .. Last_Position (Src_Buf));
      begin
         Write_Str (Out_Elems);
      end Write_Src_Buf;

      procedure Tree_To_Ada;
      procedure Tree_To_Ada is

         use Scanner;

      --  Start of processing for Tree_To_Ada

      begin
         if Debug_Mode then
            ASIS_UL.Dbg_Out.Output_Enabled := True;
         end if;

--         if not Template_Table_Initialized then
--            Init_Template_Table;
--            Init_Pp_Off_And_On;
--         end if;

         --  Note that if we're processing multiple files, we will get here multiple
         --  times, so we need to clear out data structures left over from last time.

         pragma Assert (Cur_Indentation = 0);
         Clear (All_Line_Breaks);
         Clear (Tabs);

         Get_Tokens (Src_Buf, Src_Tokens, LAL_UL.Ada_Version, Pp_Off_On_Delimiters);
         if Debug_Mode then
            Dbg_Out.Put ("Src_Tokens:\n");
            Put_Tokens (Src_Tokens);
            Dbg_Out.Put ("end Src_Tokens:\n");
         end if;

         Clear (Out_Buf);

         --  If --comments-only was specified, format the comments and quit

         if Arg (Cmd, Comments_Only) then
            Do_Comments_Only (Lines_Data, Src_Buf, Cmd);
         else
            --  Otherwise, convert the tree to text, and then run all the
            --  text-based passes.

            if True then
               pragma Assert (Is_Empty (Out_Buf));
               pragma Assert (At_Beginning (Src_Buf));
               declare
                  Temp : W_Str renames
                    Elements (Src_Buf) (1 .. Last_Position (Src_Buf));
               begin
                  Insert_NL (Out_Buf);
                  Insert_Any (Out_Buf, Temp);
                  Reset (Out_Buf);
               end;
            end if;
--            Convert_Tree_To_Ada (Root);
--            Post_Tree_Phases (Lines_Data, File_Name, Src_Buf, Cmd);
         end if;

         --  Finally, print out the result to Current_Output

         declare
--            Out_Vec : constant Char_Vector := Remove_Extra_Line_Breaks;
            Out_Vec : constant Char_Vector := To_Vector (Out_Buf);
         begin
            Write_Out_Buf (Out_Vec);
         end;

      exception
         --  If we got an error, don't produce output

--         when Common.Fatal_Error =>
--            raise;

         when others =>
            --  In order to avoid damaging the user's source code, if there is a bug
            --  (like a token mismatch in Final_Check), we avoid writing the output
            --  file in Do_Diff mode; otherwise, we write the input to the output
            --  unchanged. This happens only in production builds.

            if Enable_Token_Mismatch then
               raise;
            else
               if Do_Diff then
                  pragma Assert (not Output_Written);
               else
                  if not At_Beginning (Src_Buf) then
                     while not At_End (Src_Buf) loop
                        Move_Forward (Src_Buf);
                     end loop;
                     Reset (Src_Buf);
                  end if;

                  Write_Src_Buf;
               end if;
            end if;
      end Tree_To_Ada;

      --  ???See ada_trees.pp for commented-out stuff below.

      procedure Maybe_To_Ada
--        (CU : Asis.Compilation_Unit;
--         Cmd         : LAL_UL.Command_Lines.Command_Line;
--         Output_Name : String;
--         Form_String : String;
--         Do_Diff : Boolean;
--         Output_Written : out Boolean;
         (To_Ada : Boolean);
      --  Helper for Asis_To_Ada. To_Ada is True for the first call, indicating
      --  we're going to generate Ada text; it is False for subsequent (recursive)
      --  calls, which merely generate trees for dependencies.

      procedure Maybe_To_Ada
--        (CU      : Asis.Compilation_Unit;
--         Cmd         : LAL_UL.Command_Lines.Command_Line;
--         Output_Name : String;
--         Form_String : String;
--         Do_Diff : Boolean;
--         Output_Written : out Boolean;
         (To_Ada : Boolean)
      is
--         Src_Tokens : Scanner.Token_Vector;
--         Src_Gen_Regions : aliased Scanner.Token_Vector;
--         Gen_Regions : Scanner.Token_Vector_Ptr := null;
--         --  Set to point to Src_Gen_Regions if necessary.
--
--         Write_BOM : Boolean;
--         --  True if BOM should be written to the output
--
--         procedure Walk_Dependencies (CU : Asis.Compilation_Unit);
--         --  Recursively walk compilation units this one depends on.
--
--         procedure Walk_Dependencies (CU : Asis.Compilation_Unit) is
--            Ignore : Boolean;
--         begin
--            Maybe_To_Ada (CU, Cmd, "no Output_Name", "no Form_String",
--                          False, Ignore, To_Ada => False);
--         end Walk_Dependencies;
--
--         Do_Dependencies : constant Boolean :=
--           PP_Type_Casing (Cmd) /= PP_Name_Casing (Cmd);
--         --  Following all the dependencies is fairly expensive, so we only do it
--         --  if necessary. It is necessary in order to get the casing right for
--         --  the name of a task body, which should be PP_Type_Casing if it's the
--         --  body of a task type, and PP_Name_Casing if it's the body of a
--         --  singleton task. Same issue for protected bodies. See Do_Def_Name in
--         --  ada_trees-formatting-tree_to_ada.adb.
--
--         Id : constant Unit_Id := Set_Get.Get_Unit_Id (CU);
--         use type System.WCh_Con.WC_Encoding_Method;

      --  Start of processing for Maybe_To_Ada

      begin
--         while Cache_Last < Id loop
--            Cache_Last := Cache_Last + 1;
--            Cache (Cache_Last) := null;
--         end loop;
--         pragma Assert (Cache (Id) /= Pending);
--         if Cache (Id) /= null then
--            pragma Assert (not To_Ada);
--            return;
--         end if;
--
--         Cache (Id) := Pending;

         if To_Ada then -- ??? or Skip_Gen then
--            Read_Ada_File (Src_Buf, File_Name,
--                           Opt.Wide_Character_Encoding_Method, Write_BOM,
--                           Expand_Tabs => True);
            Read_Ada_File (Src_Buf, File_Name,
                           System.WCh_Con.WCEM_Brackets, Write_BOM,
                           Expand_Tabs => True);
            --  Expand tabs unconditionally. This differs from the behavior of
            --  the old gnatpp, which has an option for that (but only for
            --  comments).
--            pragma Assert
--              (if Write_BOM then
--                 Opt.Wide_Character_Encoding_Method = System.WCh_Con.WCEM_UTF8);
--
--            if Skip_Gen then
--               Scanner.Get_Tokens
--                 (Src_Buf, Src_Tokens, LAL_UL.Ada_Version, Pp_Off_On_Delimiters,
--                  Gen_Regions => Src_Gen_Regions'Unchecked_Access);
--               Gen_Regions := Src_Gen_Regions'Unchecked_Access;
--            end if;
         end if;

--         declare
--            Tree : constant Ada_Tree :=
--              Compilation_Unit_To_Tree (CU, Gen_Regions);
--         begin
--            Cache (Id) := Tree;
--            Resolve_Symbols (Tree);
--
--            if Ada_Trees.Debug_Mode or else ASIS_UL.Debug.Debug_Flag_2 then
--               Ada_Trees.Self_Rep.Put_Ada_Tree (Tree);
--               Put ("\n");
--            end if;
--
--            if Do_Dependencies then
--               Walk_Direct_Dependencies (CU, Walk_Dependencies'Access);
--            end if;
--
            if To_Ada then
               Tree_To_Ada;
--                 (Tree, Src_Buf, Write_BOM, Cmd, Output_Name,
--                  Form_String, Do_Diff, Output_Written, Is_PP => True);
            end if;
--         end;
      end Maybe_To_Ada;

   --  Start of processing for Per_File_Action

   begin
      if Debug_Flag_V then
         Print (Unit);
         Put ("With trivia\n");
         PP_Trivia (Unit);
      end if;

--      case Output_Mode is
--         when Pipe | Replace_Modes | Default =>
--            pragma Assert (Res_File_Name = null);
--            pragma Assert (Out_Dir = null);
--         when Create_Modes =>
--            pragma Assert (Res_File_Name /= null);
--            pragma Assert (Out_Dir = null);
--         when Out_Directory =>
--            pragma Assert (Res_File_Name = null);
--            pragma Assert (Out_Dir /= null);
--
--            if Out_Dir.all =
--              Containing_Directory (Source_Name (SF))
--            then
--               Error ("--output-dir=" & Out_Dir.all);
--               Error (" contains input file " & Short_Source_Name (SF));
--               Error (" skipping " & Short_Source_Name (SF));
--               Error (" use -rnb to update source files in place");
--               return;
--            end if;
--      end case;
--
--      Set_Output_Encoding;
--
--      if Output_Mode = Replace and then
--         Is_Regular_File (Source_Name (SF) & NPP_Suffix)
--      then
--         Put (Standard_Error, "gnatpp: file ");
--         Put (Standard_Error,
--              To_Wide_String (Source_Name (SF) & NPP_Suffix));
--         Put (Standard_Error, " exists. Use '-rf' option to override");
--         New_Line (Standard_Error);
--         return;
--      end if;
--
--      if Output_Mode in Replace | Force_Replace then
--
--         if Verbose_Mode then
--            Put (Standard_Error, "gnatpp: creating the back-up copy ");
--            Put (Standard_Error, "of the original source ");
--            Put (Standard_Error, To_Wide_String (Source_Name (SF)));
--            New_Line (Standard_Error);
--         end if;
--
--         declare
--            Success : Boolean;
--         begin
--            Copy_File
--              (Name     => Source_Name (SF),
--               Pathname => Source_Name (SF) & NPP_Suffix,
--               Success  => Success,
--               Mode     => Overwrite);
--
--            if not Success then
--               Put (Standard_Error, "gnatpp: can not create ");
--               Put (Standard_Error, "the back-up copy for ");
--               Put (Standard_Error, To_Wide_String (Source_Name (SF)));
--               New_Line (Standard_Error);
--            end if;
--         end;
--
--      end if;

--      pragma Assert (Is_Empty (Symtab));
      Maybe_To_Ada (To_Ada => True);
--        (CU, Cmd, Output_Name, Form_String,
--         Do_Diff, Output_Written, To_Ada => True);
--      --  We have to flush the cache here, because Unit_Id's get reused between
--      --  runs of this.
--      Flush_Cache;
--      Clear (Symtab);
      Write_File_Name_File;
   end Per_File_Action;

   ---------------
   -- Tool_Help --
   ---------------

   procedure Tool_Help (Tool : Pp_Tool) is
      pragma Unreferenced (Tool);
      use LAL_UL.Formatted_Output;
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
      use LAL_UL.Formatted_Output;
   begin
      if Debug_Flag_V then
         Put ("\1\n", Message);
      end if;
   end Dump;

end Pp.Actions;
