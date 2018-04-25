------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                                    Pp                                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2001-2017, AdaCore                      --
--                                                                          --
-- GNATPP  is free software; you can redistribute it and/or modify it under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;

with Libadalang.Analysis;

with Utils.Vectors;
with Utils.Char_Vectors; use Utils.Char_Vectors;
use Utils.Char_Vectors.WChar_Vectors;
with Utils.Symbols;
with Utils.Predefined_Symbols; use Utils.Predefined_Symbols;
with Utils.Command_Lines;
with Pp.Buffers; use Pp.Buffers; use Pp.Buffers.Marker_Vectors;
with Pp.Scanner.Seqs; use Pp.Scanner.Seqs.Token_Vectors;

package Pp.Formatting is

   package Syms renames Utils.Symbols;

   Token_Mismatch : exception;
   --  Raised by Tree_To_Ada if it detects a bug in itself that causes the
   --  output tokens to not match the input properly.

   -------------------
   -- Line Breaking --
   -------------------

   type Nesting_Level is new Positive;
   subtype Nesting_Level_Increment is
     Nesting_Level'Base range 0 .. Nesting_Level'Last;

   type Line_Break is record
      Mark : Marker;
      --  Marks the (potential) line break in the buffer. For a hard line
      --  break, there is an NL character at that position. For a soft one,
      --  there is initially nothing in the buffer; an NL will be inserted
      --  at Mark if the line break becomes enabled.
      --
      --  The reason for inserting NL characters is so we can call Get_Tokens
      --  on the buffer. The reason for not doing so for soft line breaks
      --  is that it's not necessary (there will always be something to
      --  prevent two tokens running together), and it makes the line
      --  length calculation simpler.

      Hard : Boolean;
      --  True for a hard line break, False for a soft one
      Affects_Comments : Boolean;
      --  True if the indentation of this Line_Break should affect the
      --  indentation of surrounding comments. For example, True for '$' but
      --  False for '%" (see type Ada_Template).
      Enabled : Boolean;
      --  True if this line break will appear in the final output
      Level : Nesting_Level := 1000;
      --  Nesting level of [...] (continuation-line indentation, mainly for
      --  soft line breaks).
      Indentation : Natural := 1000;
      --  Indentation level of this line break
      Length : Natural := Natural'Last;
      --  Number of characters in line, not counting NL. Calculated by
      --  Split_Lines. Valid only for enabled line breaks.

      --  For debugging:

--  ????      Kind     : Ada_Tree_Kind;
      Template : Syms.Symbol;
      UID      : Modular := 123_456_789;
   end record; -- Line_Break

   type Line_Break_Index is new Positive;
   type Line_Break_Array is array (Line_Break_Index range <>) of Line_Break;
   package Line_Break_Vectors is new Utils.Vectors
     (Line_Break_Index,
      Line_Break,
      Line_Break_Array);
   subtype Line_Break_Vector is Line_Break_Vectors.Vector;

   use Line_Break_Vectors;
   --  use all type Line_Break_Vector;

   type Line_Break_Index_Index is new Positive;
   type Line_Break_Index_Array is
     array (Line_Break_Index_Index range <>) of Line_Break_Index;
   package Line_Break_Index_Vectors is new Utils.Vectors
     (Line_Break_Index_Index,
      Line_Break_Index,
      Line_Break_Index_Array);
   subtype Line_Break_Index_Vector is Line_Break_Index_Vectors.Vector;
   use Line_Break_Index_Vectors;

   ------------------------
   -- Tabs and Alignment --
   ------------------------

   --  We use "tabs" to implement alignment. For example, if the input is:
   --     X : Integer := 123;
   --     Long_Ident : Boolean := False;
   --     Y : constant Long_Type_Name := Something;
   --  we're going to align the ":" and ":=" in the output, like this:
   --     X          : Integer                 := 123;
   --     Long_Ident : Boolean                 := False;
   --     Y          : constant Long_Type_Name := Something;
   --
   --  A "tab" appears before each ":" and ":=" in the above. This information
   --  is recorded in Tabs, below. The position of the tab in the buffer
   --  is indicated by Mark, which gets automatically updated as unrelated
   --  passes update Out_Buf. Finally, Insert_Alignment calculates the Col
   --  and Num_Blanks for each tab, and then inserts blanks accordingly.
   --
   --  A tab always occurs at the start of a token.

   type Tab_Index_In_Line is range 1 .. 9;
   --  We probably never have more than a few tabs in a given construct, so 9
   --  should be plenty, and it allows us to use a single digit in the
   --  templates, as in "^2".

   type Tab_Rec is record
      Parent, Tree : Libadalang.Analysis.Ada_Node;
      --  Tree is the tree whose template generated this tab, and Parent is its
      --  parent. Tree is used to ensure that the relevant tabs within a single
      --  line all come from the same tree; other tabs in the line are ignored.
      --  Parent is used across lines to ensure that all lines within a
      --  paragraph to be aligned together all come from the same parent tree.
      Token : Syms.Symbol := Name_Empty;
      --  This is some text associated with the Tab. Usually, it is the text of
      --  the token that follows the Tab in the template.
      Mark : Marker;
      --  Position in the buffer of the tab
      Index_In_Line   : Tab_Index_In_Line := Tab_Index_In_Line'Last;
      Col             : Positive          := Positive'Last;
      --  Column number of the tab
      Num_Blanks : Natural := 0;
      --  Number of blanks this tab should expand into
      Is_Fake : Boolean;
      --  True if this is a "fake tab", which means that it doesn't actually
      --  insert any blanks (Num_Blanks = 0). See Append_Tab for more
      --  explanation.
      Is_Insertion_Point : Boolean;
      --  False for "^", true for "&". Normally, "^" means insert blanks at the
      --  point of the "^" to align things. However, if there is a preceding
      --  (and matching) "&", then the blanks are inserted at the "insertion
      --  point" indicated by "&". This feature provides for
      --  right-justification.
      --  See Tree_To_Ada.Insert_Alignment.Calculate_Num_Blanks.Process_Line in
      --  pp-formatting.adb for more information.
   end record;

   type Tab_Index is new Positive;
   type Tab_Array is array (Tab_Index range <>) of Tab_Rec;
   package Tab_Vectors is new Utils.Vectors (Tab_Index, Tab_Rec, Tab_Array);
   subtype Tab_Vector is Tab_Vectors.Vector;

   use Tab_Vectors;
   --  use all type Tab_Vector;

   package Tab_In_Line_Vectors is new Ada.Containers.Bounded_Vectors
     (Tab_Index_In_Line,
      Tab_Index);
   use Tab_In_Line_Vectors;
   subtype Tab_In_Line_Vector is
     Tab_In_Line_Vectors
       .Vector
     (Capacity => Ada.Containers.Count_Type (Tab_Index_In_Line'Last));

   type Tab_In_Line_Vector_Index is new Positive;
   package Tab_In_Line_Vector_Vectors is new Ada.Containers.Indefinite_Vectors
     (Tab_In_Line_Vector_Index,
      Tab_In_Line_Vector);
   --  We use Indefinite_Vectors rather than Vectors because otherwise we get
   --  "discriminant check failed" at a-cobove.ads:371. I'm not sure whether
   --  that's a compiler bug.
   use Tab_In_Line_Vector_Vectors;

   type Lines_Data_Rec is record

      Out_Buf : Buffer;
      --  Buffer containing the text that we will eventually output as the
      --  final result. We first fill this with initially formatted text by
      --  walking the tree, and then we modify it repeatedly in multiple
      --  passes.

      Cur_Indentation : Natural := 0;

      Next_Line_Break_Unique_Id : Modular := 1;
      --  Used to set Line_Break.UID for debugging.

      --  Each line break is represented by a Line_Break appended onto the
      --  Line_Breaks vector. Hard line breaks are initially enabled. Soft
      --  line breaks are initially disabled, and will be enabled if
      --  necessary to make lines short enough.

      All_Line_Breaks : Line_Break_Vector;
      --  All line breaks in the whole input file. Built in two passes.

      Temp_Line_Breaks : Line_Break_Vector;
      --  Used by Insert_Comments_And_Blank_Lines to add new line breaks to
      --  All_Line_Breaks; they are appended to Temp_Line_Breaks, which is
      --  then merged with All_Line_Breaks when done. This is for efficiency
      --  and to keep the tables in source-location order.

      Enabled_Line_Breaks : Line_Break_Vector;
      --  All enabled line breaks
      Syntax_Line_Breaks : Line_Break_Vector;
      --  All (enabled) nonblank hard line breaks. These are called
      --  "Syntax_..."  because they are determined by the syntax (e.g. we
      --  always put a line break after a statement).

      --  ???Perhaps make the above tables contain Line_Break_Indexes
      --  instead of Line_Breaks. Can we use an index into a single table
      --  instead of UID?

      Tabs : Tab_Vector;
      --  All of the tabs in the whole input file, in increasing order

      Src_Tokens, -- from original source file (Src_Buf)
      Out_Tokens : -- from Out_Buf
        Scanner.Seqs.Token_Vector;

      Out_Buf_Line_Ends : aliased Marker_Vector;

      -------------------------------------
      -- Support for -pp-off and --pp-on --
      -------------------------------------

      Pp_Off_On_Delimiters : Scanner.Pp_Off_On_Delimiters_Rec;

      --  Debugging:

      Check_Whitespace : Boolean := True;
      --  Used during the Subtree_To_Ada phase. True except within comments and
      --  literals. Check for two blanks in a row.
   end record; -- Lines_Data_Rec

   procedure Collect_Enabled_Line_Breaks
     (Lines_Data : in out Lines_Data_Rec; Syntax_Also : Boolean);
   --  Collect all the enabled line breaks, and (if Syntax_Also is True) also
   --  the syntax line breaks. This reads All_Line_Breaks, and writes
   --  Enabled_Line_Breaks and Syntax_Line_Breaks.

   function Next_Enabled
     (Line_Breaks : Line_Break_Vector; F : Line_Break_Index)
     return Line_Break_Index;
   --  Next currently-enabled line break after F. Thus, F..Next_Enabled(F) is a
   --  line.

   function Is_Empty_Line
     (Out_Buf : Buffer;
      Line_Breaks : Line_Break_Vector;
      F, L : Line_Break_Index) return Boolean;
   --  True if F..L forms an empty line (or would, if both were enabled).

   ----------------

   function Good_Column
     (PP_Indentation : Positive; Indentation : Natural)
     return Natural is
     ((Indentation / PP_Indentation) * PP_Indentation);
   --  Make sure indentation is a multiple of PP_Indentation; otherwise style
   --  checking complains "(style) bad column".

   procedure Do_Comments_Only
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Implement the --comments-only switch. This skips most of the usual
   --  pretty-printing passes, and just formats comments.

   procedure Post_Tree_Phases
     (Lines_Data : in out Lines_Data_Rec;
      Messages : out Scanner.Source_Message_Vector;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line;
      Partial : Boolean);
   --  The first pretty-printing pass walks the tree and produces text,
   --  along with various tables. This performs the remaining passes, which
   --  do not make use of the tree.

   procedure Assert_No_Trailing_Blanks (Buf : Buffer);
   --  Assert that there are no lines with trailing blanks in Buf, and that
   --  all space characters are ' ' (e.g. no tabs), and that the last line
   --  is terminated by NL.

   ----------------

   --  Debugging:

   function Line_Text
     (Out_Buf : Buffer;
      Line_Breaks : Line_Break_Vector;
      F, L : Line_Break_Index) return W_Str;
   --  F and L are the first and last index forming a line; returns the text of
   --  the line, not including any new-lines.

   function Tab_Image
     (Out_Buf : Buffer; Tabs : Tab_Vector; X : Tab_Index) return String;

   procedure Put_Line_Breaks
     (Out_Buf : Buffer; Line_Breaks : Line_Break_Vector);
   --  ???This doesn't work unless Line_Breaks is All_Line_Breaks, because of
   --  various global variables!

   procedure Put_Line_Break (Out_Buf : Buffer; Break : Line_Break);

   procedure Put_Buf_With_Marks (Lines_Data : Lines_Data_Rec);

   procedure Format_Debug_Output
     (Lines_Data : Lines_Data_Rec; Message : String);

   Simulate_Token_Mismatch : Boolean renames Debug.Debug_Flag_8;
   Disable_Final_Check : Boolean renames Debug.Debug_Flag_7;
   function Enable_Token_Mismatch return Boolean is
      ((Assert_Enabled or Debug.Debug_Flag_5)
         and not Simulate_Token_Mismatch
         and not Debug.Debug_Flag_6);

   ----------------------------------------------------------------
   --
   --  Formatting uses the following major passes. Convert_Tree_To_Ada is in
   --  Pp.Actions. Split_Lines through Final_Check are done by Post_Tree_Phases
   --  above.
   --
   --  Convert_Tree_To_Ada
   --     Walks the Ada_Tree, using Ada_Templates to convert the tree into
   --     text form in Out_Buf. Out_Buf is further modified by subsequent
   --     passes. Builds the Line_Break table for use by Split_Lines and
   --     Insert_NLs_And_Indentation. Builds the Tabs table for use by
   --     Insert_Alignment.
   --
   --     Subsequent passes work on the text in Out_Buf, and not the
   --     Ada_Tree. Therefore, if they need any syntactic/structural
   --     information, it must be encoded in other data structures, such
   --     as the Line_Breaks and Tabs tables.
   --
   --  Split_Lines (first time)
   --     Determine which soft line breaks should be enabled.
   --
   --  Enable_Line_Breaks_For_EOL_Comments
   --     For all end-of-line comments that occur at a soft line break, enable
   --     the line break.
   --
   --  Insert_Comments_And_Blank_Lines
   --     Step through the source tokens and Out_Buf tokens. Copy comment and
   --     blank line tokens into Out_Buf as they are encountered.
   --
   --  Split_Lines (again)
   --     We do this again because inserted end-of-line comments can cause
   --     lines to be too long. We don't want to split the line just before the
   --     comment; we want to split at some auspicious soft line break(s).
   --
   --  Insert_NLs_And_Indentation
   --     Insert newline characters and leading blanks for each soft line break
   --     that was enabled by Split_Lines.
   --
   --  Insert_Alignment
   --     Walk the Tabs table to calculate how many blanks (if any) should be
   --     inserted for each Tab. Then insert those blanks in Out_Buf.
   --
   --  Keyword_Casing
   --     Convert reserved words to the appropriate case as specified by
   --     command-line options.
   --
   --  Insert_Form_Feeds
   --     Implement the --ff-after-pragma-page switch, by inserting FF
   --     characters after "pragma Page;".
   --
   --  Copy_Pp_Off_Regions
   --     Regions where pretty printing should be turned off have been
   --     formatted as usual. This phase undoes all that formatting by copying
   --     text from Src_Buf to Out_Buf.
   --
   --  Final_Check
   --     Go through the source tokens and Out_Buf tokens (the latter now
   --     containing comments and blank lines), and make sure they (mostly)
   --     match. If there is any mismatch besides a small set of allowed ones,
   --     raise an exception. This pass makes no changes, so it serves no
   --     useful purpose unless there is a bug in some previous pass; the
   --     purpose is to prevent gnatpp from damaging the user's source code.
   --     The algorithm in this pass is quite similar to the one in
   --     Insert_Comments_And_Blank_Lines.
   --
   --  Write_Out_Buf
   --     Write Out_Buf to the appropriate file (or Current_Output).
   --
   --  Each pass expects to be entered with Out_Buf's 'point' at the beginning,
   --  and returns with Out_Buf's 'point' STILL at the beginning. Thus, passes
   --  that step through Out_Buf need to call Reset(Out_Buf) before returning.
   --
   ----------------------------------------------------------------

end Pp.Formatting;
