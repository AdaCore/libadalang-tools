------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                      Copyright (C) 2001-2022, AdaCore                    --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Paragraph_Filling;

with Utils.Command_Lines.Common;
with Utils.Err_Out;
with Utils.Formatted_Output;
with Utils.Generic_Formatted_Output;
with Utils.Symbols; use Utils.Symbols;

with Langkit_Support.Slocs; use Langkit_Support;

with Pp.Command_Lines; use Pp.Command_Lines;
with Pp.Error_Slocs; use Pp.Error_Slocs;
with Pp.Scanner.Lines;
with Pp.Actions;

package body Pp.Formatting is
   use Utils.Command_Lines;

   use Pp_Flag_Switches, Pp_Boolean_Switches, Pp_Nat_Switches;

   function Next_Enabled
     (Lines_Data : Lines_Data_Rec;
      F : Line_Break_Index_Index)
     return Line_Break_Index_Index;
   --  Next currently-enabled line break after F. Thus, F..Next_Enabled(F) is a
   --  line.

   function Good_Column
     (PP_Indentation : Positive; Indentation : Natural) return Natural is
       ((Indentation / PP_Indentation) * PP_Indentation);
   --  Make sure indentation is a multiple of PP_Indentation; otherwise style
   --  checking complains "(style) bad column".

   procedure Tokns_To_Buffer
     (Buf : in out Buffer; Tokns : Scanner.Tokn_Vec;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Turns a sequence of tokens back into text. Overwrites Buf, and leaves
   --  'point' at the beginning. Whole_Line_Comment takes their indentation
   --  from the previous Spaces token, if any.

   --  The following Next_ss/Prev_ss are the same as Scanner.Next/Prev, except
   --  they skip a following Spaces token, if present. The "ss" stands for
   --  "skip spaces".

   function Next_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor;
   procedure Next_ss (Cur : in out Scanner.Tokn_Cursor);
   function Prev_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor;
   procedure Prev_ss (Cur : in out Scanner.Tokn_Cursor);

   procedure Next_ssnl (Cur : in out Scanner.Tokn_Cursor);
   --  Same as Next_ss, but also skip line endings

   procedure Next_ssnl (Cur : in out Scanner.Tokn_Cursor) is
      use Scanner;
   begin
      Next (Cur);

      while Kind (Cur) in Enabled_LB_Token | EOL_Token loop
         Next (Cur);
      end loop;

      if Kind (Cur) = Spaces then
         Next (Cur);
         pragma Assert
           (Kind (Cur) not in Enabled_LB_Token | EOL_Token | Spaces);
      end if;
   end Next_ssnl;

   procedure Next_ss (Cur : in out Scanner.Tokn_Cursor) is
      use Scanner;
   begin
      Next (Cur);
      while Kind (Cur) = Spaces loop
         Next (Cur);
      end loop;
      pragma Assert (Kind (Cur) not in Spaces);
   end Next_ss;

   function Next_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor is
   begin
      return Result : Scanner.Tokn_Cursor := Cur do
         Next_ss (Result);
      end return;
   end Next_ss;

   procedure Prev_ss (Cur : in out Scanner.Tokn_Cursor) is
      use Scanner;
   begin
      Prev (Cur);

      if Kind (Cur) = Spaces then
         Prev (Cur);
         pragma Assert (Kind (Cur) /= Spaces);
      end if;
   end Prev_ss;

   function Prev_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor is
   begin
      return Result : Scanner.Tokn_Cursor := Cur do
         Prev_ss (Result);
      end return;
   end Prev_ss;

   subtype Symbol is Syms.Symbol;

   function "=" (X, Y : Symbol) return Boolean renames Syms."=";

   subtype Ada_Node is Libadalang.Analysis.Ada_Node;
   use type Ada_Node;
   function Is_Null (Tree : Ada_Node) return Boolean is (Tree.Is_Null);
   function T_Img (Tree : Ada_Node) return String is (Tree.Image);

   Op_Sym_Table : constant array (Positive range <>) of Symbol :=
     [Name_Q_And,
      Name_Q_Or,
      Name_Q_Xor,
      Name_Q_Mod,
      Name_Q_Rem,
      Name_Q_Abs,
      Name_Q_Not];

   function Is_Op_Sym_With_Letters (N : Symbol) return Boolean is
     (for some Op of Op_Sym_Table => Case_Insensitive_Equal (N, Op));
      --  True if N looks like a string literal that can be used as an operator
      --  symbol containing letters, so case might matter. N should be in all
      --  lower case.

   function Sname_83 (Tok : Scanner.Tokn_Cursor) return Boolean;
   --  True if Tok can be a simple_name (in Ada 83). This includes reserved
   --  words that were added to the language after Ada 83. Needed because we
   --  don't know which language version is being used.

   function Sname_83 (Tok : Scanner.Tokn_Cursor) return Boolean is
      use Scanner;
   begin
      return Kind (Tok) in Ident | String_Lit | Reserved_Word_New;
   end Sname_83;

   procedure Insert_Comment_Text
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Utils.Command_Lines.Command_Line;
      Comment_Tok : Scanner.Tokn_Cursor);
   --  Insert the text of the comment into New_Tokns, including the initial
   --  "--" and leading blanks.
   --  This will eventually be replaced by Comment_Tokn_To_Buf.

   procedure Append_Temp_Line_Break
     (Lines_Data_P : Lines_Data_Ptr; Org : String);

   function Equal_Ignoring_CR (Src_S, Out_S : Symbol) return Boolean;
   --  Used in Match functions below, where the source and output tokens
   --  should be identical, except that source line endings can contain
   --  CR (the Windows convention).

   -----------------------
   -- Equal_Ignoring_CR --
   -----------------------

   function Equal_Ignoring_CR (Src_S, Out_S : Symbol) return Boolean is
   begin
      if Src_S = Out_S then
         return True;
      end if;

      return Replace_String
        (Str (Src_S).S, From => [1 => ASCII.CR], To => "")
       = Str (Out_S).S;
   end Equal_Ignoring_CR;

   ----------------

   function Next_Enabled
     (Lines_Data : Lines_Data_Rec;
      F : Line_Break_Index_Index)
     return Line_Break_Index_Index
   is
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      First :  Line_Break renames All_LB (All_LBI (F));
      pragma Assert (First.Enabled);
      Result : Line_Break_Index_Index := F + 1;
      Last   : Line_Break := All_LB (All_LBI (Result));
   begin
      while not Last.Enabled loop
         --  Add this verification to prevent, in case of partial formatting
         --  mode of gnatpp, to get an exceeding value for the table index.
         if Result /= Last_Index (All_LBI) then
            Result := Result + 1;
            Last   := All_LB (All_LBI (Result));
         end if;

         --  In some situation when partial formatting is performed no line
         --  break is enabled after the 1st one. Adding this exit condition
         --  to cover this situation.
         exit when Result = Last_Index (All_LBI);
      end loop;

      --???      pragma Assert (First.Level = Last.Level);
      return Result;
   end Next_Enabled;

   ----------------

   procedure Assert_No_Trailing_Blanks (V : WChar_Vector);
   --  Assert that there are no lines with trailing blanks in V, and that
   --  all space characters are ' ' (e.g. no tabs), and that the last line
   --  is terminated by NL.

   procedure Raise_Token_Mismatch
     (Message              : String;
      Lines_Data           : Lines_Data_Rec;
      Src_Buf              : Buffer;
      Src_Tok, Out_Tok     : Scanner.Tokn_Cursor);
   --  Called when either Insert_Comments_And_Blank_Lines or Final_Check finds
   --  a mismatch. Prints debugging information and raises Token_Mismatch.

   procedure Final_Check_Helper
     (Lines_Data_P : Lines_Data_Ptr;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line);
   procedure Final_Check
     (Lines_Data_P   : Lines_Data_Ptr;
      Src_Buf        : in out Buffer;
      Cmd            : Utils.Command_Lines.Command_Line;
      Pp_Off_Present : Boolean);
   --  Final pass: check that we have not damaged the input source text.
   --  except that comments are now included in Out_[Tokens|Buf], and this
   --  checks that they match the ones in Src_Tokns. Final_Check simply
   --  calls Final_Check_Helper, plus asserts that Out_Buf wasn't modified.
   --  If Pp_Off_Present is True, then do not check for trailing spaces since
   --  pp off regions are allowed to have them.

   --  The code in Final_Check[_Helper] is parallel to the code in
   --  Insert_Comments_And_Blank_Lines, so there's a bit of code duplication.
   --  It is worth it to keep Final_Check[_Helper] as simple as possible. If
   --  you make changes to one, consider making similar changes to the other.

   function Num_Lits_Match
     (Src_Tok, Out_Tok : Scanner.Tokn_Cursor;
      Cmd : Utils.Command_Lines.Command_Line) return Boolean;
   --  Called by the Match functions for Numeric_Literal

   function Num_Lits_Match
     (Src_Tok, Out_Tok : Scanner.Tokn_Cursor;
      Cmd : Utils.Command_Lines.Command_Line) return Boolean
   is
      use Scanner;
   begin
      return R : Boolean do
         if Text (Src_Tok) = Text (Out_Tok) then
            R := True;
         else
            declare
               Src_Tok_Text : constant W_Str := To_W_Str (Text (Src_Tok));
               Out_Tok_Text : constant W_Str := To_W_Str (Text (Out_Tok));
            begin
               --  If we're not inserting underscores, then the tokens must be
               --  identical to match. Note that if the source token already
               --  contains underscores, we don't modify it.

               if (Arg (Cmd, Decimal_Grouping) = 0
                     and then Arg (Cmd, Based_Grouping) = 0)
                 or else Find (Src_Tok_Text, "_") /= 0
               then
                  R := False;
               else
                  R := Src_Tok_Text = Replace_All (Out_Tok_Text, "_", "");
               end if;
            end;
         end if;
      end return;
   end Num_Lits_Match;

   procedure Assert_No_Trailing_Blanks (V : WChar_Vector) is
   begin
      for X in 2 .. Last_Index (V) loop
         declare
            C : constant W_Char := V (X);
         begin
            pragma Assert (if C /= ' ' then not Is_Space (C));

            if C = NL then
               pragma Assert (V (X - 1) /= ' ');
            end if;
         end;
      end loop;

      pragma Assert (Last_Element (V) = NL);
   end Assert_No_Trailing_Blanks;

   procedure Append_Temp_Line_Break
     (Lines_Data_P : Lines_Data_Ptr; Org : String)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      Temp_LBI : Line_Break_Index_Vector renames Lines_Data.Temp_LBI;
   begin
      Append
        (All_LB,
         Line_Break'
           (Tok | Tokn_Val             => <>, -- Initial value not used
            Hard                       => True,
            Affects_Comments           => False,
            Enabled                    => True,
            Source_Line_Breaks_Enabled => False,
            Level                      => 1,
            Indentation                => Cur_Indentation,
            Bin_Op_Count               => 0,
            Length                     => <>
--            Kind        => Not_An_Element
           ));
      Append (Temp_LBI, Last_Index (All_LB));

      Scanner.Lines.Append_Line_Break_Tokn
        (New_Tokns, Enabled => True,
         Index => Last_Index (All_LB), Org => Org);
   end Append_Temp_Line_Break;

   procedure Insert_Comment_Text
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Utils.Command_Lines.Command_Line;
      Comment_Tok : Scanner.Tokn_Cursor)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      use Scanner;

      function Filled_Text
        (Comment_Tok    : Tokn_Cursor;
         Leading_Blanks : Natural)
         return           W_Str;
      --  Returns the text of the comment after filling (see
      --  GNATCOLL.Paragraph_Filling).

      function Filled_Text
        (Comment_Tok    : Tokn_Cursor;
         Leading_Blanks : Natural)
         return           W_Str
      is
         use GNATCOLL.Paragraph_Filling, Ada.Strings.Unbounded;
         S1 : constant String := Str (Text (Comment_Tok)).S;
         S2 : constant String :=
           To_String
             (Pretty_Fill
                (S1,
                 Max_Line_Length =>
                   Arg (Cmd, Max_Line_Length) -
                   (Cur_Indentation + String'("--")'Length + Leading_Blanks)));
      begin
         return From_UTF8 (S2);
      end Filled_Text;

      --  Comments_Gnat_Beginning causes the comment to start with at least 2
      --  blanks.
      --  This is only aplicable to Fillable_Comment or
      --  Other_Whole_Line_Comment tokens that are not part of a header
      --  comment.

      Leading_Blanks : constant Natural :=
        (if Arg (Cmd, Comments_Gnat_Beginning)
           and then not Is_Header_Comment (Token_At_Cursor (Comment_Tok))
           and then Kind (Comment_Tok) in
                      Fillable_Comment | Other_Whole_Line_Comment
         then
           Natural'Max (Scanner.Leading_Blanks (Comment_Tok), 2)
         else Scanner.Leading_Blanks (Comment_Tok));
      --  In Comments_Only mode, we need to indent "by hand" here. In normal
      --  mode, Cur_Indentation will be heeded by the line breaks.
      Do_Filling : constant Boolean :=
        Comment_Filling_Enabled (Cmd)
          and then Kind (Comment_Tok) = Fillable_Comment;
      Text : constant W_Str :=
        (if Do_Filling then Filled_Text (Comment_Tok, Leading_Blanks)
         else To_W_Str (Scanner.Text (Comment_Tok)));

   --  Start of processing for Insert_Comment_Text

   begin
      Append_Comment_Text
        (New_Tokns, Comment_Tok, Text,
         Recompute_Length => True,
         Comments_Only => Arg (Cmd, Comments_Only),
         Comments_Gnat_Beginning => Arg (Cmd, Comments_Gnat_Beginning),
         Org => "Insert_Comment_Text");
      --  It would be good to avoid dealing with text here, and avoid
      --  recomputing the length all the time.
   end Insert_Comment_Text;

   procedure Comment_Tokn_To_Buf
     (Buf : in out Buffer;
      Comment_Tok : Scanner.Tokn_Cursor;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Called by Tokns_To_Buffer in the comment case, which is the most
   --  complicated.

   procedure Comment_Tokn_To_Buf
     (Buf : in out Buffer;
      Comment_Tok : Scanner.Tokn_Cursor;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      use Scanner;

      function Filled_Text
        (Comment_Tok    : Tokn_Cursor)
         return           W_Str;
      --  Returns the text of the comment after filling (see
      --  GNATCOLL.Paragraph_Filling).

      --  Comments_Gnat_Beginning causes the comment to start with at least 2
      --  blanks.

      pragma Assert
        (if Arg (Cmd, Comments_Gnat_Beginning)
           and then not Is_Header_Comment (Token_At_Cursor (Comment_Tok))
           and then Kind (Comment_Tok) in
                      Fillable_Comment | Other_Whole_Line_Comment
         then
           Scanner.Leading_Blanks (Comment_Tok) >= 2);
      Prev_Tok : constant Tokn_Cursor := Prev (Comment_Tok);
      pragma Assert (if Kind (Comment_Tok) in Whole_Line_Comment then
        Kind (Prev_Tok) in Spaces | EOL_Token | Line_Break_Token);
      Indentation : constant W_Str :=
        (if Kind (Comment_Tok) in Whole_Line_Comment then
          (if Kind (Prev_Tok) = Spaces then To_W_Str (Text (Prev_Tok)) else "")
         else "");
      pragma Assert
        (if Kind (Comment_Tok) in Whole_Line_Comment then
           Indentation'Length = Sloc_Col (Comment_Tok) - 1);
      First_Line_Prelude : constant W_Str :=
          "--" & [1 .. Scanner.Leading_Blanks (Comment_Tok) => ' '];
      --  String that precedes the comment Text (first line)
      Subsequent_Prelude : constant W_Str := Indentation & First_Line_Prelude;
      --  String that precedes subsequent line of the comment Text

      function Filled_Text
        (Comment_Tok    : Tokn_Cursor)
         return           W_Str
      is
         use GNATCOLL.Paragraph_Filling, Ada.Strings.Unbounded;
         S1 : String renames Str (Text (Comment_Tok)).S;
         S2 : constant String :=
           To_String
             (Pretty_Fill
                (S1,
                 Max_Line_Length =>
                   Arg (Cmd, Max_Line_Length) - Subsequent_Prelude'Length));
      begin
         return From_UTF8 (S2);
      end Filled_Text;

      Do_Filling : constant Boolean :=
          Arg (Cmd, Comments_Only)
          and then Comment_Filling_Enabled (Cmd)
          and then Kind (Comment_Tok) = Fillable_Comment;
      Text_NL : constant W_Str :=
        (if Do_Filling then Filled_Text (Comment_Tok)
         else To_W_Str (Scanner.Text (Comment_Tok)));
      pragma Assert (Text_NL (Text_NL'Last) = NL);
      Text : W_Str renames Text_NL (Text_NL'First .. Text_NL'Last - 1);
      --  Skip last NL

   --  Start of processing for Comment_Tokn_To_Buf

   begin
      Insert (Buf, First_Line_Prelude);

      for X in Text'Range loop
         if Text (X) = NL then
            Insert_NL (Buf);
            Insert (Buf, Subsequent_Prelude);
         else
            Insert (Buf, Text (X));
         end if;
      end loop;
   end Comment_Tokn_To_Buf;

   procedure Tokns_To_Buffer
     (Buf : in out Buffer; Tokns : Scanner.Tokn_Vec;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      use Scanner;
   begin
      Clear (Buf);
      if not Is_Empty (Tokns) then
         declare
            Cur : Tokn_Cursor := Next (First (Tokns'Unrestricted_Access));
         begin
            while not After_Last (Cur) loop
               if Kind (Cur) in Comment_Kind then
                  Comment_Tokn_To_Buf (Buf, Cur, Cmd);
               else
                  Insert_Any (Buf, To_W_Str (Text (Cur)));
               end if;
               Next (Cur);
            end loop;
            Reset (Buf);
         end;
      end if;
   end Tokns_To_Buffer;

   procedure Do_Comments_Only
     (Lines_Data_P : Lines_Data_Ptr;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      use Scanner;
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      Src_Toks : aliased Tokn_Vec;
      pragma Assert (Is_Empty (Src_Toks));
      Ignored : Boolean := Get_Tokns
        (Src_Buf, Src_Toks,
         Comments_Special_On => Arg (Cmd, Comments_Special));
      Cur_Tok : Tokn_Cursor :=
        Next (First (Src_Toks'Unchecked_Access)); -- skip sentinel

      Saved_New_Tokns : Scanner.Tokn_Vec renames Lines_Data.Saved_New_Tokns;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      Ignore : Boolean := Move_Tokns
        (Target => Saved_New_Tokns, Source => New_Tokns);

      procedure Reset_Indentation;
      --  Set the indentation to it's initial value (usually 0, but can be set
      --  by the --initial-indentation switch.

      procedure Reset_Indentation is
      begin
         Cur_Indentation := Arg (Cmd, Initial_Indentation);
      end Reset_Indentation;

   --  Start of processing for Do_Comments_Only

   begin
      Append_Tokn (New_Tokns, Start_Of_Input);
      Append_Tokn (New_Tokns, True_End_Of_Line_LF);

      while Kind (Cur_Tok) /= End_Of_Input loop
         if Kind (Cur_Tok) in Comment_Kind then
            --  Set Cur_Indentation to the number of spaces to be inserted
            --  before "--". For whole-line comments, that's one less than the
            --  starting column. For end-of-line comments, it's the number of
            --  blanks between the last character of the previous token to the
            --  first character of this (comment) token.

            case Comment_Kind'(Kind (Cur_Tok)) is
               when Whole_Line_Comment =>
                  Cur_Indentation := Sloc (Cur_Tok).Col - 1;
               when End_Of_Line_Comment =>
                  Cur_Indentation :=
                    Sloc (Cur_Tok).First - Sloc (Prev_ss (Cur_Tok)).Last - 1;
            end case;

            Insert_Comment_Text (Lines_Data_P, Cmd, Cur_Tok);
            Reset_Indentation;
         else
            Append_Tokn (New_Tokns, Cur_Tok, Org => "only, other");
         end if;

         Next (Cur_Tok);
      end loop;

      Append_Tokn (New_Tokns, End_Of_Input);
      Clear (Saved_New_Tokns);

      Tokns_To_Buffer (Lines_Data.Out_Buf, New_Tokns, Cmd);

      Final_Check (Lines_Data_P, Src_Buf, Cmd, False);

   end Do_Comments_Only;

   Post_Tree_Phases_Done : exception;

   procedure Keyword_Casing
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line);
   --  Convert reserved words to lower/upper case based on command-line
   --  options.

   procedure Insert_Form_Feeds
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line);
   --  Insert FF after "pragma Page;" if --ff-after-pragma-page switch was
   --  given. It might seem silly to have a whole extra pass for this little
   --  feature, but it's a rarely used feature, so we don't care if it's a
   --  little slower, and this seems cleanest. We could have put this
   --  processing in some other unrelated pass. Note that it would not be
   --  easy to do this in Convert_Tree_To_Ada, because the FF goes after the
   --  ";", and the ";" is not printed as part of the pragma -- it goes
   --  BETWEEN the pragma and whatever comes next. Furthermore, we want to
   --  do this last so the FF doesn't get turned back into NL.

   procedure Copy_Pp_Off_Regions
     (Input          : Char_Vector;
      Lines_Data_P   : Lines_Data_Ptr;
      Pp_Off_Present : Boolean;
      Cmd            : Command_Line);
   --  Input is the the Char_Vector with the original source. This should
   --  be an immaculate copy of the source file, since pp off regions should
   --  not change at all.
   --  Out_Buf is fully formatted at this point, including regions where
   --  pretty printing is supposed to be turned off. This replaces those
   --  regions of Out_Buf with the corresponding regions of Src_Buf. Note
   --  that this destroys any markers that might be pointing to Out_Buf.

   procedure Keyword_Casing
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line)
   is
   begin
      --  The usual case is Lower_Case, in which case there's nothing to do,
      --  because all of the Ada_Templates have reserved words in lower case.
      --  If it's Upper_Case, we loop through the tokens, converting reserved
      --  words to upper case.
      case PP_Keyword_Casing (Cmd) is
         when Lower_Case =>
            null;

         when Upper_Case =>
            declare
               use Scanner;
               Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
               Out_Buf : Buffer renames Lines_Data.Out_Buf;
               Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;

               Ignored : Boolean := Get_Tokns
                 (Out_Buf, Out_Tokns,
                  Comments_Special_On => Arg (Cmd, Comments_Special));

               Out_Tok : Tokn_Cursor := First (Out_Tokns'Access);

            begin
               Outer_Loop :
               while not After_Last (Out_Tok) loop
                  Next_ss (Out_Tok);
                  Error_Sloc := To_Langkit (Sloc (Out_Tok));

                  loop
                     if Kind (Out_Tok) in Reserved_Word then
                        Replace_Cur (Out_Buf, To_Upper (Cur (Out_Buf)));
                     end if;

                     Move_Forward (Out_Buf);

                     exit Outer_Loop when At_End (Out_Buf);
                     --  If there are extra blank lines at the end of file,
                     --  then we need the At_End test.

                     exit when At_Point (Out_Buf, Next_Sloc_First (Out_Tok));
                  end loop;
               end loop Outer_Loop;

               Reset (Out_Buf);
               Clear (Out_Tokns);
            end;
      end case;
   end Keyword_Casing;

   procedure Insert_Form_Feeds_Helper
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line);

   procedure Insert_Form_Feeds
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line) is
   begin
      --  Ignore this switch for now. It's not clear that anyone uses it, and
      --  we have much bigger issues that need fixing.

      if False and then Arg (Cmd, Ff_After_Pragma_Page) then
         Insert_Form_Feeds_Helper (Lines_Data_P, Cmd);
      end if;
   end Insert_Form_Feeds;

   procedure Insert_Form_Feeds_Helper
     (Lines_Data_P : Lines_Data_Ptr;
      Cmd : Command_Line)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
      use Scanner;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns,
         Comments_Special_On => Arg (Cmd, Comments_Special));

      Prev_Prev_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      Prev_Tok : Tokn_Cursor := Next_ss (Prev_Prev_Tok);
      Out_Tok : Tokn_Cursor := Next_ss (Prev_Tok);
      --  Out_Tok skips sentinel and first 3 tokens
   begin
      while not At_Last (Out_Tok) loop
         Error_Sloc := To_Langkit (Sloc (Out_Tok));
         loop
            Move_Forward (Out_Buf);
            exit when At_Point (Out_Buf, Next_Sloc_First (Out_Tok));
         end loop;

         if Kind (Out_Tok) = ';'
           and then Kind (Prev_Tok) = Ident
           and then Case_Insensitive_Equal (Text (Prev_Tok), Name_Page)
           and then Kind (Prev_Prev_Tok) = Res_Pragma
         then
            Insert_Any (Out_Buf, W_FF);
         end if;

         Prev_Prev_Tok := Prev_Tok;
         Prev_Tok := Out_Tok;
         Next_ss (Out_Tok);
      end loop;

      Reset (Out_Buf);
      Clear (Out_Tokns);
   end Insert_Form_Feeds_Helper;

   procedure Copy_Pp_Off_Regions_Helper
     (Input        : Char_Vector;
      Lines_Data_P : Lines_Data_Ptr;
      Cmd          : Command_Line);

   procedure Copy_Pp_Off_Regions
     (Input          : Char_Vector;
      Lines_Data_P   : Lines_Data_Ptr;
      Pp_Off_Present : Boolean;
      Cmd            : Command_Line) is
   begin
      --  Optimize by skipping this phase if there are no Pp_Off_Comments
      if Pp_Off_Present then
         Copy_Pp_Off_Regions_Helper (Input, Lines_Data_P, Cmd);
      end if;
   end Copy_Pp_Off_Regions;

   procedure Copy_Pp_Off_Regions_Helper
     (Input        : Char_Vector;
      Lines_Data_P : Lines_Data_Ptr;
      Cmd          : Command_Line)
   is
      Src_Buf   : Buffer;

      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      Out_Buf    : Buffer renames Lines_Data.Out_Buf;

      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;

      use Scanner;

      New_Buf : Buffer;
      --  Buffers don't support deletion, so we need to build up a whole new
      --  Buffer. This will be moved into Out_Buf when we are done.

      procedure Get_Next_Off_On
        (Tok    : in out Tokn_Cursor;
         Expect : Pp_Off_On_Comment);
      --  Get the next OFF or ON (or End_Of_Input). The token itself is
      --  returned in Tok. Expect is purely for assertions; it alternates
      --  between OFF and ON; Tok must be as expected (or End_Of_Input).

      procedure Copy (Buf : in out Buffer; Up_To : Positive);
      --  Copy from Buf to New_Buf, up to the given position

      procedure Skip (Buf : in out Buffer; Up_To : Positive);
      --  Move forward in Buf, up to the given position, ignoring the
      --  characters.

      procedure Indent_Pp_Off (Src_Tok, Out_Tok : Tokn_Cursor) with
        Pre => Kind (Src_Tok) = Kind (Out_Tok)
          and then Kind (Src_Tok) in Pp_Off_Comment | End_Of_Input
          and then Kind (Out_Tok) in Pp_Off_Comment | End_Of_Input;
      --  This is called just after copying Spaces from Out_Buf, and just
      --  before copying Pp_Off_Comment from Src_Buf. This appends additional
      --  spaces to New_Buf so the Pp_Off_Comment will be indented as it was in
      --  Src_Buf. We don't need any such adjustment for Pp_On_Comment, because
      --  indentation preceding those is copied from Src_Buf.

      procedure Get_Next_Off_On
        (Tok    : in out Tokn_Cursor;
         Expect : Pp_Off_On_Comment) is
      begin
         loop
            Next (Tok);
            Error_Sloc :=
              (if Kind (Tok) in End_Of_Input then
                 Slocs.No_Source_Location
               else To_Langkit (Scanner.Sloc (Tok)));
            exit when Kind (Tok) in Expect | End_Of_Input;
         end loop;
         pragma Assert (Kind (Tok) in Expect | End_Of_Input);
         pragma Assert
           (Kind (Prev_ss (Tok)) in Start_Of_Input | EOL_Token);
      end Get_Next_Off_On;

      procedure Copy (Buf : in out Buffer; Up_To : Positive) is
      begin
         while not At_Point (Buf, Up_To) loop
            Insert_Any (New_Buf, Cur (Buf));
            Move_Forward (Buf);
         end loop;
      end Copy;

      procedure Skip (Buf : in out Buffer; Up_To : Positive) is
      begin
         while not At_Point (Buf, Up_To) loop
            Move_Forward (Buf);
         end loop;
      end Skip;

      procedure Indent_Pp_Off (Src_Tok, Out_Tok : Tokn_Cursor) is
      begin
         if Kind (Out_Tok) = Pp_Off_Comment then
            Insert (New_Buf,
                    [Sloc_Col (Out_Tok) .. Sloc_Col (Src_Tok) - 1 => ' ']);
         end if;
      end Indent_Pp_Off;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns,
         Comments_Special_On => Arg (Cmd, Comments_Special));

      Src_Tok : Tokn_Cursor;
      Out_Tok : Tokn_Cursor := First (Out_Tokns'Access);

   --  Start of processing for Copy_Pp_Off_Regions_Helper

   begin
      Clear (Src_Tokns);

      --  UA21-003 : Fill Src_Buf with Input's content, including the trailing
      --  spaces since trailing spaces in pp off regions should be kept.

      Insert_Ada_Source
        (Buf                     => Src_Buf,
         Input                   =>
           Utils.Char_Vectors.Char_Vectors.Elems
             (Input) (1 .. Utils.Char_Vectors.Char_Vectors.Last_Index (Input)),
         Wide_Character_Encoding =>
           Utils.Command_Lines.Common.Wide_Character_Encoding (Cmd),
         Expand_Tabs             => True,
         Include_Trailing_Spaces => True);

      --  The Src_Buf contains a sequence of zero or more OFF and ON
      --  commands. The first must be OFF, then ON, then OFF and so on,
      --  alternating. If that weren't true, we would have gotten an error in
      --  Insert_Comments_And_Blank_Lines, in which case we don't get here.
      --  The final End_Of_Input acts as an ON or OFF as appropriate.
      --  The Out_Buf contains a corresponding sequence with the same
      --  number of OFF's and ON's.

      --  Pretty printing is ON between the beginning and the first OFF, then
      --  OFF until the next ON, and so on.

      Reset (Src_Buf);

      --  Create a token vector out of the source buffer. This vector will
      --  include trailing Space tokens.

      Ignored :=
        Get_Tokns
          (Input               => Src_Buf,
           Result              => Src_Tokns,
           Comments_Special_On => Arg (Cmd, Comments_Special));

      Src_Tok := First (Src_Tokns'Access);

      --  When we see an OFF, we want to copy/ignore starting at the
      --  beginning of the line on which the OFF appears, which is the
      --  Prev. For an ON, we ignore the Prev.

      if Debug_Mode then
         Dbg_Out.Put ("Copy_Pp_Off_Regions: Src_Tokns:\n");
         Put_Tokens (Src_Tokns);
         Dbg_Out.Put ("end Src_Tokns:\n");
         Dbg_Out.Put ("Copy_Pp_Off_Regions: Out_Tokns:\n");
         Put_Tokens (Out_Tokns);
         Dbg_Out.Put ("end Out_Tokns:\n");
      end if;

      --  The following loop repeatedly copies an ON region from Out_Buf to
      --  New_Buf (ignoring the corresponding region of Src_Buf), then copies
      --  an OFF region from Src_Buf to New_Buf (ignoring the corresponding
      --  region of Out_Buf).

      loop
         Get_Next_Off_On (Out_Tok, Expect => Pp_Off_Comment);
         Get_Next_Off_On (Src_Tok, Expect => Pp_Off_Comment);
         Copy (Out_Buf, Up_To => Next_Sloc_First (Prev (Out_Tok)));
         Skip (Src_Buf, Up_To => Next_Sloc_First (Prev (Src_Tok)));

         exit when Kind (Src_Tok) = End_Of_Input;

         Indent_Pp_Off (Src_Tok, Out_Tok);

         Get_Next_Off_On (Out_Tok, Expect => Pp_On_Comment);
         Get_Next_Off_On (Src_Tok, Expect => Pp_On_Comment);
         Copy (Src_Buf, Up_To => Next_Sloc_First (Prev (Src_Tok)));
         Skip (Out_Buf, Up_To => Next_Sloc_First (Prev (Out_Tok)));

         --  Make sure we reach the end of both token vector at the same time
         pragma Assert
           ((Kind (Out_Tok) = End_Of_Input) = (Kind (Src_Tok) = End_Of_Input));

         exit when Kind (Src_Tok) = End_Of_Input;
      end loop;

      Reset (Src_Buf);
      Reset (Out_Buf);
      Reset (New_Buf);
      Clear (Out_Tokns);

      Move (Target => Out_Buf, Source => New_Buf);
   end Copy_Pp_Off_Regions_Helper;

   package Tok_Phases is
      --  ???The plan is to implement the new phases here, and get rid of
      --  Tok_Phases once it's all working.

      procedure Split_Lines
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         First_Time : Boolean;
         Partial_Gnatpp : Boolean := False);
      --  Enable soft line breaks as necessary to prevent too-long lines.
      --  First_Time is for debugging.

      procedure Enable_Line_Breaks_For_EOL_Comments
        (Src_Buf : in out Buffer;
         Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False);
      --  For all end-of-line comments that occur at a soft line break, enable
      --  the line break. Note that this does not modify the output.

      procedure Insert_Comments_And_Blank_Lines
        (Src_Buf : in out Buffer;
         Messages : out Scanner.Source_Message_Vector;
         Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Pp_Off_Present : in out Boolean;
         Partial_Gnatpp : Boolean := False);
      --  New_Tokns doesn't contain any comments; they are inserted into the
      --  output from Src_Tokns. Blank lines are also copied from Src_Tokns to
      --  New_Tokns. The output is also patched up in miscellaneous other ways,
      --  such as inserting preprocessor directives (see comments in the body
      --  for details).
      --
      --  This procedure also does some work in preparation for
      --  Copy_Pp_Off_Regions. In particular, it checks that OFF/ON commands
      --  are in the proper sequence, and it sets the Pp_Off_Present flag.

      procedure Insert_Indentation (Lines_Data_P : Lines_Data_Ptr;
                                    Cmd : Command_Line;
                                    Partial_Gnatpp : Boolean := False);

      procedure Insert_Alignment
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False);

      --  Expand tabs as necessary to align things

   end Tok_Phases;

   procedure Post_Tree_Phases
     (Input          : Char_Vector;
      Lines_Data_P   : Lines_Data_Ptr;
      Messages       : out Scanner.Source_Message_Vector;
      Src_Buf        : in out Buffer;
      Cmd            : Command_Line;
      Partial        : Boolean;
      Partial_Gnatpp : Boolean := False)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;

      Pp_Off_Present : Boolean := False;
      --  True if there is at least one Pp_Off_Comment. We don't care about
      --  Pp_On_Comments, because it's an error to have a Pp_On_Comment
      --  without a preceding Pp_Off_Comment. Set True if appropriate by
      --  Insert_Comments_And_Blank_Lines. This allows us to skip the
      --  Copy_Pp_Off_Regions pass as an optimization.

   begin
      --  ????We probably want to do more, but this is for gnatstub
      --  for now.
      if Partial and then not Partial_Gnatpp then
         Clear (All_LB);
         Clear (All_LBI);
         Clear (Lines_Data.Tabs);
         Tokns_To_Buffer (Lines_Data.Out_Buf, Lines_Data.New_Tokns, Cmd);
         return;
      end if;

      if not Partial_Gnatpp then
         Tok_Phases.Split_Lines
           (Lines_Data_P, Cmd,
            First_Time => True);
         Tok_Phases.Enable_Line_Breaks_For_EOL_Comments
           (Src_Buf, Lines_Data_P, Cmd);
         Tok_Phases.Insert_Comments_And_Blank_Lines
           (Src_Buf, Messages, Lines_Data_P,
            Cmd, Pp_Off_Present);
         Tok_Phases.Split_Lines
           (Lines_Data_P, Cmd,
            First_Time => False);
         Tok_Phases.Insert_Indentation (Lines_Data_P, Cmd);
         Tok_Phases.Insert_Alignment (Lines_Data_P, Cmd);

         Tokns_To_Buffer (Lines_Data.Out_Buf, Lines_Data.New_Tokns, Cmd);

         Keyword_Casing (Lines_Data_P, Cmd);
         Insert_Form_Feeds (Lines_Data_P, Cmd);
         Copy_Pp_Off_Regions (Input, Lines_Data_P, Pp_Off_Present, Cmd);

         --  The following pass doesn't modify anything; it just checks that
         --  the sequence of tokens we have constructed matches the original
         --  source code (with some allowed exceptions).

         Final_Check (Lines_Data_P, Src_Buf, Cmd, Pp_Off_Present);

      else
         --  Actions only in partial gnatpp mode. Final_Check is omitted
         --  since applying for the entire file for now.

         Tok_Phases.Split_Lines
           (Lines_Data_P, Cmd,
            First_Time => True,
            Partial_Gnatpp => Partial_Gnatpp);

         Tok_Phases.Enable_Line_Breaks_For_EOL_Comments
           (Src_Buf, Lines_Data_P, Cmd, Partial_Gnatpp);

         Tok_Phases.Insert_Comments_And_Blank_Lines
           (Src_Buf, Messages, Lines_Data_P, Cmd,
            Pp_Off_Present,
            Partial_Gnatpp);

         Tok_Phases.Split_Lines
           (Lines_Data_P, Cmd,
            First_Time => False,
            Partial_Gnatpp => Partial_Gnatpp);

         Tok_Phases.Insert_Indentation (Lines_Data_P, Cmd, Partial_Gnatpp);

         Tok_Phases.Insert_Alignment (Lines_Data_P, Cmd, Partial_Gnatpp);

         Tokns_To_Buffer (Lines_Data.Out_Buf, Lines_Data.New_Tokns, Cmd);

         Keyword_Casing (Lines_Data_P, Cmd);
      end if;

   exception
      when Post_Tree_Phases_Done => null;
   end Post_Tree_Phases;

   procedure Raise_Token_Mismatch
     (Message              : String;
      Lines_Data           : Lines_Data_Rec;
      Src_Buf              : Buffer;
      Src_Tok, Out_Tok     : Scanner.Tokn_Cursor)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;

      use Scanner;
      subtype Q is Opt_Token_Kind with
        Predicate => Q in Nil | Spaces | Start_Of_Input | End_Of_Input |
                          EOL_Token | Line_Break_Token;

      function Txt (Tok : Tokn_Cursor) return String is
        (if Kind (Tok) in Q then "``" & Str (Text (Tok)).S & "''"
         else Str (Text (Tok)).S);
      --  Text to print. Put quotes around some kinds.

   --  Start of processing for Raise_Token_Mismatch

   begin
      Error_Sloc := To_Langkit (Sloc (Src_Tok));

      if Enable_Token_Mismatch then
         Utils.Dbg_Out.Output_Enabled := True;
         Err_Out.Put ("Src_Buf:\n");
         Dump_Buf (Src_Buf);
         Err_Out.Put ("Out_Buf:\n");
         Dump_Buf (Out_Buf);

         Err_Out.Put
           ("\1: Token mismatch: \2 --> \3\n",
            Message, Txt (Src_Tok), Txt (Out_Tok));
         Err_Out.Put ("Src tokens:\n");
         Put_Tokens (Highlight => Src_Tok);
         Err_Out.Put ("========================================\n");
         Err_Out.Put ("Out tokens:\n");
         Put_Tokens (Highlight => Out_Tok);
      end if;

      raise Token_Mismatch;
   end Raise_Token_Mismatch;

   ------------------------
   -- Final_Check_Helper --
   ------------------------

   procedure Final_Check_Helper
     (Lines_Data_P : Lines_Data_Ptr;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;

      use Scanner;

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
      --  Similar to Match in Insert_Comments_And_Blank_Lines, but here we need
      --  to deal with comments.

      procedure Collect_Comments
        (Tok : in out Tokn_Cursor; Result : in out WChar_Vector);
      --  Collect up all the text of a sequence of Whole_Line_Comments,
      --  ignoring changes made by paragraph filling. Paragraph_Filling might
      --  have changed blank to NL and vice versa, and it turns a series of
      --  blanks into a single one. Similarly needed if Comments_Gnat_Beginning
      --  is True.

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean is
      begin
         if Arg (Cmd, Spaces_Only) then
            return Kind (Src_Tok) = Kind (Out_Tok)
              and then Case_Insensitive_Equal (Text (Src_Tok), Text (Out_Tok));
         end if;

         return R : Boolean do
            if Kind (Src_Tok) = Kind (Out_Tok) then
               case Kind (Src_Tok) is
                  when EOL_Token | Spaces | Line_Break_Token =>
                     raise Program_Error;

                  when Start_Of_Input | End_Of_Input |
                    Tab_Token | Other_Lexeme | Illegal_Character =>
                     pragma Assert
                       (Equal_Ignoring_CR (Text (Src_Tok), Text (Out_Tok)));
                     R := True;

                  when Reserved_Word =>
                     pragma Assert
                       (Case_Insensitive_Equal
                          (Text (Src_Tok), Text (Out_Tok)));
                     R := True;

                  when Ident =>
                     R := Case_Insensitive_Equal
                       (Text (Src_Tok), Text (Out_Tok));

                  when Numeric_Literal =>
                     R := Num_Lits_Match (Src_Tok, Out_Tok, Cmd);

                  when Character_Literal =>
                     R := Text (Src_Tok) = Text (Out_Tok);

                  when String_Lit =>
                     if Is_Op_Sym_With_Letters (Text (Src_Tok)) then
                        R := Case_Insensitive_Equal
                          (Text (Src_Tok), Text (Out_Tok));
                     else
                        R := Text (Src_Tok) = Text (Out_Tok);
                     end if;

                  when Preprocessor_Directive =>
                     R := Text (Src_Tok) = Text (Out_Tok);

                  when Comment_Kind =>
                     R :=
                       (Arg (Cmd, Comments_Gnat_Beginning)
                        or else Leading_Blanks (Src_Tok) =
                                Leading_Blanks (Out_Tok))
                       and then Text (Src_Tok) = Text (Out_Tok);
               end case;

            elsif Kind (Src_Tok) in Reserved_Word_New
              and then Kind (Out_Tok) = Ident
            then
               R := Case_Insensitive_Equal (Text (Src_Tok), Text (Out_Tok));

            elsif Kind (Src_Tok) = End_Of_Line_Comment
              and then Kind (Out_Tok) in Whole_Line_Comment
            then
               R := Text (Src_Tok) = Text (Out_Tok)
                 and then
                 (if
                    not Arg (Cmd, Comments_Gnat_Beginning)
                  then
                    Leading_Blanks (Src_Tok) = Leading_Blanks (Out_Tok));
               --  ???This case will be needed if/when we turn end-of-line
               --  comments that don't fit into whole-line comments. That
               --  transformation seems questionable, because it would
               --  damage idempotency: first run of gnatpp turns an
               --  end-of-line comment into a whole-line-comment, and then a
               --  second run considers it part of a comment paragraph and
               --  fills it.
            else
               R := False;
            end if;
         end return;
      end Match;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns,
         Comments_Special_On => Arg (Cmd, Comments_Special));

      Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
      Out_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      --  Cursors into Src_Tokns and Out_Tokns, respectively. Skip the
      --  first Start_Of_Input token, which is just a sentinel.

      procedure Collect_Comments
        (Tok    : in out Tokn_Cursor;
         Result : in out WChar_Vector)
      is
      begin
         while Kind (Tok) in Whole_Line_Comment loop
            declare
               Text : constant W_Str := To_W_Str (Scanner.Text (Tok));
               function White (X : Positive) return Boolean is
                 (X <= Text'Last
                  and then
                  (Is_Space (Text (X)) or else Is_Line_Terminator (Text (X))));
               --  True if X points to a space or NL character

               pragma Assert
                 (Text'First = 1
                  and then Text'Last >= 1
                  and then (if Text'Last > 1 then not White (1))
                  and then White (Text'Last));
               X : Positive := 1;
            begin
               while X <= Text'Last loop
                  if White (X) then
                     Append (Result, ' ');
                     while White (X) loop
                        X := X + 1;
                     end loop;
                  else
                     Append (Result, Text (X));
                     X := X + 1;
                  end if;
               end loop;
            end;

            loop
               Next_ss (Tok);

               exit when Kind (Tok) not in EOL_Token;
            end loop;
         end loop;
      end Collect_Comments;

   --  Start of processing for Final_Check_Helper

   begin
      --  Skip initial EOL_Token token
      pragma Assert (Kind (Out_Tok) in EOL_Token);
      Next_ss (Out_Tok);

      --  This loop is similar to the one in
      --  Insert_Comments_And_Blank_Lines; see that for commentary.

      loop
         Error_Sloc := To_Langkit (Scanner.Sloc (Src_Tok));

         if Simulate_Token_Mismatch and then Get_Tokn_Index (Src_Tok) > 6 then
            --  Simulate a token mismatch, for testing
            Raise_Token_Mismatch
              ("Final_Check 0", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
         end if;

         --  Spaces_Only requires an exact match, except for Spaces

         if Arg (Cmd, Spaces_Only) then
            if Match (Src_Tok, Out_Tok) then
               exit when Kind (Src_Tok) = End_Of_Input;
               --  i.e. exit when both Src and Out are at end of input

               Next (Src_Tok);
               Next (Out_Tok);

               while Kind (Src_Tok) = Spaces loop
                  Next (Src_Tok);
               end loop;

               while Kind (Out_Tok) = Spaces loop
                  Next (Out_Tok);
               end loop;

            else
               Raise_Token_Mismatch
                 ("Final_Check 1", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
            end if;

         else
            if Kind (Src_Tok) not in EOL_Token
              and then
                (Match (Src_Tok, Out_Tok)
                 or else (Kind (Src_Tok) = '!' and then Kind (Out_Tok) = '|'))
            then
               exit when Kind (Src_Tok) = End_Of_Input;
               --  i.e. exit when both Src and Out are at end of input

               Next_ss (Src_Tok);
               Next_ss (Out_Tok);

            else
               --  If we're filling comments, then the comments might not match
               --  up. For example, a line break could be added such that the
               --  first line is too short to be considered part of a fillable
               --  comment paragraph, thus turning one comment into two. So we
               --  collect them all together and check that their text
               --  more-or-less matches.
               --
               --  Similarly, we do this if Comments_Gnat_Beginning. For
               --  example, if one comment starts with a single blank and the
               --  next starts with two, then they will not look like a single
               --  paragraph during Insert_Comments_And_Blank_Lines, but here
               --  they will, because an extra blank has been added to the
               --  first.
               --
               --  Actually, we need to do this in any case: if two comments in
               --  the input are not indented the same, they will be indented
               --  the same in the output, and thus appear to be a fillable
               --  paragraph.

               if Kind (Src_Tok) in Whole_Line_Comment
                 and then Kind (Out_Tok) in Whole_Line_Comment
               then
                  declare
                     Src_Comments : WChar_Vector;
                     Out_Comments : WChar_Vector;
                  begin
                     Collect_Comments (Src_Tok, Src_Comments);
                     Collect_Comments (Out_Tok, Out_Comments);

                     if Src_Comments /= Out_Comments then
                        if Enable_Token_Mismatch then
                           Err_Out.Put
                             ("\1 --> \2\n",
                              To_UTF8 (To_Array (Src_Comments)),
                              To_UTF8 (To_Array (Out_Comments)));
                        end if;

                        Raise_Token_Mismatch
                          ("Final_Check 2",
                           Lines_Data, Src_Buf, Src_Tok, Out_Tok);
                     end if;
                  end;

               --  Check for "end;" --> "end Some_Name;" case

               elsif Kind (Src_Tok) = ';'
                 and then
                   Kind (Prev_Lexeme (Src_Tok)) = Res_End
                 and then Sname_83 (Out_Tok)
               then
                  loop -- could be "end A.B.C;"
                     Next_ssnl (Out_Tok);

                     exit when Kind (Out_Tok) /= '.';

                     Next_ssnl (Out_Tok);

                     if not Sname_83 (Out_Tok) then
                        Raise_Token_Mismatch
                          ("Final_Check 3",
                           Lines_Data, Src_Buf, Src_Tok, Out_Tok);
                     end if;
                  end loop;
                  if Kind (Out_Tok) /= ';' then
                     Raise_Token_Mismatch
                       ("Final_Check 4",
                        Lines_Data, Src_Buf, Src_Tok, Out_Tok);
                  end if;

               elsif Kind (Src_Tok) in EOL_Token then
                  Next_ss (Src_Tok);

               elsif Kind (Out_Tok) in EOL_Token then
                  Next_ss (Out_Tok);

               --  Else print out debugging information and crash. This avoids
               --  damaging the source code in case of bugs.

               else
                  Raise_Token_Mismatch
                    ("Final_Check 5", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
               end if;
            end if;
         end if;
      end loop;

      pragma Assert (At_Beginning (Out_Buf));
      --  We didn't touch Out_Buf, other than to get the tokens from it

      if not At_Last (Src_Tok) or else not At_Last (Out_Tok) then
         Raise_Token_Mismatch
           ("Final_Check 6", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
      end if;
      Clear (Out_Tokns);
   end Final_Check_Helper;

   -----------------
   -- Final_Check --
   -----------------

   procedure Final_Check
     (Lines_Data_P   : Lines_Data_Ptr;
      Src_Buf        : in out Buffer;
      Cmd            : Utils.Command_Lines.Command_Line;
      Pp_Off_Present : Boolean)
   is
      Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
   begin
      if Disable_Final_Check then
         return;
      end if;

      --  In Dev mode, we have Enable_Token_Mismatch, which prints debugging
      --  information on a mismatch.

      if Enable_Token_Mismatch then
         declare
            Old_Out_Buf : constant WChar_Vector :=
              To_Vector (Lines_Data.Out_Buf);
         begin
            Final_Check_Helper (Lines_Data_P, Src_Buf, Cmd);

            declare
               Out_Buf : constant WChar_Vector :=
                 To_Vector (Lines_Data.Out_Buf);
            begin
               pragma Assert (Out_Buf = Old_Out_Buf);

               --  There might exist trailing spaces in pp off regions
               if not Pp_Off_Present then
                  pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));
               end if;
            end;
         end;

      --  In production mode, we don't print debugging information, but we
      --  still raise the exception.

      else
         Final_Check_Helper (Lines_Data_P, Src_Buf, Cmd);
      end if;
   end Final_Check;

   ----------------

   --  Debugging:

   package Enable_Dbg_Out is
      --  Declaring an object of type Enable_Dbg_Out causes Dbg_Out to be
      --  enabled, and then restored to its previous state on finalization.

      use Ada.Finalization;
      type Enable_Dbg_Out is new Limited_Controlled with record
         Old : Boolean;
      end record;

      procedure Initialize (X : in out Enable_Dbg_Out);
      procedure Finalize (X : in out Enable_Dbg_Out);
   end Enable_Dbg_Out;

   package body Enable_Dbg_Out is

      procedure Initialize (X : in out Enable_Dbg_Out) is
      begin
         X.Old := Utils.Dbg_Out.Output_Enabled;
         Utils.Dbg_Out.Output_Enabled := True;
      end Initialize;

      procedure Finalize (X : in out Enable_Dbg_Out) is
      begin
         Utils.Dbg_Out.Output_Enabled := X.Old;
      end Finalize;

   end Enable_Dbg_Out;

   procedure Put_All_Tokens (Message : String; Lines_Data : Lines_Data_Rec) is
      use Scanner, Dbg_Out;
      Dummy : Enable_Dbg_Out.Enable_Dbg_Out;
   begin
      if Debug_Flag_4 then
         Dbg_Out.Output_Enabled := True;
         Put ("\n\1\n", Message);
         Put ("Src_Tokns:\n");
         Put_Tokens (Lines_Data.Src_Tokns);
         Put ("New_Tokns:\n");
         Put_Tokens (Lines_Data.New_Tokns);
         Dbg_Out.Output_Enabled := False;
      end if;

      pragma Assert (Is_Empty (Lines_Data.Out_Tokns));
      pragma Assert (Is_Empty (Lines_Data.Saved_New_Tokns));
   end Put_All_Tokens;

   function Line_Text
     (Lines_Data : Lines_Data_Rec;
      F, L : Line_Break_Index_Index) return W_Str
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      First  : constant Line_Break := All_LB (All_LBI (F));
      Last   : constant Line_Break := All_LB (All_LBI (L));
      use Scanner;
      Result : constant W_Str      :=
        Slice (Out_Buf, Sloc_First (First.Tok), Sloc_First (Last.Tok));
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Line_Text;

   function Tab_Image (Tabs : Tab_Vector; X : Tab_Index) return String is
      Tab : constant Tab_Rec := Tabs (X);
   begin
      return "Tabs(" &
        Image (Integer (X)) &
        ") = ^" &
        Image (Integer (Tab.Index_In_Line)) &
        Str (Tab.Token).S &
        ASCII.HT &
        " at " &
        (if Tab.Col = Positive'Last
           then ""
           else " Col = " & Image (Tab.Col)) &
        (if Tab.Num_Blanks = 0 then ""
         else " Blanks = " & Image (Tab.Num_Blanks)) &
        (if Tab.Is_Fake then " FAKE" else "") &
        (if Is_Null (Tab.Tree)
           then ""
           else "(Tr = " & T_Img (Tab.Tree) & ")") &
        (if Is_Null (Tab.Parent) then ""
         else "(Pa = " & T_Img (Tab.Parent) & ")");
   end Tab_Image;

   procedure Put_Line_Break (Break : Line_Break) is
      Dummy : Enable_Dbg_Out.Enable_Dbg_Out;
      use Dbg_Out;
   begin
      Put
        ("\1, \2, \3",
         String'(1 .. Break.Indentation => '_'),
         (if Break.Hard then "hard" else "soft"),
         (if Break.Enabled then "enabled" else "disabled"));
      Scanner.Put_Token (Break.Tok);
   end Put_Line_Break;

   procedure Put_LBs (LB_Vec : Line_Break_Vector) is
      Dummy : Enable_Dbg_Out.Enable_Dbg_Out;
      use Dbg_Out;
   begin
      if Is_Empty (LB_Vec) then
         Put ("empty Line_Break_Vector\n");
      end if;
      for LBI in 1 .. Last_Index (LB_Vec) loop
         Put ("\1 => ", Image (Integer (LBI)));
         Put_Line_Break (LB_Vec (LBI));
      end loop;
   end Put_LBs;

   procedure Put_LBIs (LBI_Vec : Line_Break_Index_Vector) is
      Dummy : Enable_Dbg_Out.Enable_Dbg_Out;
      use Dbg_Out;
   begin
      if Is_Empty (LBI_Vec) then
         Put ("empty Line_Break_Index_Vector\n");
      end if;
      for LBII in 1 .. Last_Index (LBI_Vec) loop
         declare
            LBI : constant Line_Break_Index := LBI_Vec (LBII);
         begin
            Put ("\1 => \2\n",
                 Image (Integer (LBII)), Image (Integer (LBI)));
         end;
      end loop;
   end Put_LBIs;

   procedure Put_Line_Breaks (Lines_Data : Lines_Data_Rec) is
      Dummy : Enable_Dbg_Out.Enable_Dbg_Out;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      L        : Line_Break_Index_Index;
      Line_Num : Natural := 0; -- only counts enabled lines
      use Dbg_Out;
   begin
      Put
        ("Last_Index (All_LBI) = \1\n",
         Image (Integer (Last_Index (All_LBI))));

      for Cur_Line in 1 .. Last_Index (All_LBI) loop
         if All_LB (All_LBI (Cur_Line)).Enabled then
            Line_Num := Line_Num + 1;
         end if;

         Put
           ("\1:\t\2\3",
            Image (Line_Num),
            String'(1 .. All_LB (All_LBI (Cur_Line)).Indentation => '_'),
            (if All_LB (All_LBI (Cur_Line)).Enabled then "" else "?"));

         Put (" lev=\1", Image (Integer (All_LB (All_LBI (Cur_Line)).Level)));

--         if False then
--            Put ("\t\1", Image (All_LB (All_LBI (Cur_Line)).Kind));
--         end if;

         if All_LB (All_LBI (Cur_Line)).Enabled
           and then Cur_Line /= Last_Index (All_LBI)
         then
            L := Next_Enabled (Lines_Data, Cur_Line);
            Put
              ("\t\1..\2 len=\3",
               Image (Integer (Cur_Line)),
               Image (Integer (L)),
               Image (All_LB (All_LBI (Cur_Line)).Length));
            --  ???Line_Text doesn't work, because Out_Buf isn't set yet, so
            --  disable this for now. It would be better to reconstruct the
            --  text from the tokens.
            if False then
               Put ("\t<<\1>>", To_UTF8 (Line_Text (Lines_Data, Cur_Line, L)));
            end if;
         end if;

         Put ("\n");
      end loop;
      for Cur_Line in 1 .. Last_Index (All_LBI) loop
         Put_Line_Break (All_LB (All_LBI (Cur_Line)));
      end loop;
   end Put_Line_Breaks;

   procedure Format_Debug_Output
     (Lines_Data : Lines_Data_Rec; Message : String)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Tabs : Tab_Vector renames Lines_Data.Tabs;

      use Dbg_Out;
   begin
      if not Dbg_Out.Output_Enabled then
         return;
      end if;

      Text_IO.Flush (Text_IO.Standard_Output);
      Text_IO.Flush (Text_IO.Standard_Error);

      Put ("\n\nFormat_Debug_Output: \1:\n", Message);

      Dump_Buf (Out_Buf);

      Put_Line_Breaks (Lines_Data);

      for X in 1 .. Last_Index (Tabs) loop
         Put ("\1\n", Tab_Image (Tabs, X));
      end loop;

      Text_IO.Flush (Text_IO.Standard_Error);
      Text_IO.Flush (Text_IO.Standard_Output);
   end Format_Debug_Output;

   procedure Assert_No_LB (Lines_Data : Lines_Data_Rec) is
   begin
      pragma Assert (Is_Empty (Lines_Data.All_LB));
      pragma Assert (Is_Empty (Lines_Data.All_LBI));
      pragma Assert (Is_Empty (Lines_Data.Temp_LBI));
      pragma Assert (Is_Empty (Lines_Data.Enabled_LBI));
      pragma Assert (Is_Empty (Lines_Data.Syntax_LBI));
   end Assert_No_LB;

   procedure Put_Char_Vector (Container : Char_Vector) is
      procedure Put_Between is null;
   begin
      Utils.Char_Vectors.Char_Vectors.Put
        (Container, Utils.Formatted_Output.Put_Char'Access,
         Put_Between'Access);
   end Put_Char_Vector;

   package Wide_Formatted_Output is new Utils.Generic_Formatted_Output
     (Char_Type => W_Char,
      Str_Type => W_Str,
      Basic_Put_Char => Wide_Text_IO_Put_Char);

   procedure Put_WChar_Vector (Container : WChar_Vector) is
      procedure Put_Between is null;
      use Wide_Formatted_Output;
   begin
      Put (Container, Put_Char'Access, Put_Between'Access);
   end Put_WChar_Vector;

   function LB_Tok (LB : Line_Break) return Scanner.Tokn_Cursor is
      use Scanner;
   begin
      pragma Assert (Kind (LB.Tok) in Line_Break_Token);
      pragma Assert (Token_At_Cursor (LB.Tok) = LB.Tokn_Val);
      return LB.Tok;
   end LB_Tok;

   package body Tok_Phases is
      pragma Style_Checks ("M82"); -- because these will eventually be unnested

      use Scanner, Scanner.Lines;

      function Line_Len
        (All_LB : Line_Break_Vector; F, L : Line_Break_Index)
        return Natural;
      --  F and L are the first and last index forming a line; returns the
      --  length of the line, not counting new-lines. F must be enabled.

      function Forms_Blank_Line
        (All_LB : Line_Break_Vector;
         F, L : Line_Break_Index) return Boolean;
      --  True if F..L forms an empty line (or would, if both were enabled).

      procedure Collect_Line_Breaks
        (Lines_Data_P : Lines_Data_Ptr;
         Tokns : in out Scanner.Tokn_Vec;
         Do_All, Do_Enabled, Do_Syntax, First_Time : Boolean);
      --  Collect line breaks from New_Tokns, ignoring soft line breaks that
      --  would form blank lines. These ignored ones are also removed from
      --  New_Tokns. Set the Tok of each line break to point to the
      --  corresponding line break token in Tokns. Note that these will become
      --  invalid as soon as Tokns is modified. Set All_LBI, Enabled_LBI, and
      --  Syntax_LBI, as appropriate.

      procedure Erase_LB_Toks (All_LB : in out Line_Break_Vector);
      --  Set the Tok of each line break to Nil

      function Forms_Blank_Line
        (All_LB : Line_Break_Vector;
         F, L : Line_Break_Index) return Boolean
      is
         First : Line_Break renames All_LB (F);
         Last  : Line_Break renames All_LB (L);
         FP    : Positive            := Sloc_First (LB_Tok (First));
         LP    : constant Positive   := Sloc_First (LB_Tok (Last));
      begin
         --  Take into account the fact that a hard line break occupies one
         --  character (the NL), whereas a soft line break does not, and the fact
         --  that a soft line break can be preceded or followed by a single blank
         --  (but not both).

         if First.Hard then
            FP := FP + 1;
         end if;
         if FP < LP and then Kind (Next (LB_Tok (First))) = Spaces then
            pragma Assert (Text (Next (LB_Tok (First))) = Name_Space);
            FP := FP + 1;
         end if;

         pragma Assert (FP <= LP);
         return FP = LP;
      end Forms_Blank_Line;

      procedure Collect_Line_Breaks
        (Lines_Data_P : Lines_Data_Ptr;
         Tokns : in out Scanner.Tokn_Vec;
         Do_All, Do_Enabled, Do_Syntax, First_Time : Boolean)
      is
         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         All_LB : Line_Break_Vector renames Lines_Data.All_LB;
         All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
         Enabled_LBI : Line_Break_Index_Vector renames Lines_Data.Enabled_LBI;
         Syntax_LBI : Line_Break_Index_Vector renames Lines_Data.Syntax_LBI;
         Saved_New_Tokns : aliased Scanner.Tokn_Vec;
         Ignore : Boolean := Move_Tokns
           (Target => Saved_New_Tokns, Source => Tokns);
         pragma Assert (not Is_Empty (Saved_New_Tokns));
         P : Tokn_Cursor := First (Saved_New_Tokns'Unchecked_Access);
         --  token preceding Tok
         pragma Assert (Kind (P) = Start_Of_Input);
         Tok : Tokn_Cursor := Next (P);
      begin
         Clear (All_LBI);
         pragma Assert (Is_Empty (Lines_Data.Temp_LBI));
         Clear (Enabled_LBI);
         Clear (Syntax_LBI);

         while not After_Last (Tok) loop
            if Kind (P) in Line_Break_Token then
               pragma Assert
                 (not (Kind (Prev (Tok)) = Spaces and then Kind (Tok) = Spaces));
               declare
                  LBI : constant Line_Break_Index :=
                    Line_Break_Token_Index (P);
                  LB : Line_Break renames All_LB (LBI);
               begin
                  Append_Tokn (Tokns, P);
                  LB.Tok := Last (Tokns'Unrestricted_Access);
                  LB.Tokn_Val := Token_At_Cursor (LB.Tok);

                  --  We don't want soft line breaks to form blank lines, so
                  --  we discard them if they would. The first line break is
                  --  enabled, so we're not calling Last_Element on an empty
                  --  All_LBI.

                  if LB.Enabled
                    or else not First_Time
                    or else not Forms_Blank_Line
                      (All_LB, Last_Element (All_LBI), LBI)
                  then
                     if Do_All then
                        Append (All_LBI, LBI);
                     end if;

                     if LB.Enabled then
                        if All_LB (LBI).Hard
                          and then (All_LB (LBI).Length /= 0
                                      or else LBI = Last_Index (All_LB))
                        then
                           if Do_Syntax then
                              Append (Syntax_LBI, LBI);
                           end if;
                        end if;
                        if Do_Enabled then
                           Append (Enabled_LBI, LBI);
                        end if;
                     end if;
                  else
                     Delete_Last (Tokns);
                     --  It was appended above
                  end if;
               end;
            else
               Append_Tokn (Tokns, P);
            end if;

            P := Tok;
            Next (Tok);
         end loop;

         pragma Assert (Kind (P) = End_Of_Input);
         Append_Tokn (Tokns, P);

         pragma Debug (Check_Same_Tokens
           (Tokns, Saved_New_Tokns,
            "Collect_Line_Breaks", "Tokns", "Saved_New_Tokns"));
      end Collect_Line_Breaks;

      procedure Erase_LB_Toks (All_LB : in out Line_Break_Vector) is
      begin
         if not Assert_Enabled then
            return;
         end if;

         for LB of All_LB loop
            LB.Tok := Nil_Tokn_Cursor;
         end loop;
      end Erase_LB_Toks;

      function Line_Len
        (All_LB : Line_Break_Vector; F, L : Line_Break_Index)
        return Natural
      is
         pragma Assert (All_LB (F).Enabled);
         First : Line_Break renames All_LB (F);
         Last  : Line_Break renames All_LB (L);
         F_Pos : constant Natural := Sloc_First (Next (LB_Tok (First)));
         L_Pos : constant Natural := Sloc_First (LB_Tok (Last));

         Leading_Blank : constant Natural :=
           (if not First.Hard
              and then L_Pos > F_Pos + 1
              and then Kind (Next (LB_Tok (First))) = Spaces
            then 1
            else 0);
         --  The test for First.Hard above is needed in the annoying
         --  Extra_Blank_On_Return case, among others.
         Trailing_Blank : constant Natural :=
           (if L_Pos > F_Pos + 2 and then Kind (Prev (LB_Tok (Last))) = Spaces
            then 1
            else 0);
         Without_Indent : constant Natural :=
             L_Pos - F_Pos - Leading_Blank - Trailing_Blank;
         --  The length without the indentation is just the difference between
         --  the positions, except that if the first or last character is ' '
         --  adjacent to a soft line break, it doesn't count.

      begin
         --  If the line is blank, we ignore the indentation; we won't be
         --  putting blanks in the output. Otherwise, the length is the
         --  indentation plus the length without the indentation as
         --  calculated above.

         return
           (if Without_Indent = 0 then 0
            else First.Indentation + Without_Indent);
      end Line_Len;

      procedure Split_Lines
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         First_Time : Boolean;
         Partial_Gnatpp : Boolean := False)
      is
         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         All_LB : Line_Break_Vector renames Lines_Data.All_LB;
         All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
         New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;

         function Worthwhile_Line_Break
           (F, X : Line_Break_Index) return Boolean;
         --  Called for X = the first so-far-disabled line break on a line.
         --  Returning False means don't bother enabling it. F is the previous
         --  one.

         procedure Assertions;
         --  Assert that the line Length has been set if and only if the line
         --  break is enabled.

         procedure Assertions is
         begin
            for X in 1 .. Last_Index (All_LBI) loop
               declare
                  Break : Line_Break renames All_LB (All_LBI (X));
               begin
                  if X = Last_Index (All_LBI) then
                     pragma Assert (Break.Enabled and then Break.Length = 0);
                  elsif Break.Enabled then
                     pragma Assert
                       (Break.Length =
                        Line_Len
                          (All_LB, All_LBI (X),
                           All_LBI (Next_Enabled (Lines_Data, X))));
                  else
                     pragma Assert (Break.Length = Natural'Last);
                  end if;
               end;
            end loop;
         end Assertions;

         function Worthwhile_Line_Break
           (F, X : Line_Break_Index) return Boolean
         is
            This      : constant Positive := Sloc_First (LB_Tok (All_LB (X)));
            Prev      : Positive := Sloc_First (LB_Tok (All_LB (F)));
            Threshold : constant Positive := PP_Indent_Continuation (Cmd);
         begin
            if All_LB (F).Hard then
               Prev := Prev + 1; -- skip NL
            end if;

            --  If we have something like:
            --     P (...
            --  there's no point in turning it into:
            --     P
            --       (...
            --  assuming PP_Cont_Line_Indentation = 2, because it doesn't shorten
            --  any lines. If the procedure name is slightly longer than "P":
            --     Proc (...
            --  there's _probably_ no point in turning it into:
            --     Proc
            --       (...
            --  because it only saves 3 characters, so we will probably have
            --  to split up the "..." parameters anyway.

            if This - Prev <= Threshold then
               return False;
            end if;
            return True;
         end Worthwhile_Line_Break;

         F   : Line_Break_Index_Index := 1;
         L   : Line_Break_Index_Index;
         Len : Natural;

         Level       : Nesting_Level;
         More_Levels : Boolean;

         Again : constant String :=
           (if First_Time then "first time" else "again");

         LB : Line_Break_Index_Vector;
         --  All line breaks for a given line that are at the same level,
         --  plus an extra one at the end that is already enabled.

      --  Start of processing for Split_Lines

      begin
         if Arg (Cmd, Source_Line_Breaks) then
            return;
         end if;

         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "before Split_Lines " & Again));

         Collect_Line_Breaks
           (Lines_Data_P, New_Tokns,
            Do_All => True, Do_Enabled => False, Do_Syntax => False,
            First_Time => First_Time);

         if False then
            --  ???For debugging, always split at optional newlines
            for Line_Index in 1 .. Last_Index (All_LBI) loop
               All_LB (All_LBI (Line_Index)).Enabled := True;
            end loop;
            return;
         end if;

         while F /= Last_Index (All_LBI) loop -- through line breaks
            Level       := 1;
            More_Levels := True;

            loop -- through levels
               L   := Next_Enabled (Lines_Data, F);
               Len := Line_Len (All_LB, All_LBI (F), All_LBI (L));

               exit when Len <= Arg (Cmd, Max_Line_Length); -- short enough
               exit when not More_Levels; -- no more line breaks to enable

               More_Levels := False;
               Clear (LB);

               --  Collect line breaks at current level into LB, along with an
               --  additional one so we can always do LB (X + 1) below.

               for X in F + 1 .. L - 1 loop
                  if All_LB (All_LBI (X)).Level > Level then
                     More_Levels := True;
                  elsif All_LB (All_LBI (X)).Level = Level then
                     Append (LB, All_LBI (X));
                  end if;
               end loop;
               Append (LB, All_LBI (L));

               declare
                  FF : Line_Break_Index := All_LBI (F);
                  LL : Line_Break_Index;
                  Prev_LL : Line_Break_Index;
               begin
                  --  Loop through line breaks at current level

                  for X in 1 .. Last_Index (LB) - 1 loop

                     LL := LB (X);
                     Prev_LL := LL;
                     pragma Assert (All_LB (LL).Level = Level);

                     --  Don't enable the first one, unless it's "worthwhile"
                     --  according to the heuristic.

                     if LL = All_LBI (F + 1)
                       and then not Worthwhile_Line_Break (All_LBI (F), LL)
                     then
                        null;

                     --  If the line is too long, enable this soft line
                     --  break. In --no-compact mode, if one line break is
                     --  enabled, we enable all line breaks at the same nesting
                     --  level, except that we don't do that within binary
                     --  operators.

                     elsif Line_Len (All_LB, FF, LB (X + 1)) >
                         Arg (Cmd, Max_Line_Length)
                       or else (not Arg (Cmd, Compact)
                                  and then All_LB (LL).Bin_Op_Count = 0)
                     then
                        pragma Assert (not All_LB (LL).Enabled);
                        All_LB (LL).Enabled := True;
                        All_LB (LL).Affects_Comments := True;
                        --  Keep the indentation level of the previous LL if
                        --  the threshold between the two values is 1
                        --  (this could be the case of the split line when
                        --  one or more than one paranthesis are present on the
                        --  same level having soft line breaks associated)
                        if Prev_LL /= LL
                          and then not All_LB (Prev_LL).Enabled
                          and then (All_LB (Prev_LL).Indentation =
                                        All_LB (LL).Indentation - 1)
                        then
                           All_LB (LL).Indentation :=
                             All_LB (LL).Indentation - 1;
                        end if;

                        FF := LL;
                     end if;

                  end loop; -- through line breaks at current level
               end;

               Level := Level + 1;
            end loop; -- through levels

            All_LB (All_LBI (F)).Length := Len;
            pragma Assert
              (All_LB (All_LBI (F)).Length =
                 Line_Len (All_LB, All_LBI (F),
                              All_LBI (Next_Enabled (Lines_Data, F))));
            F := L;
         end loop; -- through line breaks

         All_LB (All_LBI (F)).Length := 0; -- last line

         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "after Split_Lines " & Again));

         --  In case of partial reformatting of an enum type makes that the
         --  last LB is not always enabled. This is why this assertion will
         --  fail.
         if not Partial_Gnatpp then
            pragma Debug (Assertions);
         end if;
      end Split_Lines;

      procedure Enable_Line_Breaks_For_EOL_Comments
        (Src_Buf : in out Buffer;
         Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False)
      is
         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         All_LB : Line_Break_Vector renames Lines_Data.All_LB;
         All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
         Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
         New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;

         function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
         --  True if the tokens have the same kind and same text, except that the
         --  matching is case insensitive for identifiers, reserved words, and
         --  string literals that could be operator symbols. The source locations
         --  are ignored.

         function Looking_At_Paren_Or_Comma (Break : Line_Break) return Boolean;
         --  True if we're look at '(' or ',', skipping any Disabled_LB_Tokens
         --  and Spaces tokens.

         procedure Do_End_Of_Line_Comment;
         --  Found an End_Of_Line_Comment; enable line breaks as appropriate.

         procedure Move_Past_Out_Tok;

         function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean is
         begin
            if Debug_Mode then
               Dbg_Out.Output_Enabled := True;
               Dbg_Out.Put
                 ("match ""\1"", ""\2"" ? ",
                  Str (Text (Src_Tok)).S, Str (Text (Out_Tok)).S);
            end if;
            return R : Boolean do
               if Kind (Src_Tok) = Kind (Out_Tok) then
                  case Kind (Src_Tok) is
                     when Line_Break_Token | Tab_Token | EOL_Token | Spaces |
                       Comment_Kind | Preprocessor_Directive =>
                        raise Program_Error;

                     when Start_Of_Input | End_Of_Input | Other_Lexeme |
                       Illegal_Character =>
                        pragma Assert
                          (Equal_Ignoring_CR (Text (Src_Tok), Text (Out_Tok)));
                        R := True;

                     when Reserved_Word =>
                        pragma Assert
                          (Case_Insensitive_Equal
                            (Text (Src_Tok), Text (Out_Tok)));
                        R := True;

                     when Ident =>
                        R := Case_Insensitive_Equal
                          (Text (Src_Tok), Text (Out_Tok));

                     when Numeric_Literal =>
                        R := Num_Lits_Match (Src_Tok, Out_Tok, Cmd);

                     when Character_Literal =>
                        R := Text (Src_Tok) = Text (Out_Tok);

                     when String_Lit =>
                        if Is_Op_Sym_With_Letters (Text (Src_Tok)) then
                           R := Case_Insensitive_Equal
                             (Text (Src_Tok), Text (Out_Tok));
                        else
                           R := Text (Src_Tok) = Text (Out_Tok);
                        end if;
                  end case;

               --  Allow, for example, reserved word "interface" to match
               --  identifier "INTERFACE".

               elsif Kind (Src_Tok) in Reserved_Word_New
                 and then Kind (Out_Tok) = Ident
               then
                  R := Case_Insensitive_Equal (Text (Src_Tok), Text (Out_Tok));

               else
                  R := False;
               end if;

               if Debug_Mode then
                  Dbg_Out.Put ("\1\n", (if R then "yes" else "No!"));
               end if;
            end return;
         end Match;

         Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
         New_Tok : Tokn_Cursor := Next_ss (First (New_Tokns'Access));
         --  Cursors into Src_Tokns and New_Tokns, respectively. Skip the
         --  first Start_Of_Input token, which is just a sentinel.

         Last_LB : Tokn_Cursor := First (New_Tokns'Access);
         --  Last encountered line-break token

         New_Cur_Line : Line_Break_Index_Index := 1;

         procedure Move_Past_Out_Tok is
         begin
            while Kind (New_Tok) in Tab_Token loop
               Next (New_Tok);
            end loop;

            Next (New_Tok);

            while Kind (New_Tok) in Disabled_LB_Token | Tab_Token | Spaces loop
               if Kind (New_Tok) = Disabled_LB_Token then
                  Last_LB := New_Tok;
                  New_Cur_Line := New_Cur_Line + 1;
                  pragma Assert
                    (Insert_Blank_Lines (Cmd)
                       or else LB_Tok (All_LB (All_LBI (New_Cur_Line))) =
                         New_Tok);
               end if;
               Next (New_Tok);
            end loop;

            if Kind (New_Tok) = Enabled_LB_Token then
               Last_LB := New_Tok;
               New_Cur_Line := New_Cur_Line + 1;
               pragma Assert
                 (Insert_Blank_Lines (Cmd)
                    or else LB_Tok (All_LB (All_LBI (New_Cur_Line))) = New_Tok);
            end if;
         end Move_Past_Out_Tok;

         function Looking_At_Paren_Or_Comma (Break : Line_Break) return Boolean is
            T : Tokn_Cursor := LB_Tok (Break);
         begin
            while Kind (T) in Disabled_LB_Token | Spaces loop
               Next (T);
            end loop;
            return Kind (T) in '(' | ',';
         end Looking_At_Paren_Or_Comma;

         procedure Do_End_Of_Line_Comment is
            LB : Line_Break renames All_LB (All_LBI (New_Cur_Line));
         begin
            --  If an end-of-line comment appears at a place where there is a
            --  soft line break, we enable that line break. We also enable
            --  previous line breaks that are at the same level, or that belong
            --  to '('. We stop when we see an enabled line break.

            pragma Assert
              (if Kind (Last_LB) /= Start_Of_Input then
                (Sloc_First (LB_Tok (LB)) = Sloc_First (Last_LB)) =
                (LB_Tok (LB) = Last_LB));
            if LB_Tok (LB) = Last_LB then
               for Break in reverse 1 .. New_Cur_Line loop
                  declare
                     Prev_LB : Line_Break renames All_LB (All_LBI (Break));
                  begin
                     exit when Prev_LB.Enabled;

                     if Prev_LB.Level = LB.Level
                       or else
                        (Prev_LB.Level < LB.Level
                           and then Looking_At_Paren_Or_Comma (Prev_LB))
                     then
                        Prev_LB.Enabled := True;
                        Prev_LB.Affects_Comments := True;
                     end if;
                  end;
               end loop;
            end if;
            Next_ss (Src_Tok);
         end Do_End_Of_Line_Comment;

         Qual_Nesting : Natural := 0;
      --  Count the nesting level of qualified expressions containing aggregates
      --  with extra parentheses.

      --  Start of processing for Enable_Line_Breaks_For_EOL_Comments

      begin
         if Arg (Cmd, Source_Line_Breaks) then
            return;
         end if;
         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "before Enable_Line_Breaks_For_EOL_Comments"));

         --  Skip initial EOL_Token token
         pragma Assert (Kind (New_Tok) = Enabled_LB_Token);
         Next_ss (New_Tok);

         --  This loop is similar to the one in
         --  Insert_Comments_And_Blank_Lines; see that for commentary.

         loop
            Error_Sloc := To_Langkit (Scanner.Sloc (Src_Tok));

            if Kind (Src_Tok) not in EOL_Token
              and then
              (Match (Src_Tok, New_Tok)
               or else (Kind (Src_Tok) = '!' and then Kind (New_Tok) = '|'))
            then
               exit when Kind (Src_Tok) = End_Of_Input;
               --  i.e. exit when both Src and Out are at end of input

               Move_Past_Out_Tok;
               Next_ss (Src_Tok);

            else
               --  Check for "end;" --> "end Some_Name;" case

               if Kind (Src_Tok) = ';'
                 and then
                   Kind (Prev_Lexeme (Src_Tok)) = Res_End
                 and then Sname_83 (New_Tok)
               then
                  loop -- could be "end A.B.C;"
                     Move_Past_Out_Tok;

                     exit when Kind (New_Tok) /= '.';

                     Move_Past_Out_Tok;
                     pragma Assert (Sname_83 (New_Tok));
                  end loop;

                  pragma Assert
                    (Disable_Final_Check or else
                       Kind (Src_Tok) in ';' | EOL_Token | Comment_Kind);

               --  Check for "end Some_Name;" --> "end;" case. This only happens
               --  when the --no-end-id switch was given. Here, the name was
               --  present in the source, so we insert it.

               elsif not Arg (Cmd, End_Id)
                 and then Kind (New_Tok) = ';'
                 and then
                   Kind (Prev_Lexeme (New_Tok)) = Res_End
                 and then Kind (Src_Tok) in Ident | String_Lit
               then
                  loop -- could be "end A.B.C;"
                     Next_ss (Src_Tok);

                     exit when Kind (Src_Tok) /= '.';

                     Next_ss (Src_Tok);
                     pragma Assert
                       (Kind (Src_Tok) in Ident | String_Lit);
                  end loop;
                  pragma Assert
                    (Disable_Final_Check or else
                       Kind (Src_Tok) in ';' | EOL_Token | Comment_Kind);

               --  Check for "private end" --> "end" case, with a possible
               --  comment between "private" and "end".

               elsif Kind (Src_Tok) = Res_Private
                 and then Kind (New_Tok) = Res_End
               then
                  pragma Assert
                    (Disable_Final_Check
                       or else Kind (Next_Lexeme (Src_Tok)) = Res_End);
                  Next_ss (Src_Tok);

               --  Check for "T'((X, Y, Z))" --> "T'(X, Y, Z)" case

               elsif Kind (Src_Tok) = '('
                 and then Kind (Prev_Lexeme (Src_Tok)) = '('
                  --???Also check that the one before that is a tick!
               then
                  Qual_Nesting := Qual_Nesting + 1;
                  Next_ss (Src_Tok);
               elsif Qual_Nesting > 0
                 and then Kind (Src_Tok) = ')'
                 and then Kind (Prev_Lexeme (Src_Tok)) = ')'
               then
                  Qual_Nesting := Qual_Nesting - 1;
                  Next_ss (Src_Tok);

               elsif Kind (Src_Tok) = End_Of_Line_Comment then
                  Do_End_Of_Line_Comment;

               elsif Kind (Src_Tok) in EOL_Token then
                  Next_ss (Src_Tok);

               elsif Kind (Src_Tok) in Whole_Line_Comment then
                  Next_ss (Src_Tok);

               elsif Kind (New_Tok) in Line_Break_Token then
                  Move_Past_Out_Tok;

               elsif Kind (Src_Tok) = Preprocessor_Directive then
                  Next (Src_Tok);

               elsif Disable_Final_Check then
                  Next_ss (Src_Tok);
                  if At_Last (Src_Tok) then
                     goto Done;
                  end if;
               else
                  --  Partial mode formatting specific tokens synchronisations
                  --  where no token mismatch is supposed to be raised
                  if Partial_Gnatpp then

                     if Kind (Src_Tok) = ';'
                       and then Kind (Prev (Src_Tok)) in
                         Res_End | Res_Record | Res_Loop | Ident | ')'
                       and then Kind (New_Tok) = End_Of_Input
                     then
                        Next_ss (Src_Tok);
                     else
                        --  When enter here mostly an infinite loop situation
                        --  is detected in partial mode
                        raise Partial_Gnatpp_Error;
                     end if;

                  else
                     Raise_Token_Mismatch
                       ("eol_comments", Lines_Data, Src_Buf, Src_Tok, New_Tok);
                  end if;

               end if;
            end if;
         end loop;

         pragma Assert (At_Last (Src_Tok));
         pragma Assert (At_Last (New_Tok));
         <<Done>> null;

         pragma Assert (Disable_Final_Check or else Qual_Nesting = 0);
         pragma Assert (At_Beginning (Src_Buf));
      end Enable_Line_Breaks_For_EOL_Comments;

      procedure Insert_Comments_And_Blank_Lines
        (Src_Buf : in out Buffer;
         Messages : out Scanner.Source_Message_Vector;
         Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Pp_Off_Present : in out Boolean;
         Partial_Gnatpp : Boolean := False)
      is
         pragma Assert (not Pp_Off_Present); -- initialized by caller
         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
         All_LB : Line_Break_Vector renames Lines_Data.All_LB;
         Temp_LBI : Line_Break_Index_Vector renames Lines_Data.Temp_LBI;
         Enabled_LBI : Line_Break_Index_Vector renames Lines_Data.Enabled_LBI;
         Syntax_LBI : Line_Break_Index_Vector renames Lines_Data.Syntax_LBI;
         Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
         Saved_New_Tokns : Scanner.Tokn_Vec renames Lines_Data.Saved_New_Tokns;

         procedure Reset_Indentation;
         --  Set the indentation to it's initial value (usually 0, but can be set
         --  by the --initial-indentation switch.

         function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
         --  True if the tokens have the same kind and same text, except that the
         --  matching is case insensitive for identifiers, reserved words, and
         --  string literals that could be operator symbols. The source locations
         --  are ignored.

         procedure New_To_Newer (Expect : Opt_Token_Kind := Nil);
         --  Copy New_Tok to New_Tokns, and move New_Tok one forward in
         --  Saved_New_Tokns. If Expect is not Nil, then New_Tok should be of
         --  that kind.

         procedure Insert_End_Of_Line_Comment;
         --  Found an End_Of_Line_Comment comment; copy it to the buffer. If it
         --  is too long to fit on the line, turn it into a Whole_Line_Comment,
         --  taking care to indent.

         --  Note that the Subtree_To_Ada pass already inserted indentation, so we
         --  mostly keep the indentation level at zero. The exception is comments,
         --  which Subtree_To_Ada didn't see. For comments, we temporarily set the
         --  indentation to that of the surrounding code.

         procedure Insert_Whole_Line_Comment;
         --  Found a Whole_Line_Comment; copy it to the output, taking care to
         --  indent, except that if the comment starts in column 1, we assume
         --  the user wants to keep it that way.

         procedure Insert_Preprocessor_Directive;
         --  Found a Preprocessor_Directive; copy it to the output, preserving
         --  its indentation.

         procedure Insert_Private;
         --  If a private part has no declarations, the earlier passes don't
         --  insert "private", whether or not it was in the source code. If
         --  there is a comment, this re-inserts "private" before the comment,
         --  to avoid messing up the formatting.

         function Is_Named_Assoc return Boolean;
         --  Check if the current line construct contains a simple or multiple
         --  named aggregate association
         --  (i.e., something like "X => val1," or "X | Y => val2,")

         function LB_Needs_To_Be_Enabled (Src_Tok : Tokn_Cursor) return Boolean;

         function LB_Needs_To_Be_Enabled (Src_Tok : Tokn_Cursor) return Boolean
         is
         begin
            return
              Kind (Src_Tok) in Other_Whole_Line_Comment | Fillable_Comment
              and then Kind (Prev_ss (Src_Tok)) in True_End_Of_Line
              and then
                (Kind (Prev (Prev_ss (Src_Tok))) in  ',' | Res_Is
                 or else Kind (Prev (Prev_ss (Src_Tok))) = End_Of_Line_Comment
                 or else
                   (Kind (Prev (Prev_ss (Src_Tok))) in True_End_Of_Line
                    and then
                    Kind (Prev
                          (Prev (Prev_ss (Src_Tok)))) = End_Of_Line_Comment));
         end LB_Needs_To_Be_Enabled;

         procedure Reset_Indentation is
         begin
            Cur_Indentation := Arg (Cmd, Initial_Indentation);
         end Reset_Indentation;

         function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean is
         begin
            if Debug_Mode then
               Dbg_Out.Output_Enabled := True;
               Dbg_Out.Put
                 ("match ""\1"", ""\2"" ? ",
                  Str (Text (Src_Tok)).S, Str (Text (Out_Tok)).S);
            end if;
            return R : Boolean do
               if Kind (Src_Tok) = Kind (Out_Tok) then
                  case Kind (Src_Tok) is
                     when EOL_Token | Spaces | Comment_Kind |
                       Preprocessor_Directive | Line_Break_Token =>
                        raise Program_Error;

                     when Start_Of_Input | End_Of_Input |
                       Tab_Token | Other_Lexeme | Illegal_Character =>
                        pragma Assert
                          (Equal_Ignoring_CR (Text (Src_Tok), Text (Out_Tok)));
                        R := True;

                     when Reserved_Word =>
                        pragma Assert
                          (Case_Insensitive_Equal
                            (Text (Src_Tok), Text (Out_Tok)));
                        R := True;

                     when Ident =>
                        R := Case_Insensitive_Equal
                          (Text (Src_Tok), Text (Out_Tok));

                     when Numeric_Literal =>
                        R := Num_Lits_Match (Src_Tok, Out_Tok, Cmd);

                     when Character_Literal =>
                        R := Text (Src_Tok) = Text (Out_Tok);

                     when String_Lit =>
                        if Is_Op_Sym_With_Letters (Text (Src_Tok)) then
                           R := Case_Insensitive_Equal
                             (Text (Src_Tok), Text (Out_Tok));
                        else
                           R := Text (Src_Tok) = Text (Out_Tok);
                        end if;
                  end case;

               elsif Kind (Src_Tok) in Reserved_Word_New
                 and then Kind (Out_Tok) = Ident
               then
                  R := Case_Insensitive_Equal (Text (Src_Tok), Text (Out_Tok));

               else
                  R := False;
               end if;

               if Debug_Mode then
                  Dbg_Out.Put ("\1\n", (if R then "yes" else "No!"));
               end if;
            end return;
         end Match;

         New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
         Ignore : Boolean := Move_Tokns
           (Target => Saved_New_Tokns, Source => New_Tokns);
         --  After Collect_Line_Breaks below, the Tok field of each line break
         --  will point into Saved_New_Tokns.

         Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
         New_Tok : Tokn_Cursor := Next_ss (First (Saved_New_Tokns'Access));
         --  Cursors into Src_Tokns and Out_Tokns, respectively. Skip the
         --  first Start_Of_Input token, which is just a sentinel.

         Start_Line_Src_Tok : Tokn_Cursor := First (Src_Tokns'Access);
         --  Token at the beginning of the previous line, but never a comment

         function L_Paren_Indentation_For_Preserve return Natural;
         --  Returns the last parenthesis indent of the stack
         procedure Update_L_Paren_Indent_For_Preserve (Indent : Natural);
         --  Update last paren indentation to Indentation
         function Before_Paren_Indent_For_Preserve return Natural;
         --  Returns the previous (before the last element) parenthesis indent
         --  from the stack

         --  In --preserve-line-breaks mode, this is the amount of additional
         --  indentation needed to make lines start just after a "(" on the
         --  previous line. For example, if the input is:
         --     X := Foo (Y,
         --               Z);
         --  we would normally put that all on one line. In
         --  --preserve-line-breaks mode, we want a line break between
         --  "," and "Z", and we want Z to line up after "(".
         --  The previous accessors are used to get indentation
         --  levels for previous and last paranthesis of a paren stack and to
         --  update paren indentation level when this is needed.

         function In_Paren_Context return Boolean;
         --  Returns True if Paren_Stack contains at least 2 elements

         procedure Manage_Paren_Stack;
         --  Paren_Stack (below) is a stack containing one entry for each left
         --  parenthesis that has not yet been closed by a right parenthesis.
         --  Indent is the additional amount to indent to get to one past just
         --  under the "(".
         --
         --  ???This is currently used only for --preserve-line-breaks mode.
         --  We could Consider using it also for the "extra" indent by 1
         --  character mentioned in PP.Actions (see type Ada_Template), and
         --  also for Qual_Nesting.

         New_Line_Start_Out : Tokn_Cursor :=
             Next (First (Saved_New_Tokns'Access));
         --  Used only in --preserve-line-breaks mode. The first token on the
         --  current line in Out_Tokns. Since Split_Lines has not yet been run,
         --  there is no indentation present in the output.

         type Paren_Stack_Index is new Positive;
         type Paren_Stack_Element is record
            Indent : Natural;
         end record;
         type Paren_Stack_Element_Array is
           array (Paren_Stack_Index range <>) of Paren_Stack_Element;
         package Paren_Vectors is new Utils.Vectors
           (Paren_Stack_Index,
            Paren_Stack_Element,
            Paren_Stack_Element_Array);
         Paren_Stack : Paren_Vectors.Vector;
         use Paren_Vectors;

         --  Enabled_LBI are the line breaks used for indenting end-of-line
         --  comments. Syntax_LBI are the ones used for indenting whole-line
         --  comments. Enabled_Cur_Line and Syntax_Cur_Line index into those,
         --  respectively.

         Enabled_Cur_Line : Line_Break_Index_Index := 1;
         Syntax_Cur_Line  : Line_Break_Index_Index := 1;

         Prev_New_Tok : Tokn_Cursor := First (Saved_New_Tokns'Access);
         --  Used by Manage_Paren_Stack to avoid pushing/popping the same paren
         --  twice.

         Extra_Indent_For_Preserved_Line : Boolean := True;
         --  True if a "preserved" line break should get extra indentation as a
         --  line continuation, in addition to any "("-related indentation.
         --  For example, we want extra here:
         --     Foo
         --       (X,
         --        Y);
         --  but not here:
         --     Foo (X,
         --          Y);

         procedure Manage_Paren_Stack is
            Crt_Indent : Natural := 0;
         begin
            if New_Tok = Prev_New_Tok then
               return;
            end if;

            Prev_New_Tok := New_Tok;

            --  We push the stack for "(" and pop for ")".

            --  Normally, Extra_Indent_For_Preserved_Line is True, but we set
            --  it False when we're inside an unclosed "(..." that is on the
            --  same line as the previous Enabled line break. We're talking
            --  about line breaks from previous phases, not "preserved" ones
            --  added in this phase.

            if Kind (New_Tok) = '(' then
               if Is_Empty (Paren_Stack) then
                  if Partial_Gnatpp and then Enabled_Cur_Line = 1 then
                     Extra_Indent_For_Preserved_Line := False;
                  else
                     declare
                        LB : Line_Break renames
                          All_LB (Enabled_LBI (Enabled_Cur_Line - 1));
                        New_Enabled_Line_Start : constant Positive :=
                          Sloc_First (LB_Tok (LB)) + 1;
                        New_This_Line_Start : constant Positive :=
                          Sloc_First (New_Line_Start_Out);
                     begin
                        if New_Enabled_Line_Start = New_This_Line_Start then
                           Extra_Indent_For_Preserved_Line := False;
                        end if;
                     end;
                  end if;
               end if;

               --  Add 2 extra spaces to indent at the first character position
               --  inside the parenthesis
               Crt_Indent :=
                 Sloc (New_Tok).First - Sloc (New_Line_Start_Out).First + 2;
               Push (Paren_Stack, (Indent => Crt_Indent));

            elsif Kind (New_Tok) = ')' then
               Pop (Paren_Stack);

               if Is_Empty (Paren_Stack) then
                  Extra_Indent_For_Preserved_Line := True;
               end if;
            end if;
         end Manage_Paren_Stack;

         function L_Paren_Indentation_For_Preserve return Natural is
         begin
            if Is_Empty (Paren_Stack) then
               return 0;
            else
               return Last_Element (Paren_Stack).Indent;
            end if;
         end L_Paren_Indentation_For_Preserve;

         procedure Update_L_Paren_Indent_For_Preserve (Indent : Natural)
         is
            pragma Assert (not Is_Empty (Paren_Stack));
         begin
            Pop (Paren_Stack);
            Push (Paren_Stack, (Indent => Indent + 1));
         end Update_L_Paren_Indent_For_Preserve;

         function Before_Paren_Indent_For_Preserve return Natural is
            use Ada.Containers;
            pragma Assert (Length (Paren_Stack) >= 2);
         begin
            return Element (Previous (Last (Paren_Stack))).Indent;
         end Before_Paren_Indent_For_Preserve;

         function In_Paren_Context return Boolean is
            use Ada.Containers;
         begin
            return (Length (Paren_Stack) >= 2);
         end In_Paren_Context;

         Pending_Tokns : aliased Tokn_Vec;
         --  Disabled_LB_Token, Tab_Token, and Spaces tokens are saved here, so
         --  they can be output later. Spaces need to be deferred, because we
         --  might need to insert tokens before them. Disabled_LB_Token and
         --  Tab_Token need to be deferred because they might be adjacent to
         --  Spaces, and we don't want them to get out of order.

         procedure New_To_Newer (Expect : Opt_Token_Kind := Nil) is
         begin
            pragma Assert (if Expect /= Nil then Kind (New_Tok) = Expect);

            declare
               Pending : Tokn_Cursor :=
                 Next (First (Pending_Tokns'Unchecked_Access));
            begin
               while not After_Last (Pending) loop
                  Append_Tokn (New_Tokns, Pending, Org => "Pending");
                  Next (Pending);
               end loop;
               Clear (Pending_Tokns);
               Append_Tokn (Pending_Tokns, Start_Of_Input);
            end;

            pragma Assert (Kind (New_Tok) not in EOL_Token);
            if Kind (New_Tok) in Line_Break_Token
              and then All_LB (Line_Break_Token_Index (New_Tok)).Enabled
            then
               --  Step past Syntax_LBI at the current position

               while Syntax_Cur_Line <= Last_Index (Syntax_LBI)
                 and then Sloc_First (New_Tok) >=
                   Sloc_First (LB_Tok (All_LB (Syntax_LBI (Syntax_Cur_Line))))
               loop
                  Syntax_Cur_Line := Syntax_Cur_Line + 1;
               end loop;

               --  Step past Enabled_LBI at the current position, taking care
               --  to avoid going past the end.

               while Enabled_Cur_Line < Last_Index (Enabled_LBI)
                 and then Sloc_First (New_Tok) >=
                   Sloc_First (LB_Tok (All_LB (Enabled_LBI (Enabled_Cur_Line))))
               loop
                  Enabled_Cur_Line := Enabled_Cur_Line + 1;
               end loop;

               --  If the Source_Line_Breaks switch was given, and we have
               --  something like "is begin" (with no line break in the
               --  source), then we need to insert a Space to avoid turning it
               --  into the identifier "isbegin".

               if Arg (Cmd, Source_Line_Breaks)
                 and then not All_LB (Line_Break_Token_Index (New_Tok))
                                .Source_Line_Breaks_Enabled
               then
                  if Tokens_Require_Space
                    (Last (New_Tokns'Access), Next_Lexeme (New_Tok))
                  then
                     Append_Spaces (New_Tokns, Count => 1);
                  end if;
               elsif Kind (Last (New_Tokns'Access)) in Line_Break_Token
                 and then All_LB (Line_Break_Token_Index
                                    (Last (New_Tokns'Access))).Enabled
               then
                  pragma Assert
                    (Kind (Last (New_Tokns'Access)) not in Disabled_LB_Token);
                  if Kind (Prev (Last (New_Tokns'Access))) in Comment_Kind
                    and then not Arg (Cmd, Source_Line_Breaks)
                  then
                     null; -- Append_Temp_Line_Break already put one
                  else
                     if not Is_Blank_Line (Last (New_Tokns'Access))
                       or else Preserve_Blank_Lines (Cmd)
                       or else Arg (Cmd, Source_Line_Breaks)
                     then
                        Append_Tokn
                          (New_Tokns, New_Tok, Org => "New_To_Newer 1");
                     end if;
                  end if;
               else
                  Append_Tokn (New_Tokns, New_Tok, Org => "New_To_Newer 2");
               end if;
            else
               Append_Tokn (New_Tokns, New_Tok, Org => "New_To_Newer 3");
            end if;
            Next (New_Tok);

            while Kind (New_Tok) in Disabled_LB_Token | Tab_Token | Spaces loop
               Append_Tokn (Pending_Tokns, New_Tok);
               Next (New_Tok);
            end loop;
         end New_To_Newer;

         procedure Insert_End_Of_Line_Comment is
            pragma Assert (Enabled_Cur_Line > 1);

            function New_Space_NL return Boolean;
            --  True if we should move past a space that is immediately
            --  followed by a new line.

            function New_Space_NL return Boolean is
            begin
               if Get_Num_Tokens (Pending_Tokns) < 3 then
                  return False;
               end if;

               pragma Assert
                 (Kind (Last (Pending_Tokns'Unchecked_Access)) =
                  Kind (Prev (New_Tok)));
               pragma Assert
                 (Kind (Prev (Last (Pending_Tokns'Unchecked_Access))) =
                  Kind (Prev (Prev (New_Tok))));

               declare
                  Pending_Space : constant Boolean :=
                    Kind (Prev (Prev (New_Tok))) = Spaces;
                  At_LB : constant Boolean :=
                    Kind (Prev (New_Tok)) = Disabled_LB_Token
                    or else Kind (Prev (New_Tok)) = Tab_Token;
               begin
                  return Pending_Space and At_LB;
               end;
            end New_Space_NL;

            Prev_LB : Line_Break renames
              All_LB (Enabled_LBI (Enabled_Cur_Line - 1));
            LB      : Line_Break renames All_LB (Enabled_LBI (Enabled_Cur_Line));
            Indentation : constant Natural := Prev_LB.Indentation;
            New_Prev_Src_Tok : constant Tokn_Cursor := Prev (Src_Tok);
            New_Preceding_Blanks : Natural :=
              (if Kind (New_Prev_Src_Tok) = Spaces then
               Tokn_Length (Prev (Src_Tok))
               else 0);
            --  Number of blanks between the previous token and this comment.
            --  Note that tab characters have been expanded into spaces in
            --  Src_Buf.

         --  Start of procedding for Insert_End_Of_Line_Comment

         begin
            --  If we're just before a blank followed by NL, move past the blank,
            --  so we won't add a new NL below.

            if New_Space_NL then
               pragma Assert (New_Preceding_Blanks > 0);
               New_Preceding_Blanks := New_Preceding_Blanks - 1;

               --  Remove the Spaces token (which must exist) from
               --  Pending_Tokns.

               declare
                  New_Pending_Tokns : Tokn_Vec;
                  Pending : Tokn_Cursor := First (Pending_Tokns'Unchecked_Access);
                  Spaces_Found : Boolean := False;
               begin
                  while not After_Last (Pending) loop
                     if Kind (Pending) = Spaces then
                        pragma Assert (Tokn_Length (Pending) = 1);
                        Spaces_Found := True;
                     else
                        Append_Tokn (New_Pending_Tokns, Pending,
                                     Org => "Pending for comment");
                     end if;
                     Next (Pending);
                  end loop;
                  pragma Assert (Spaces_Found);
                  Pending_Tokns := New_Pending_Tokns;
               end;
               Append_Spaces (New_Tokns, Count => 1);
            end if;

            Append_Spaces
              (New_Tokns, Count => New_Preceding_Blanks, Existing_OK => True);
            Insert_Comment_Text (Lines_Data_P, Cmd, Src_Tok);

            --  In the usual case, the end-of-line comment is at a natural
            --  hard line break, like this:
            --
            --      X := X + 1; -- Increment X
            --
            --  so we don't want another one. Likewise, if
            --  Enable_Line_Breaks_For_EOL_Comments enabled a natural
            --  soft line break, as in:
            --
            --     type Enum is
            --       (Red, -- soft line break enabled here
            --        ...);
            --
            --  we don't want another one. But if the original was:
            --
            --      procedure P ( -- strange place for a comment
            --
            --  we need to add a line break after the comment.

            declare
               At_Tok : Boolean := False;
            begin
               if Get_Num_Tokens (Pending_Tokns) >= 3 then
                  At_Tok := Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                    and then Kind (Prev (New_Tok)) = Spaces
                    and then LB_Tok (LB) = Prev (Prev (New_Tok));
               end if;
               if LB_Tok (LB) = Prev (New_Tok) or else LB_Tok (LB) = New_Tok then
                  At_Tok := True;
               end if;

               if At_Tok then
                  Append_Tokn (New_Tokns, False_End_Of_Line, "eol extra");
                  --  This is needed because every comment in New_Tokns must be
                  --  followed by EOL_Token.
               else
                  pragma Assert
                    (if Kind (New_Tok) = Disabled_LB_Token then
                       not All_LB (Line_Break_Token_Index (New_Tok)).Enabled);

                  if Kind (New_Tok) = Enabled_LB_Token then
                     declare
                        This_LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (New_Tok));
                     begin
                        Append_Spaces (New_Tokns, Count => This_LB.Indentation);
                     end;

                  --  Avoid inserting an extra line break if we're just past a
                  --  line break followed by spaces. This happens for soft line
                  --  breaks, e.g. a comment on an enumeration literal
                  --  specification.

                  elsif Kind (Prev (New_Tok)) = Spaces
                    and then Kind (Prev (Prev (New_Tok))) in Line_Break_Token
                  then
                     null;

                  else
                     if Kind (New_Tok) = Ident
                       and then Kind (Prev (New_Tok)) = '('
                     then
                        --  Handling the situation where EOL comment follows '('
                        --  (i.e., the situations like
                        --      function X
                        --        (-- some EOL comment here
                        --         V1,
                        --         ...) return Boolean
                        --
                        --  Taking the last LB position for the indentation
                        --  level for the parameters and adjust '(' position in
                        --  Paren_Stack if needed
                        declare
                           Crt_Indent : Integer := LB.Indentation + 1;
                        begin

                           if Arg (Cmd, Par_Threshold) = 0 then
                              Crt_Indent := Crt_Indent - 1;
                           end if;

                           Cur_Indentation := Crt_Indent;

                           if L_Paren_Indentation_For_Preserve /= Crt_Indent
                           then
                              Update_L_Paren_Indent_For_Preserve
                                (Crt_Indent - 2);
                           end if;
                        end;

                     else
                        --  Allign on the previous LB indentation level
                        Cur_Indentation := Indentation;
                     end if;

                     if not Arg (Cmd, Source_Line_Breaks) then
                        Append_Temp_Line_Break
                          (Lines_Data_P,
                           Org =>
                             "Append_Temp_ in Insert_End_Of_Line_Comment");
                     end if;
                     Reset_Indentation;
                  end if;
               end if;
            end;

            Next_ss (Src_Tok);
         end Insert_End_Of_Line_Comment;

         Pp_On : Boolean := True;
         --  True initially, and if the most recently encountered Pp_Off_Comment
         --  or Pp_On_Comment was Pp_On_Comment.
         Last_Pp_Off_On : Tokn_Cursor := First (Src_Tokns'Access);
         --  If > First, this points to the most recently encountered
         --  Pp_Off_Comment or Pp_On_Comment in Src_Tokns. Used to check for
         --  errors; they must alternate, OFF, ON, OFF, ....

         function Before_Indentation return Natural;
         --  Same as "All_LB (Syntax_LBI (Cur_Line - 1)).Indentation", except
         --  we skip Syntax_LBI with Affects_Comments = False. In other
         --  words, this is the previous line-breaks indentation which should
         --  affect comments.
         function Prev_Indentation_Affect_Comments return Boolean;
         --  Will return True if the last line-breaks indentation affect
         --  comments (i.e., Syntax_LBI Affects_Comments = False
         function After_Indentation return Natural;
         --  Same as "All_LB (Syntax_LBI (Cur_Line)).Indentation", except we
         --  skip Syntax_LBI with Affects_Comments = False. In other words,
         --  this is the current/next line-breaks indentation which should
         --  affect comments.

         function Before_Indentation return Natural is
            X : Line_Break_Index_Index :=
              (if Syntax_Cur_Line = 1 then 1 else Syntax_Cur_Line - 1);
         begin
            while X > 1 and then
              not All_LB (Syntax_LBI (X)).Affects_Comments
            loop
               X := X - 1;
            end loop;
            return All_LB (Syntax_LBI (X)).Indentation;
         end Before_Indentation;

         function Prev_Indentation_Affect_Comments return Boolean is
            X : constant Line_Break_Index_Index :=
              (if Syntax_Cur_Line = 1 then 1 else Syntax_Cur_Line - 1);
         begin
            return (X > 1 and then All_LB (Syntax_LBI (X)).Affects_Comments);
         end Prev_Indentation_Affect_Comments;

         function After_Indentation return Natural is
            X : Line_Break_Index_Index := Syntax_Cur_Line;
         begin
            while X < Last_Index (Syntax_LBI)
              and then not All_LB (Syntax_LBI (X)).Affects_Comments
            loop
               X := X + 1;
            end loop;
            if X <= Last_Index (Syntax_LBI) then
               return All_LB (Syntax_LBI (X)).Indentation;
            else
               pragma Assert (Arg (Cmd, Source_Line_Breaks)
                              or else Partial_Gnatpp);
               return 0;
            end if;
         end After_Indentation;

         procedure Insert_Whole_Line_Comment is
            function Look_Before return Boolean;
            --  True if we should look before the current location to determine
            --  indentation level for the comment. If the next lexeme is "begin",
            --  for example, we want to indent to the level of "begin", even
            --  though there is probably previous code more deeply indented.

            procedure Set_Cur_Indent;
            --  Set Cur_Indentation as appropriate

            function Look_Before return Boolean is
            begin
               if Kind (New_Tok) = End_Of_Input then
                  return True;
               end if;

               --  If we have a comment lined up with the preceding line, with
               --  no blank lines in between, then we try to keep it lined up,
               --  even if "begin" (etc) follows.

               pragma Assert
                 (Sloc (Start_Line_Src_Tok).Line < Sloc (Src_Tok).Line);
               if Sloc (Start_Line_Src_Tok).Line = Sloc (Src_Tok).Line - 1
                 and then Sloc (Start_Line_Src_Tok).Col = Sloc (Src_Tok).Col
               then
                  return True;
               end if;

               --  Should the following list include "exception"???
               return Kind (New_Tok) not in
                 Res_Begin | Res_Else | Res_Elsif | Res_When;
            end Look_Before;

            Indentation : Natural;
            Corrected_Indentation : Natural := 0;

            procedure Set_Cur_Indent is
            begin
               if Sloc (Src_Tok).Col = 1
                 or else Kind (Src_Tok) = Special_Comment
                 or else Arg (Cmd, Comments_Unchanged)
               then
                  Cur_Indentation := Sloc (Src_Tok).Col - 1; -- Keep as in input
               else
                  Cur_Indentation := Indentation;

                  declare
                     W          : constant Positive := Width (Src_Tok);
                     Src_Indent : constant Positive := Sloc (Src_Tok).Col - 1;
                  begin

                     if (not Comment_Filling_Enabled (Cmd)
                         or else Kind (Src_Tok) /= Fillable_Comment)
                       and then Cur_Indentation + W > Arg (Cmd, Max_Line_Length)
                       and then W <= Arg (Cmd, Max_Line_Length)
                     then
                        --  If the current line length exceeds Max_Line_Length
                        --  get the minimal indentation level and allign at the
                        --  left, either choosing the current indentation
                        --  level either keeping it as in input.

                        Cur_Indentation :=
                          (if Src_Indent mod PP_Indentation (Cmd) = 0
                           then Natural'Min (Src_Indent, Indentation)
                           else Indentation);
                     end if;
                  end;

               end if;
            end Set_Cur_Indent;

            function Next_Is_Action (Tok : Tokn_Cursor) return Boolean is
              (not After_Last (Next (Tok)) and then
               Kind (Next (Tok))
               in Res_Procedure | Res_Function | Res_Overriding);

            function Next_Is_Type (Tok : Tokn_Cursor) return Boolean is
              (not After_Last (Next (Tok)) and then
               Kind (Next (Tok)) = Res_Type);

            function Next_Is_Begin (Tok : Tokn_Cursor) return Boolean is
              (not After_Last (Next (Tok)) and then
               Kind (Next (Tok)) = Res_Begin);

            function Next_Is_End (Tok : Tokn_Cursor) return Boolean is
              (not After_Last (Next (Tok)) and then
               (Kind (Next (Tok)) = Res_End
               or else Kind (Next_ss (Tok)) = Res_End));

            function Next_Is_Inside_Case (Tok : Tokn_Cursor) return Boolean
            is
              (not After_Last (Next (Tok)) and then
               Kind (Next (Tok)) in Res_Case | Res_When);

            function Next_Is_Inside_If (Tok : Tokn_Cursor) return Boolean is
              (not After_Last (Next (Tok)) and then
               Kind (Next (Tok)) in Res_Else | Res_Elsif);

            use Source_Message_Vectors;

            Other_Sloc : constant String := Sloc_Image (Sloc (Last_Pp_Off_On));
            Message : Source_Message := (Sloc (Src_Tok), others => <>);

         --  Start of processing for Insert_Whole_Line_Comment

         begin

            --  Processing in preparation for Copy_Pp_Off_Regions. That depends
            --  on an alternating sequence: OFF, ON, OFF, ON, .... So we check
            --  that here, and abort processing if it's not true.

            case Whole_Line_Comment'(Kind (Src_Tok)) is
               when Pp_Off_Comment =>
                  if Pp_On then
                     Pp_On := False;
                     Last_Pp_Off_On := Src_Tok;
                     pragma Assert (Last_Pp_Off_On /= First (Src_Tokns'Access));
                  else
                     Utils.Char_Vectors.Char_Vectors.Append
                       (Message.Text,
                        "pretty printing already disabled at " & Other_Sloc);
                     Append (Messages, Message);
                     raise Post_Tree_Phases_Done;
                  end if;
               when Pp_On_Comment =>
                  if Pp_On then
                     Utils.Char_Vectors.Char_Vectors.Append
                       (Message.Text,
                        "pretty printing already enabled at " & Other_Sloc);
                     Append (Messages, Message);
                     raise Post_Tree_Phases_Done;
                  else
                     Pp_On := True;
                     Last_Pp_Off_On := Src_Tok;
                     pragma Assert (Last_Pp_Off_On /= First (Src_Tokns'Access));
                  end if;
               when Other_Whole_Line_Comment |
                 Special_Comment | Fillable_Comment => null;
            end case;

            --  Comments at the beginning are not indented. The "2" is to skip the
            --  initial sentinel NL.

            if Kind (Prev (Prev (New_Tok))) = Start_Of_Input then
               Indentation := 0;

            --  Otherwise, we indent as for the max of the preceding and following
            --  line breaks, except when Look_Before is False (as it is for this
            --  comment, which is followed by "else").

            else
               Indentation := After_Indentation;

               if Look_Before then

                  --  If last lines-breaks indentation does not affect comments
                  --  and if the previous line is a blank line and the following
                  --  line is a subprogram declaration, the after indentation
                  --  value is kept.
                  if not Prev_Indentation_Affect_Comments and then
                    Next_Is_Action (New_Tok) and then
                    (Is_Blank_Line (Prev_ss (Src_Tok))
                     or else
                       (Kind (Src_Tok) = Other_Whole_Line_Comment and then
                        Kind (New_Tok) = Enabled_LB_Token and then
                        Kind (Prev (New_Tok)) = ';'))

                  then

                     null;

                     --  Handling the case where a new comment is added after
                     --  a sequence of ");" and separated by a blank line from
                     --  a code line. In this case the alignment will be based
                     --  on the following line indentation.

                  elsif Prev_Indentation_Affect_Comments and then
                    Is_Blank_Line (Prev_ss (Src_Tok)) and then
                    Kind (New_Tok) = Enabled_LB_Token and then
                    Kind (Prev (New_Tok)) = ';' and then
                    Kind (Prev (Prev (New_Tok))) = ')'
                  then

                     --  Preserve the maximal indentation level when
                     --  the current ');' is followed by an IF statement
                     --  part or a CASE statement part or a BEGIN or END
                     --  keyword align with the following line of code otherwise
                     --  (i.e., keep the after indentation level)

                     if Next_Is_Begin (New_Tok) or else
                       Next_Is_End (New_Tok) or else
                       Next_Is_Inside_If (New_Tok) or else
                       Next_Is_Inside_Case (New_Tok)
                     then
                        Indentation := Natural'Max (Indentation,
                                                    Before_Indentation);
                     end if;

                     --  Preserve the following type indentation level when
                     --  the current ';' is followed by a type declaration

                  elsif Prev_Indentation_Affect_Comments and then
                    Is_Blank_Line (Prev_ss (Src_Tok)) and then
                    Kind (New_Tok) = Enabled_LB_Token and then
                    Kind (Prev (New_Tok)) = ';' and then
                    Next_Is_Type (Next_ss (New_Tok))
                  then

                     Indentation := After_Indentation;

                     --  This is for the situation where no Enabled_LB_Token
                     --  is present and the indentation information
                     --  is held by Disabled_LB_Token. In this situation,
                     --  since if this information affects comments then this
                     --  information should be used to compute the right
                     --  indentation value.

                  elsif Prev_Indentation_Affect_Comments and then
                    (Kind (Src_Tok) = Other_Whole_Line_Comment
                     or else Kind (Src_Tok) = Fillable_Comment)
                    and then (Kind (New_Tok) = Ident
                              or else Kind (New_Tok) = Res_Others
                              or else Kind (New_Tok) = Res_When
                              or else Kind (New_Tok) = Numeric_Literal)
                    and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                    and then Kind (Prev (Prev (Prev (New_Tok)))) in  ',' | ';'
                  then

                     --  The corrected indentation is based on an already set
                     --  value on the LB Indentation and it is used to adjust
                     --  the Cur_Indentation in case of a parathesized context.

                     declare
                        P : constant Tokn_Cursor := Prev (Prev (New_Tok));
                        LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (P));
                     begin
                        if LB.Enabled and then LB.Affects_Comments then
                           --  Do nothing when ');' is detected before
                           if not
                             (Kind (Prev (Prev (Prev (New_Tok)))) = ';' and then
                              Kind (Prev (Prev (Prev (Prev (New_Tok))))) = ')')
                           then
                              Corrected_Indentation := LB.Indentation;
                              Indentation := Corrected_Indentation;
                           end if;
                        else
                           Indentation :=
                             Natural'Max (Indentation,
                                          Before_Indentation);
                        end if;
                     end;

                  --  Whole line comment between last aggregate parameter and
                  --  closing parathesis should be aligned with the others
                  --  aggregates and closing parenthesis should have the right
                  --  indentation too.

                  elsif Prev_Indentation_Affect_Comments
                    and then (Kind (Src_Tok) = Other_Whole_Line_Comment
                              or else Kind (Src_Tok) = Fillable_Comment)
                    and then Kind (Prev (New_Tok)) = Ident
                    and then Kind (New_Tok) = ')'
                    and then Kind (Next (New_Tok)) = ';'
                    and then
                      Kind (Prev (Prev (Prev (New_Tok)))) = Disabled_LB_Token
                  then
                     declare
                        P : constant Tokn_Cursor :=
                          Prev (Prev (Prev (New_Tok)));
                        LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (P));
                     begin
                        if LB.Enabled and then LB.Affects_Comments then
                           Indentation := LB.Indentation;
                           Corrected_Indentation := Indentation;
                        end if;
                     end;

                  --  Preserve the following type indentation level when
                  --  the current ';' is followed by an action
                  elsif Prev_Indentation_Affect_Comments and then
                    Kind (Src_Tok) = Other_Whole_Line_Comment and then
                    Kind (New_Tok) = Enabled_LB_Token and then
                    Kind (Prev (New_Tok)) = ';' and then
                    Next_Is_Action (New_Tok)
                  then
                     Indentation := After_Indentation;

                  --  The next case deals with comments between the
                  --  Param_Spec and the return keyword.
                  --
                  --  function Foo
                  --    (--  Comment about A
                  --     A : Integer;
                  --     --  Comment about B
                  --     B : Float)
                  --     --  Why put a comment here?
                  --     return Boolean;
                  elsif Prev_Indentation_Affect_Comments
                    and then (Kind (Src_Tok) in
                                Other_Whole_Line_Comment | Fillable_Comment)
                    --  Here look for the sequence:
                    --  )
                    --  Line_Break_Token
                    --  Spaces
                    --  return
                    and then (Kind (New_Tok) = Res_Return
                              and then Kind (Prev (New_Tok)) = Spaces
                              and then Kind (Prev (Prev (New_Tok))) in
                                         Line_Break_Token
                              and then Kind (Prev (Prev (Prev (New_Tok)))) =
                                         ')')
                  then
                     declare
                        P : constant Tokn_Cursor := Prev (Prev (New_Tok));
                        LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (P));
                     begin
                        Indentation := LB.Indentation;
                        Corrected_Indentation := Indentation;
                     end;

                  --  The next case deals with a comment right after the with
                  --  keyword of an aspect clause. This comment must be
                  --  affected by the with keyword identation.
                  elsif (Kind (Src_Tok) in
                           Other_Whole_Line_Comment | Fillable_Comment)
                    --  Here look for the sequence:
                    --  with
                    --  Line_Break_Token
                    --  Spaces
                    and then (Kind (New_Tok) = Enabled_LB_Token
                              and then Kind (Prev (New_Tok)) = Res_With
                              and then Kind (Prev (Prev (New_Tok))) in
                                         Spaces)
                  then
                     declare
                        LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (New_Tok));
                     begin
                        Indentation := LB.Indentation;
                        Corrected_Indentation := Indentation;
                     end;

                  elsif Prev_Indentation_Affect_Comments
                    and then Kind (New_Tok) = Res_Record
                    and then Kind (Src_Tok) in
                      Other_Whole_Line_Comment | Fillable_Comment
                    and then
                      Kind (Prev (Prev (Prev (Src_Tok)))) = End_Of_Line_Comment
                    and then
                      Kind (Prev_ss (Prev (Prev (Prev (Src_Tok))))) = Res_Is
                    and then Kind (Next (New_Tok)) = Enabled_LB_Token
                  then
                     --  This is the case of a type declaration having an EOL
                     --  comment and a whole line or fillable comment between
                     --  "is" and "record" keyword.
                     --  In this case, the line comment should be aligned on
                     --  the type indentation.

                     Indentation := Before_Indentation;
                  else
                     Indentation := Natural'Max (Indentation,
                                                 Before_Indentation);
                  end if;
               end if;
            end if;

            --  Make sure Indentation is a multiple of PP_Indentation; otherwise
            --  style checking complains "(style) bad column".

            Indentation := Good_Column (PP_Indentation (Cmd), Indentation);
            pragma Assert ((Indentation mod PP_Indentation (Cmd)) = 0);

            --  If we're inside something parenthesized, add an extra level
            --  (Note : this is a particular alignment case handled to avoid
            --   regressions in internal-testsuite tests MB20-050 and R504-012)

            if Kind (New_Tok) = ')' and then
              (Kind (Next (Next (New_Tok))) = Res_Is
               or else (Kind (Next (New_Tok)) = Disabled_LB_Token and then
                        Kind (Next (Next (New_Tok))) = Spaces and then
                        Kind (Next (Next (Next (New_Tok)))) = Res_Return))
            then
               Indentation := Indentation + PP_Indentation (Cmd);
            end if;

            --  V111-001:
            --  This is needed to take into account the paranthesis level and
            --  add 1 to be inside of the paranthesis for the next line
            --  indentation (see above for Corrected_Indentation value)
            if Corrected_Indentation /= 0
              and then Corrected_Indentation /= Indentation
            then
               Indentation := Corrected_Indentation;
            end if;

            Set_Cur_Indent;

            if Is_Blank_Line (Prev_ss (Src_Tok))
              or else Kind (Last (New_Tokns'Access)) /= Enabled_LB_Token
            then
               if Arg (Cmd, Source_Line_Breaks) then
                  declare
                     P : Tokn_Cursor := Last (New_Tokns'Access);
                  begin
                     while Kind (P) in
                       Line_Break_Token | End_Of_Line_Comment | Spaces
                     loop
                        Prev (P);
                     end loop;

                     if Kind (P) not in Whole_Line_Comment | ',' then
                        Append_Temp_Line_Break
                          (Lines_Data_P,
                           Org => "Append_Temp_ in Insert_Whole_Line_Comment");
                     end if;
                  end;
               else
                  Append_Temp_Line_Break
                    (Lines_Data_P,
                     Org => "Append_Temp_ in Insert_Whole_Line_Comment");
               end if;
            end if;

            loop
               --  ???Handle blank lines here, too?
               Insert_Comment_Text (Lines_Data_P, Cmd, Src_Tok);
               Next_ss (Src_Tok);
               pragma Assert (Kind (Src_Tok) in EOL_Token);
               Next_ss (Src_Tok);

               exit when Kind (Src_Tok) not in
                 Special_Comment | Fillable_Comment | Other_Whole_Line_Comment;

               Set_Cur_Indent;
               Append_Temp_Line_Break
                 (Lines_Data_P,
                  Org => "Append_Temp_ in Insert_Whole_Line_Comment 2");
            end loop;

            --  A line break is needed after the comment. If one is not already
            --  there, add one.

            if Kind (New_Tok) = Enabled_LB_Token then
               while All_LB (Enabled_LBI (Enabled_Cur_Line)).Tok /= New_Tok loop
                  Enabled_Cur_Line := Enabled_Cur_Line + 1;
               end loop;
            end if;

            declare
               LB : Line_Break renames All_LB (Enabled_LBI (Enabled_Cur_Line));
               pragma Assert (if Kind (New_Tok) = Enabled_LB_Token then
                 LB.Tok = New_Tok);
               pragma Assert
                 (if LB_Tok (LB) = New_Tok then
                    Kind (New_Tok) = Enabled_LB_Token);
               pragma Assert
                 (if not Partial_Gnatpp then
                    (if LB_Tok (LB) = Prev (New_Tok) then
                         (if not Arg (Cmd, Source_Line_Breaks) then
                                 Kind (Prev (New_Tok)) = Disabled_LB_Token)));
            begin
               if LB.Tok = New_Tok then

                  --  The inserted LB.Indentation is set to 0 when the LB is
                  --  added in case of --insert-blank-lines switch usage
                  --  Take the Cur_Indentation value as reference in that case
                  --  for the current line break indentation.

                  if Insert_Blank_Lines (Cmd) and then
                    LB.Indentation = 0
                  then

                     if LB.Indentation < Cur_Indentation then
                        LB.Indentation := Cur_Indentation;

                     elsif Cur_Indentation = 0 and then
                       LB.Indentation < Indentation
                     then
                        --  Update the indentation level for current line break
                        --  to the expected level after this comment.
                        LB.Indentation := Indentation;
                     end if;

                  end if;

                  Append_Tokn
                    (New_Tokns, False_End_Of_Line, "whole line extra");
                  --  This is needed because every comment in New_Tokns must
                  --  be followed by EOL_Token.

               elsif (Kind (New_Tok) = Ident
                      or else Kind (New_Tok) = Res_Others
                      or else Kind (New_Tok) = Numeric_Literal)
                 and then Kind (New_Tok) = Kind (Src_Tok)
                 and then Kind (Prev (Prev (Src_Tok))) in True_End_Of_Line
                 and then (Kind
                           (Prev (Prev (Prev (Src_Tok)))) =
                             Other_Whole_Line_Comment
                           or else Kind (Prev (Prev (Prev (Src_Tok)))) =
                             Fillable_Comment)
                 and then Kind (Prev (Prev (Prev (New_Tok)))) in ',' | ';'
                 and then Kind (Prev (Prev (New_Tok))) in Disabled_LB_Token
               then
                  --  V111-001
                  --  Avoid adding an empty line after a whole line comment
                  --  between two parameters
                  --  A (X => 1,
                  --     --  whole line comment
                  --     False);
                  --  In case of paranthesized expressions (like paramaters of
                  --  and action call) the indentation must be corrected if this
                  --  was not already taken as set in the Disabled_LB_Token
                  --  Indentation field.
                  null;

               elsif Kind (Prev (New_Tok)) in Spaces
                 and then Kind (Prev (Prev (New_Tok))) in Disabled_LB_Token
                 and then Kind (Prev (Prev (Prev (New_Tok)))) in ','
                 and then Kind (Next_ss (New_Tok)) in Tab_Token
                 and then Kind (Next (Next_ss (New_Tok))) in Arrow
                 and then not (Kind (Src_Tok) in True_End_Of_Line
                               and then Kind (Prev (Src_Tok)) in
                                 True_End_Of_Line)
               then

                  --  U329-016
                  --  Avoid adding an empty line after a whole line comment
                  --  betweeen two value assignments of enumerations, unless
                  --  the whole line comment is itselft surrounded by empty
                  --  lines.
                  --
                  --  ...
                  --  literal_foo => literal_foo_value,
                  --  --  Some comment (does not add a LB here)
                  --  literal_bar => literal_bar_value,
                  --  ...
                  --
                  --  ...
                  --  literal_foo => literal_foo_value,
                  --
                  --  --  Some comment (but adds a LB here)
                  --
                  --  literal_bar => literal_bar_value,
                  --  ...
                  null;

               elsif Arg (Cmd, Source_Line_Breaks)
                 and then not Partial_Gnatpp
                 and then
                   ((Kind (Src_Tok) in True_End_Of_Line
                     and then Kind (Prev (Src_Tok)) in True_End_Of_Line
                     and then Kind (Prev (Prev (Src_Tok))) in
                       Other_Whole_Line_Comment | Fillable_Comment
                     and then Kind (Next_ss (Src_Tok)) = Kind (New_Tok)
                     and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                     and then Kind (Prev (Prev (Prev (New_Tok)))) in ',')
                    or else
                      (Kind (Src_Tok) = Kind (New_Tok)
                       and then
                         ((Kind (Prev (Prev (Src_Tok))) =
                             Other_Whole_Line_Comment
                           and then Kind (Prev (Prev (Prev (Src_Tok)))) in
                             True_End_Of_Line)
                          or else
                            (Kind (Prev (Prev (Src_Tok))) in True_End_Of_Line
                             and then Kind (Prev (Prev (Prev (Src_Tok)))) =
                               Fillable_Comment))
                       and then Kind (Next_ss (Src_Tok)) in Ident | ',' | ')'
                       and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                       and then Kind (Prev (Prev (Prev (New_Tok)))) in
                         Res_Is | ',')
                    or else
                      (Kind (Src_Tok) in True_End_Of_Line
                       and then Kind (Prev (Prev (Src_Tok))) in
                           Other_Whole_Line_Comment | Fillable_Comment
                       and then Kind (Prev (Prev (Prev (Src_Tok)))) in
                         True_End_Of_Line | Spaces
                       and then Kind (New_Tok) = '('
                       and then Kind (New_Tok) = Kind (Next_ss (Src_Tok))
                       and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                       and then Kind (Prev (Prev (Prev (New_Tok)))) in
                         Res_Is | ',')
                    or else
                      (Kind (Src_Tok) in True_End_Of_Line
                       and then Kind (Prev (Prev (Src_Tok))) =
                           Fillable_Comment
                       and then Kind (Prev (Prev (Prev (Src_Tok)))) = Spaces
                       and then Kind (New_Tok) = '('
                       and then Kind (New_Tok) = Kind (Next_ss (Src_Tok))
                       and then Kind (Prev (Prev (New_Tok))) = Spaces
                       and then Kind (Prev (Prev (Prev (New_Tok)))) =
                         Disabled_LB_Token)
                    or else
                      (Kind (Src_Tok) in True_End_Of_Line
                       and then Kind (Prev (Prev (Src_Tok))) =
                           Other_Whole_Line_Comment
                       and then Kind (Prev (Prev (Prev (Src_Tok)))) in
                                  True_End_Of_Line
                       and then Kind (New_Tok) = Res_Then
                       and then Kind (New_Tok) = Kind (Next_ss (Src_Tok))
                       and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                       and then Kind (Prev (Prev (Prev (New_Tok)))) =
                         Character_Literal))
               then
                  --  The Disabled_LB_Token was enabled and will match the
                  --  True_End_Of_Line between the source token and a blank line
                  null;

               elsif Kind (LB.Tok) = Disabled_LB_Token
                 and then Kind (Src_Tok) = Res_Record
                 and then Kind (Src_Tok) = Kind (New_Tok)
                 and then Kind (Prev (Prev_ss (Src_Tok))) = Fillable_Comment
                 and then
                   Kind (Prev (Prev_ss (Prev (Prev (Prev_ss (Src_Tok)))))) =
                     End_Of_Line_Comment
                 and then
                   Kind (Prev_ss (Prev
                         (Prev_ss (Prev (Prev (Prev_ss (Src_Tok))))))) = Res_Is
               then
                  null;

               else
                  Cur_Indentation := Indentation;

                  if Partial_Gnatpp
                    and then Kind (New_Tok) = End_Of_Input
                    and then Kind (Src_Tok) = End_Of_Input
                  then
                     --  For partial formatting no need to add an additional
                     --  line break after comment.

                     null;
                  else
                     Append_Temp_Line_Break
                       (Lines_Data_P,
                        Org => "Append_Temp_ in Insert_Whole_Line_Comment 3");
                  end if;

               end if;
            end;
            Reset_Indentation;
         end Insert_Whole_Line_Comment;

         procedure Insert_Preprocessor_Directive is
         begin
            --  The libadalang parser simply ignores preprocessor directives,
            --  so it will produce a more-or-less reasonable tree if the input
            --  text is syntactically legal after deleting the directives. The
            --  Convert_Tree_To_Ada phase will format based on that tree, so
            --  something like:
            --
            --     package P is
            --     #IF SOMETHING
            --        X : constant Integer := 123;
            --     #ELSE
            --        X : constant Integer := 456;
            --     #END IF;
            --     end P;
            --
            --  will be formatted as if it were:
            --
            --     package P is
            --        X : constant Integer := 123;
            --        X : constant Integer := 456;
            --     end P;
            --
            --  Then this procedure reinserts the directives.
            --
            --  So there is a limitation: if deleting the directives does not
            --  produce text that libadalang can parse, then pretty printing
            --  will fail.

            Cur_Indentation := Sloc (Src_Tok).Col - 1; -- Keep as in input
            Append_Temp_Line_Break
              (Lines_Data_P,
               Org => "Append_Temp_ Preprocessor_Directive");
            Append_Tokn (New_Tokns, Src_Tok);
            Next (Src_Tok);
            Reset_Indentation;
         end Insert_Preprocessor_Directive;

         procedure Insert_Private is
         begin
            --  The previous line break is just before "end";
            --  that's the indentation we want for "private".

            Cur_Indentation :=
              All_LB (Syntax_LBI (Syntax_Cur_Line - 1)).Indentation;

            if not Arg (Cmd, Source_Line_Breaks) then
               Append_Temp_Line_Break
                 (Lines_Data_P, Org => "Append_Temp_ private 1");
            end if;
            Append_Tokn (New_Tokns, Res_Private);

            --  If Source_Line_Breaks, then avoid running "private end"
            --  together into "privateend".

            if Arg (Cmd, Source_Line_Breaks) then
               Append_Spaces (New_Tokns, Count => 1);
            else
               Append_Temp_Line_Break
                 (Lines_Data_P, Org => "Append_Temp_ private 2");
            end if;

            Reset_Indentation;
            Next_ss (Src_Tok);
         end Insert_Private;

         function Is_Named_Assoc return Boolean is
            pragma Assert (Kind (New_Tok) = Ident);
            Crt_Tok : Tokn_Cursor := New_Tok;
         begin
            --  It is expected to have something like "X => val1" or
            --  "X | Y | Z => val2" or "others => val"
            --  Return false otherwise.

            Crt_Tok := New_Tok;
            while Kind (Crt_Tok) /= Arrow loop
               if Kind (Crt_Tok) = Ident or Kind (Crt_Tok) = Res_Others or
                 (Kind (Crt_Tok) in Spaces | Tab_Token | Disabled_LB_Token) or
                 Kind (Crt_Tok) = '|'
               then
                  Crt_Tok := Next (Crt_Tok);
               else
                  return False;
               end if;
            end loop;

            return Kind (Crt_Tok) = Arrow;
         end Is_Named_Assoc;

         Qual_Nesting : Natural := 0;
      --  Count the nesting level of qualified expressions containing aggregates
      --  with extra parentheses.

      --  Start of processing for Insert_Comments_And_Blank_Lines

      begin
         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "before Insert_Comments_And_Blank_Lines"));
         --  ???At this point, we might need another pass to insert hard line
         --  breaks after end-of-line comments, so they will be indented properly.
         --  Or better yet, insert the EOL comments, with tabs and soft line break
         --  before, hard line break after.

         Collect_Line_Breaks
           (Lines_Data_P, Saved_New_Tokns,
            Do_All => True, Do_Enabled => True, Do_Syntax => True,
            First_Time => False);
         Append_Tokn (Pending_Tokns, Start_Of_Input);

         pragma Assert (Is_Empty (New_Tokns));
         Scanner.Clear (New_Tokns);
         Append_Tokn (New_Tokns, Start_Of_Input);

         --  Skip initial EOL_Token token
         pragma Assert (Kind (New_Tok) = Enabled_LB_Token);

         if Arg (Cmd, Source_Line_Breaks) then
            All_LB (Line_Break_Token_Index (New_Tok))
              .Source_Line_Breaks_Enabled := True;
         end if;

         New_To_Newer;
         New_Line_Start_Out := New_Tok;
         pragma Assert (Is_Empty (Temp_LBI));

         --  The two sequences Src_Tokns and Out_Tokns should be identical,
         --  with some exceptions where mismatches are possible. The code below
         --  to insert comments depends on this fact. We step through the two
         --  sequences, copying text into Buffer, and detect any token mismatch.
         --  The allowed mismatches are:
         --
         --     The Out sequence has no comments, so when we detect a mismatch and
         --     the source one is a comment, that's where we insert the comment.
         --
         --     The sequences may have blank lines in different places.
         --
         --     We normalize "end;" to "end Some_Name;"
         --
         --     We normalize by removing "private" from a package (etc) when there
         --     is nothing in the private part. We put the "private" back in here.
         --
         --     We normalize a qualified expression with unnecessary parentheses
         --     containing an aggregate. That is "T'((X, Y, Z))" is normalized to
         --     "T'(X, Y, Z)", where "(X, Y, Z)" is an aggregate. We pretty-much
         --     have to do that, because ASIS provides no way to distinguish these
         --     two forms.
         --
         --     There is a mode in which we insert underscores in numeric
         --     literals, as in 12_345_678.
         --
         --     Allowed Replacements of Characters (see RM-J.2). We normalize "!"
         --     to "|" when used as a delimiter. The other allowed replacements
         --     (: for # and % for ") are not normalized.
         --
         --  Any other mismatch is considered to be a bug.

         loop
            pragma Assert (Kind (New_Tok) not in Comment_Kind);
            Manage_Paren_Stack;

            --  The order of the if/elsif's below is important in some
            --  cases. Blank lines must be handled late, even if they match.
            --  End_Of_Line_Comments must be handled before blank lines,
            --  because they need to appear at the end of the preceding line.
            --  Whole_Line_Comments must be handled after blank lines, because
            --  the blank line should precede the comment.

            if Kind (Src_Tok) not in EOL_Token
              and then
              (Match (Src_Tok, New_Tok)
               or else (Kind (Src_Tok) = '!' and then Kind (New_Tok) = '|'))
            then
               exit when Kind (Src_Tok) = End_Of_Input;
               --  i.e. exit when both Src and Out are at end of input

               pragma Assert (Kind (New_Tok) /= Enabled_LB_Token);
               --  No need to set New_Line_Start_Out

               Next_ss (Src_Tok);
               New_To_Newer;

            else
               --  Check for "end;" --> "end Some_Name;" case
               if Kind (Src_Tok) = ';'
                 and then
                   Kind (Prev_Lexeme (Src_Tok)) = Res_End
                 and then Sname_83 (New_Tok)
               then
                  loop -- could be "end A.B.C;"
                     New_To_Newer;

                     exit when Kind (New_Tok) /= '.';

                     New_To_Newer ('.');
                     pragma Assert (Sname_83 (New_Tok));
                  end loop;
                  pragma Assert
                    (Disable_Final_Check or else
                       Kind (Src_Tok) in ';' | EOL_Token | Comment_Kind);

               --  Check for "end Some_Name;" --> "end;" case. This only happens
               --  when the --no-end-id switch was given. Here, the name was
               --  present in the source, so we insert it.

               elsif not Arg (Cmd, End_Id)
                 and then Kind (New_Tok) = ';'
                 and then
                   Kind (Prev_Lexeme (New_Tok)) = Res_End
                 and then Kind (Src_Tok) in Ident | String_Lit
               then
                  Append_Tokn (New_Tokns, Spaces, Name_Space);
                  loop -- could be "end A.B.C;"
                     Append_Tokn (New_Tokns, Src_Tok);
                     Next_ss (Src_Tok);

                     exit when Kind (Src_Tok) /= '.';

                     Append_Tokn (New_Tokns, Src_Tok);
                     Next_ss (Src_Tok);
                     pragma Assert
                       (Kind (Src_Tok) in Ident | String_Lit);
                  end loop;
                  pragma Assert
                    (Disable_Final_Check or else Kind (Src_Tok) = ';');

               --  Check for "private end" --> "end" case, with a possible
               --  comment between "private" and "end".

               elsif Kind (Src_Tok) = Res_Private
                 and then Kind (New_Tok) = Res_End
               then
                  pragma Assert
                    (Disable_Final_Check
                       or else Kind (Next_Lexeme (Src_Tok)) = Res_End);
                  Insert_Private;

               --  Check for "T'((X, Y, Z))" --> "T'(X, Y, Z)" case

               elsif Kind (Src_Tok) = '('
                 and then Kind (Prev_Lexeme (Src_Tok)) = '('
                  --???Also check that the one before that is a tick!
               then
                  Qual_Nesting := Qual_Nesting + 1;
                  Append_Tokn (New_Tokns, Src_Tok);
                  Next_ss (Src_Tok);

               elsif Qual_Nesting > 0
                 and then Kind (Src_Tok) = ')'
                 and then Kind (Prev_Lexeme (Src_Tok)) = ')'
               then
                  Qual_Nesting := Qual_Nesting - 1;
                  Append_Tokn (New_Tokns, Src_Tok);
                  Next_ss (Src_Tok);

               elsif Kind (Src_Tok) = End_Of_Line_Comment then
                  Insert_End_Of_Line_Comment;

               --  If the source has a blank line at this point, send it to the
               --  output (unless Insert_Blank_Lines is True, in which case we
               --  want to ignore blank lines in the input, since a previous
               --  phase inserted them in the "right" place). But avoid
               --  multiple blank lines (unless either Preserve_Line_Breaks or
               --  Preserve_Blank_Lines is True) and blank lines just before
               --  End_Of_Input.

               elsif Is_Blank_Line (Src_Tok)
                 and then not Arg (Cmd, Source_Line_Breaks)
               then
                  --  Src_Tok is the second in a series of two or more
                  --  EOL_Tokens.

                  declare
                     pragma Assert (Kind (Prev_ss (Src_Tok)) in EOL_Token);
                     Prev_Prev_Tok_Kind : constant Token_Kind :=
                       Kind (Prev_ss (Prev_ss (Src_Tok)));
                  begin
                     loop
                        Next_ss (Src_Tok);
                        exit when Kind (Src_Tok) not in EOL_Token
                          or else Preserve_Blank_Lines (Cmd);
                     end loop;
                     declare
                        Next_Tok_Kind : constant Opt_Token_Kind :=
                          (if At_Last (Src_Tok)
                             then Nil
                             else Kind (Next_ss (Src_Tok)));
                     begin
                        if Preserve_Blank_Lines (Cmd)
                          or else (not Insert_Blank_Lines (Cmd)
                                     and then Kind (Src_Tok) /= End_Of_Input)
                          or else Prev_Prev_Tok_Kind in Comment_Kind
                          or else Next_Tok_Kind in Comment_Kind
                        then
                           Append_Temp_Line_Break
                             (Lines_Data_P, Org => "Append_Temp_ blank line");
                        end if;
                     end;
                  end;

               --  Normally, we simply ignore EOL_Token in the input. But for
               --  --preserve-line-breaks or --source-line-breaks mode, if we
               --  see a line break in the input that is not yet in the output,
               --  we copy it over. We set the indentation to take into account
               --  surrounding indentation, plus line continuation if
               --  appropriate, plus "("-related indentation. If the next
               --  character in the output is already ' ', we subtract one from
               --  the indentation to make up for that. (There can never be two
               --  in a row.)

               elsif Kind (Src_Tok) in EOL_Token then
                  pragma Assert
                    (not Is_Blank_Line (Src_Tok)
                     or else Arg (Cmd, Source_Line_Breaks));

                  Next_ss (Src_Tok);

                  if Arg (Cmd, Preserve_Line_Breaks) then

                     declare
                        Indentation : Natural := Before_Indentation;
                     begin
                        if Extra_Indent_For_Preserved_Line then
                           --  See Manage_Paren_Stack
                           Indentation :=
                             Indentation + PP_Indent_Continuation (Cmd);
                        end if;

                        if Indentation /= 0 then
                           --  Handle cases when line break just after Spaces or
                           --  just after a '('
                           if Kind (Prev (New_Tok)) = Spaces then
                              --  If an affectation is preceeding the Spaces
                              --  then nothing to do
                              if Kind
                                (Prev (Prev (Prev (New_Tok)))) /= Colon_Equal
                              then
                                 Indentation := Indentation - 1;
                              end if;

                           elsif Kind (Prev (New_Tok)) = '(' then
                              Indentation := Indentation - 1;
                           end if;
                        end if;

                        if Kind (New_Tok) /= Enabled_LB_Token then

                           --  Check if the current indentation is for a named
                           --  aggregate element association and adjust
                           --  indentation if needed
                           if Kind (New_Tok) = Ident and then Is_Named_Assoc
                           then
                              --  Indentation needs to be rectified only if
                              --  this is not the first element of a named
                              --  association
                              if Kind (Prev (New_Tok)) /= '(' then
                                 Indentation := Indentation + 1;
                              end if;
                           end if;

                           --  If the EOL token should be inserted and the next
                           --  element is a paranthesized expression inside a
                           --  subprogram call or any other structure, the
                           --  indentation level should be recomputed to allign
                           --  the parathesis with the previous opened
                           --  paranthesis instead of the current paren position
                           --  (indent) of the Paren_Stack as Cur_Indentation.
                           --  (the pattern that we are looking for to do that is
                           --  "(.., (.." in a subprogram call with positional
                           --  parameters).

                           if not Is_Empty (Paren_Stack) and then
                             In_Paren_Context and then
                             Kind (New_Tok) = '(' and then
                             Kind (Prev (Prev (Prev (New_Tok)))) = ','
                           then
                              Indentation :=
                                Indentation + Before_Paren_Indent_For_Preserve;

                              Update_L_Paren_Indent_For_Preserve
                                   (Before_Paren_Indent_For_Preserve);

                           else
                              --  If we have an aggregate or parathesized
                              --  expression just after a line break we should
                              --  not take into account parenthesis position
                              if not Is_Empty (Paren_Stack) and then
                                Kind (New_Tok) = '(' and then
                                Kind (Prev (Prev (Prev (New_Tok)))) = Colon_Equal
                              then
                                 pragma Assert
                                   (not Extra_Indent_For_Preserved_Line);
                                 --  Keep the PP_Indentation as current
                                 --  indentation reference
                                 Indentation :=
                                   Indentation + PP_Indent_Continuation (Cmd);
                              else
                                 Indentation :=
                                   Indentation + L_Paren_Indentation_For_Preserve;
                              end if;

                           end if;

                           Cur_Indentation := Indentation;

                           Append_Temp_Line_Break
                             (Lines_Data_P,
                              Org => "Append_Temp_ Preserve_Line_Breaks");

                           Reset_Indentation;
                           New_Line_Start_Out := New_Tok;
                        end if;
                     end;

                  elsif Arg (Cmd, Source_Line_Breaks) then

                     if Kind (New_Tok) in Line_Break_Token then
                        declare
                           LB : Line_Break renames
                             All_LB (Line_Break_Token_Index (New_Tok));
                        begin
                           --  If we have a line break with
                           --  Source_Line_Breaks_Enabled already True, it means
                           --  we already did New_To_Newer in the 'else' below
                           --  for a previous EOL_Token, so create a new one
                           --  with the same indentation.

                           if LB.Source_Line_Breaks_Enabled then
                              Cur_Indentation := LB.Indentation;
                              Append_Temp_Line_Break
                                (Lines_Data_P,
                                 Org => "Append_Temp_ Source_Line_Breaks 1");

                              Reset_Indentation;
                              New_Line_Start_Out := New_Tok;

                           --  Source_Line_Breaks_Enabled is False, so tell
                           --  New_To_Newer to use this line break.

                           else
                              LB.Enabled := True;
                              LB.Source_Line_Breaks_Enabled := True;

                              if Kind (Src_Tok) in Whole_Line_Comment then
                                 New_To_Newer;
                                 New_Line_Start_Out := New_Tok;
                              end if;
                           end if;
                        end;

                     --  There is no line break in New_Tokns corresponding to
                     --  the EOL_Token in the source, so create a new one using
                     --  Append_Temp_Line_Break.

                     else
                        declare
                           Indentation : Natural := 0;
                           P : Tokn_Cursor := New_Tok;
                        begin

                           case Kind (New_Tok) is
                              when Res_Is | Comment_Kind =>
                                 Indentation := Before_Indentation;

                              when others =>
                                 while Kind (P) not in Line_Break_Token loop
                                    P := Prev (P);
                                 end loop;
                                 declare
                                    LB : Line_Break renames
                                      All_LB (Line_Break_Token_Index (P));
                                 begin
                                    if Kind (P) = Disabled_LB_Token
                                      and then LB_Needs_To_Be_Enabled (Src_Tok)
                                    then
                                       LB.Enabled := True;
                                       LB.Affects_Comments := True;
                                    end if;

                                    Indentation := LB.Indentation;
                                 end;
                           end case;

                           Cur_Indentation := Indentation;
                           Append_Temp_Line_Break
                             (Lines_Data_P,
                              Org => "Append_Temp_ Source_Line_Breaks 2");
                           Reset_Indentation;
                           New_Line_Start_Out := New_Tok;
                        end;
                     end if;
                  end if;

               elsif Kind (Src_Tok) in Whole_Line_Comment then

                  --  In the context of call parameters or aggregate association
                  --  having a whole line comment inserted in between the
                  --  elements the present Disabled_LB_Token should be enabled
                  --  and the indentation needs to be updated in the current
                  --  token context
                  --  In a paranthesized context, the indentation level should
                  --  be increased by 1 related to the last left paranthesis
                  --  position
                  if (Kind (Src_Tok) = Other_Whole_Line_Comment
                      or else Kind (Src_Tok) = Fillable_Comment)
                    and then (Kind (New_Tok) = Ident
                              or else Kind (New_Tok) = Res_Others
                              or else Kind (New_Tok) = Res_When
                              or else Kind (New_Tok) = Numeric_Literal
                              or else Kind (New_Tok) = ')')
                    and then Kind (Prev (Prev (New_Tok))) = Disabled_LB_Token
                    and then (Kind (Prev (Prev (Prev (New_Tok)))) = ','
                              or else Kind (Prev (Prev (Prev (New_Tok)))) = ';')
                    and then not
                      (Kind (Prev_ss (Src_Tok)) in True_End_Of_Line
                       and then
                         (Kind (Prev (Prev_ss (Src_Tok))) = End_Of_Line_Comment
                          or (Kind (Prev (Prev_ss (Src_Tok))) in
                                True_End_Of_Line
                              and
                                (not After_Last (Prev (Prev_ss (Src_Tok)))
                                 and then
                                 Kind (Prev (Prev (Prev_ss (Src_Tok)))) =
                                   End_Of_Line_Comment))))
                  then

                     declare
                        P : constant Tokn_Cursor := Prev (Prev (New_Tok));
                        LB : Line_Break renames
                          All_LB (Line_Break_Token_Index (P));
                     begin
                        LB.Enabled := True;
                        LB.Affects_Comments := True;

                        if not Is_Empty (Paren_Stack) then

                           --  Nothing to do here since these situations
                           --  are handled by other means
                           if Arg (Cmd, Comments_Unchanged)
                             or else
                               Kind
                                 (Prev (Prev (Prev (Prev (New_Tok))))) = ')'
                             or else Kind (Src_Tok) = Fillable_Comment
                           then
                              null;

                           elsif Arg (Cmd, Source_Line_Breaks) then
                              if Kind (New_Tok) = Res_When then
                                 Cur_Indentation := LB.Indentation;
                              elsif Kind (New_Tok) = Ident then
                                 Cur_Indentation :=
                                   L_Paren_Indentation_For_Preserve;
                              end if;

                           else
                              LB.Indentation :=
                                L_Paren_Indentation_For_Preserve + 1;

                              Cur_Indentation := LB.Indentation;
                           end if;
                        end if;
                     end;
                  end if;
                  Insert_Whole_Line_Comment;

               elsif Kind (Src_Tok) = Preprocessor_Directive then
                  Insert_Preprocessor_Directive;

               elsif Kind (New_Tok) = Enabled_LB_Token then
                  New_To_Newer;
                  New_Line_Start_Out := New_Tok;

               --  Else print out debugging information and crash. This
               --  avoids damaging the source code in case of bugs. However,
               --  if the Disable_Final_Check debug flag is set, try to
               --  continue by skipping one source token, or one output
               --  token.

               elsif Disable_Final_Check then
                  Next_ss (Src_Tok);
                  if At_Last (Src_Tok) then
                     goto Done;
                  end if;
               else
                  --  Handle specific cases in partial formatting where no
                  --  token mismatch raise is needed
                  if Partial_Gnatpp then
                     if Kind (Src_Tok) = ';'
                       and then Kind (Prev (Src_Tok)) in
                         Res_End | Res_Record | Res_Loop | Ident | ')'
                       and then Kind (New_Tok) = End_Of_Input
                     then
                        Next_ss (Src_Tok);
                     else
                        --  When enter here mostly an infinite loop situation
                        --  is detected in partial mode
                        raise Partial_Gnatpp_Error;
                     end if;

                  else
                     Raise_Token_Mismatch
                       ("Inserting", Lines_Data, Src_Buf, Src_Tok, New_Tok);
                  end if;
               end if;
            end if;

            if Kind (Src_Tok) not in Comment_Kind
              and then Sloc (Src_Tok).Line /= Sloc (Start_Line_Src_Tok).Line
            then
               Start_Line_Src_Tok := Src_Tok;
            end if;
         end loop;

         if Last_Pp_Off_On /= First (Src_Tokns'Access) then
            Pp_Off_Present := True;
         end if;

         pragma Assert (Is_Empty (Paren_Stack));
         pragma Assert (Cur_Indentation = Arg (Cmd, Initial_Indentation));
         pragma Assert (At_Last (Src_Tok));

         Append_Tokn (New_Tokns, End_Of_Input);
         Erase_LB_Toks (All_LB);
         Clear (Saved_New_Tokns);
         pragma Assert (Syntax_Cur_Line = Last_Index (Syntax_LBI) + 1);

         if not Partial_Gnatpp then
            pragma Assert (Enabled_Cur_Line = Last_Index (Enabled_LBI));
         end if;

         <<Done>> null;

         Clear (Temp_LBI);
         pragma Assert (Disable_Final_Check or else Qual_Nesting = 0);
         pragma Assert (At_Beginning (Src_Buf));
         Clear (Lines_Data.All_LBI);
         Clear (Enabled_LBI);
         Clear (Syntax_LBI);
      end Insert_Comments_And_Blank_Lines;

      procedure Insert_Indentation (Lines_Data_P : Lines_Data_Ptr;
                                    Cmd : Command_Line;
                                    Partial_Gnatpp : Boolean := False)
      is
         function Is_Action_Call_Parameter (Tok : Tokn_Cursor) return Boolean;
         --  Returns true if the line break token is part of an action call
         --  parameter..

         function Called_As_Parameter_Of_An_Action_Call (Tok : Tokn_Cursor)
                                                         return Boolean;
         --  Returns true if the line break token is part of an action call
         --  parameter and this call is a parameter of an other action call.

         function Enclosing_Open_Paren (Tok      : Tokn_Cursor;
                                        OP_Count : Natural) return Tokn_Cursor;
         --  Returns the enclosing parenthesis corresponding token.

         function Prev_LB_Indentation (Paren : Tokn_Cursor) return Positive;
         --  Returns the previous indentation of the 1st found line break before
         --  the given '(' token cursor parameter.

         procedure Adjust_Indentation_After_Comma (Tok : Tokn_Cursor);
         --  This function adjust the indentation level based on the count of
         --  the opened parenthesis between two enabled line breaks.
         --  This is used in order to adjust line breaks indentation in case
         --  of a call parameter.

         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         All_LB     : Line_Break_Vector renames Lines_Data.All_LB;

         function Enclosing_Open_Paren (Tok      : Tokn_Cursor;
                                        OP_Count : Natural) return Tokn_Cursor
         is
            Index   : constant Line_Break_Index := Line_Break_Token_Index (Tok);
            LB      : Line_Break renames All_LB (Index);
            Crt_Tok : Tokn_Cursor := Prev (Tok);

            Crt_OP_Count : Natural     := 0;
            Crt_CP_Count : Natural     := 0;
         begin
            pragma Assert (Kind (Tok) in Line_Break_Token
                           and then LB.Enabled
                           and then not Is_Nil (Crt_Tok));

            while not Is_Nil (Crt_Tok) loop
               if Kind (Crt_Tok) = ')' then
                  Crt_CP_Count := Crt_CP_Count + 1;

               elsif Kind (Crt_Tok) = '(' then
                  Crt_OP_Count := Crt_OP_Count + 1;
               end if;

               if Crt_OP_Count = OP_Count + 1 then
                  return Crt_Tok;
               end if;

               exit when Is_Nil (Crt_Tok)
                 or else
                   Kind (Crt_Tok)
                    in Res_For | Res_When | Res_Begin | Res_Is | Colon_Equal;

               Crt_Tok := Prev (Crt_Tok);
            end loop;

            return Nil_Tokn_Cursor;
         end Enclosing_Open_Paren;

         function Is_Action_Call_Parameter (Tok : Tokn_Cursor) return Boolean
         is
            Index : constant Line_Break_Index := Line_Break_Token_Index (Tok);
            LB      : Line_Break renames All_LB (Index);
            Crt_Tok : Tokn_Cursor := Prev (Tok);

            function Pattern_Found (Tok : Tokn_Cursor) return Boolean is
              ((Kind (Prev (Tok)) = Spaces
               and then Kind (Prev (Prev (Tok))) = Ident)
               or else
                 (Kind (Prev (Tok)) in Line_Break_Token
                  and then Kind (Prev (Prev (Tok))) = Ident));

            --  Returns True if the pattern "Ident/Spaces/'('" is found
         begin
            pragma Assert (Kind (Tok) in Line_Break_Token
                           and then LB.Enabled
                           and then not Is_Nil (Crt_Tok)
                           and then Kind (Crt_Tok) in Arrow | ',');

            while not Is_Nil (Crt_Tok) loop
               if Kind (Crt_Tok) = '(' then
                  if Pattern_Found (Crt_Tok) then
                     return True;
                  end if;
               elsif Kind (Crt_Tok) = ')' then

                  if Kind (Prev (Tok)) = ',' then
                     return False;
                  end if;

               end if;

               exit when Is_Nil (Crt_Tok)
                 or else
                   Kind (Crt_Tok)
               in Res_For | Res_When | Res_Begin | Res_Is | Colon_Equal |
                  Res_Package | Res_With | Start_Of_Input;

               Crt_Tok := Prev (Crt_Tok);
            end loop;

            return False;
         end Is_Action_Call_Parameter;

         function Called_As_Parameter_Of_An_Action_Call
           (Tok : Tokn_Cursor) return Boolean
         is
            Index : constant Line_Break_Index := Line_Break_Token_Index (Tok);
            LB      : Line_Break renames All_LB (Index);
            Crt_Tok : Tokn_Cursor := Prev (Tok);

            function Call_Pattern_Found (Tok : Tokn_Cursor) return Boolean is
              (not Is_Nil (Prev (Tok)) and then not Is_Nil (Prev (Prev (Tok)))
               and then ((Kind (Prev (Tok)) = Spaces
                          and then Kind (Prev (Prev (Tok))) = Ident)
                         or else
                         (Kind (Prev (Tok)) in Line_Break_Token
                          and then Kind (Prev (Prev (Tok))) = Ident)));

            Pattern_Found  : Boolean     := False;
            Call_Ident_Tok : Tokn_Cursor := Nil_Tokn_Cursor;
            Count          : Natural     := 0;
         begin
            pragma Assert (Kind (Tok) in Line_Break_Token
                           and then LB.Enabled
                           and then not Is_Nil (Crt_Tok)
                           and then Kind (Crt_Tok) in Arrow | ',');

            if not Is_Action_Call_Parameter (Tok) then
               return False;
            end if;

            while not Is_Nil (Crt_Tok) loop

               if Kind (Crt_Tok) = '(' then
                  if Call_Pattern_Found (Crt_Tok) then
                     Pattern_Found := True;
                     Call_Ident_Tok :=
                       (if not Is_Nil (Prev (Crt_Tok))
                           and then not Is_Nil (Prev (Prev (Crt_Tok)))
                           and then not Is_Nil (Prev (Prev (Prev (Crt_Tok))))
                        then Prev (Prev (Prev (Crt_Tok)))
                        else Nil_Tokn_Cursor);
                     exit;
                  end if;
               end if;

               exit when Is_Nil (Crt_Tok)
                 or else
                   Kind (Crt_Tok)
                     in Res_For | Res_When | Res_Begin | Res_Is | Colon_Equal
                        | Start_Of_Input;

               Crt_Tok := Prev (Crt_Tok);
            end loop;

            --  Look backward starting from the call to identify if it is a
            --  a parameter of an other action call
            if not Pattern_Found then
               return False;
            end if;

            Crt_Tok := Call_Ident_Tok;
            while not Is_Nil (Crt_Tok) loop
               case Kind (Crt_Tok) is
                  when ',' | Arrow =>
                     return True;
                  when ')' =>
                     Count := Count + 1;

                  when '(' =>
                     if Count = 0 then
                        if Call_Pattern_Found (Crt_Tok) then
                           return True;
                        end if;
                     else
                        Count := Count - 1;
                     end if;

                  when others => null;
               end case;

               exit when Is_Nil (Crt_Tok)
                 or else
                   Kind (Crt_Tok)
                   in Res_For | Res_When | Res_Begin | Res_Is | Colon_Equal
                      | Start_Of_Input;

               Crt_Tok := Prev (Crt_Tok);
            end loop;

            return False;
         end Called_As_Parameter_Of_An_Action_Call;

         function Is_Expected_Token (Tok : Tokn_Cursor) return Boolean
         is ((Kind (Prev (Tok)) in ',' | '&' | ';' | ''' | Arrow)
             or else (Kind (Prev (Tok)) = Ident and Kind (Next (Tok)) = '('));

         function Prev_LB_Indentation (Paren : Tokn_Cursor) return Positive
         is
            Crt_Tok : Tokn_Cursor := Paren;
         begin
            while not Is_Nil (Crt_Tok) loop

               if Kind (Crt_Tok) = Enabled_LB_Token then
                  declare
                     Index : constant Line_Break_Index :=
                       Line_Break_Token_Index (Crt_Tok);
                     LB : Line_Break renames All_LB (Index);
                  begin
                     return LB.Indentation;
                  end;
               end if;

               exit when Is_Nil (Crt_Tok)
                 or else
                   Kind (Crt_Tok)
                     in Res_For | Res_When | Res_Begin | Res_Is | Colon_Equal
                       | Start_Of_Input;
               Prev (Crt_Tok);
            end loop;

            return 1;
         end Prev_LB_Indentation;

         procedure Adjust_Indentation_After_Comma (Tok : Tokn_Cursor)
         is
            Index    : constant Line_Break_Index := Line_Break_Token_Index (Tok);
            LB       : Line_Break renames All_LB (Index);
            Crt_Tok  : Tokn_Cursor := Tok;

            OP_Count  : Natural := 0;
            CP_Count  : Natural := 0;
         begin
            pragma Assert (Kind (Tok) in Line_Break_Token and then LB.Enabled);
            if not Called_As_Parameter_Of_An_Action_Call (Crt_Tok) then
               return;
            end if;

            --  Look forward on the line after the current ',' followed by a
            --  LB and find its specificities in order to adjust the
            --  indentation value of the line break.

            while not After_Last (Crt_Tok) loop
               Next (Crt_Tok);

               case Kind (Crt_Tok) is
                  when Line_Break_Token =>
                     declare
                        Crt_Index  : constant Line_Break_Index :=
                          Line_Break_Token_Index (Crt_Tok);
                        Crt_LB     : Line_Break renames All_LB (Crt_Index);

                        EP         : Tokn_Cursor := Nil_Tokn_Cursor;
                        PLB_Indent : Natural     := 0;
                     begin
                        if Crt_LB.Enabled
                          and then Is_Expected_Token (Crt_Tok)
                        then
                           EP := Enclosing_Open_Paren (Crt_Tok, OP_Count);

                           if not Is_Nil (EP) then
                              PLB_Indent := Prev_LB_Indentation (EP);

                              --  The context is supposed to be a parenthesized
                              --  call and the indentation should be alligned
                              --  to the parenthesis position + 1.
                              --  The opened and closed parenthesis could
                              --  impact the alignement. So, will adjust
                              --  according to the different situations

                              if OP_Count = 0 and then CP_Count = 0 then
                                 null;

                              elsif OP_Count /= 0 and then CP_Count = 0 then
                                 LB.Indentation := LB.Indentation - OP_Count;

                              elsif CP_Count /= 0 and then OP_Count = 0 then
                                 null;

                              else
                                 if OP_Count = CP_Count then
                                    null;

                                 elsif CP_Count > OP_Count then
                                    LB.Indentation :=
                                      (if CP_Count - OP_Count >= 1 then
                                         (LB.Indentation -
                                              (CP_Count - OP_Count) + 1)
                                       else
                                          LB.Indentation);

                                 elsif OP_Count > CP_Count then
                                    LB.Indentation := LB.Indentation -
                                      (OP_Count - CP_Count);
                                 end if;
                              end if;

                              --  This part will handle the last line of the
                              --  parenthesized call. Sometime, after Split_Line
                              --  when more than one parenthesis is found in a
                              --  line and the max length is exceeded, when the
                              --  line is splitted the indentation is not
                              --  updated.

                              if Kind (Prev (Crt_Tok)) = ';'
                                and then OP_Count /= 0
                              then
                                 if LB.Indentation /= PLB_Indent + 1 then
                                    LB.Indentation := PLB_Indent + OP_Count + 1;
                                 end if;
                              end if;
                           end if;

                           exit;
                        end if;
                     end;

                  when '(' =>
                     OP_Count := OP_Count + 1;

                  when ')' =>
                     CP_Count := CP_Count + 1;

                  when others => null;
               end case;
            end loop;

         end Adjust_Indentation_After_Comma;

         Saved_New_Tokns : Scanner.Tokn_Vec renames Lines_Data.Saved_New_Tokns;
         New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
         Ignore : Boolean := Scanner.Move_Tokns
           (Target => Saved_New_Tokns, Source => New_Tokns);
         New_Tok : Tokn_Cursor := First (Saved_New_Tokns'Access);

         --  Start of processing for Insert_Indentation
      begin
         pragma Assert (Is_Empty (New_Tokns));

         --  Remove all disabled line break tokens, and change
         --  Disabled_LB_Token to Enabled_LB_Token if enabled.
         --  We remove False_End_Of_Lines here as well.

         while not After_Last (New_Tok) loop
            case Kind (New_Tok) is
               when Line_Break_Token =>
                  declare
                     Index : constant Line_Break_Index :=
                       Line_Break_Token_Index (New_Tok);
                     LB : Line_Break renames All_LB (Index);
                  begin
                     --  If a Disabled_LB_Token is found and the switch
                     --  --preserve-line-breaks is used then we do not want
                     --  to change it in Enabled_LB_Token even is if it is
                     --  enabled.

                     if Arg (Cmd, Preserve_Line_Breaks)
                       and then Kind (New_Tok) = Disabled_LB_Token
                     then
                        null;
                     else
                        if LB.Enabled then
                           Append_Line_Break_Tokn
                             (New_Tokns, Enabled => True, Index => Index);

                           LB.Tok := Nil_Tokn_Cursor;
                           --  Tok is no longer needed
                        end if;
                     end if;
                  end;
               when False_End_Of_Line =>
                  null;
               when True_End_Of_Line =>
                  pragma Assert (False);
               when others =>
                  Append_Tokn (New_Tokns, New_Tok);
            end case;

            Next (New_Tok);
         end loop;

         Scanner.Move_Tokns
           (Target => Saved_New_Tokns, Source => New_Tokns);
         New_Tok := First (Saved_New_Tokns'Access);

         --  Remove Spaces at start or end of line

         while not After_Last (New_Tok) loop
            case Kind (New_Tok) is
               when Spaces =>
                  if Kind (Prev (New_Tok)) in Line_Break_Token
                    or else Kind (Next (New_Tok)) in Line_Break_Token
                  then
                     goto Skip;
                  end if;
               when EOL_Token => pragma Assert (False);
               when others => null;
            end case;

            Append_Tokn (New_Tokns, New_Tok);
            <<Skip>>

            Next (New_Tok);
         end loop;

         Scanner.Move_Tokns
           (Target => Saved_New_Tokns, Source => New_Tokns);
         New_Tok := First (Saved_New_Tokns'Access);

         --  Insert indentation after each line break that is not immediately
         --  followed by another line break (i.e. at the beginning of each
         --  nonblank line).

         while not After_Last (New_Tok) loop
            Append_Tokn (New_Tokns, New_Tok);
            if Kind (New_Tok) in Line_Break_Token then
               declare
                  LB : Line_Break renames
                    All_LB (Line_Break_Token_Index (New_Tok));
                  pragma Assert (LB.Enabled);
                  pragma Assert (Kind (Next (New_Tok)) /= Spaces);
               begin

                  --  In case of parenthesized expressions
                  --  (like function or procedure calls where one of the call
                  --  param is a function call with multiple parameters), the
                  --  lines of the call having more than one open parenthesis,
                  --  the related line breaks indentation computed in the
                  --  previous steps might need to be adjusted.
                  if Kind (Prev (New_Tok)) = ',' then
                     Adjust_Indentation_After_Comma (New_Tok);
                  end if;

                  --  If partial formatting (called from partial_gnatpp) then
                  --  use an offset picked from the previous/next sibling of
                  --  the reformatted node to add spaces at the begining of
                  --  each line.
                  if Kind (Next (New_Tok)) not in Line_Break_Token then
                     declare
                        Offset     : constant Natural :=
                          Pp.Actions.Get_Partial_Gnatpp_Offset;
                        Crt_Indent : constant Natural :=
                          (if Partial_Gnatpp then (LB.Indentation + Offset)
                           else LB.Indentation);
                     begin
                        Append_Spaces (New_Tokns, Crt_Indent);
                     end;
                  end if;
               end;
            end if;

            Next (New_Tok);
         end loop;

         if Assert_Enabled then
            for LB of All_LB loop
               pragma Warnings (Off);
               LB := (others => <>);
               pragma Warnings (On);
            end loop;
         end if;

         Clear (All_LB);
         Clear (Lines_Data.All_LBI);
         Assert_No_LB (Lines_Data);

         Clear (Saved_New_Tokns);
      end Insert_Indentation;

      procedure Insert_Alignment_Helper
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False);

      procedure Insert_Alignment
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False) is
      begin
         if Alignment_Enabled (Cmd) then
            Insert_Alignment_Helper (Lines_Data_P, Cmd, Partial_Gnatpp);
         end if;
      end Insert_Alignment;

      procedure Insert_Alignment_Helper
        (Lines_Data_P : Lines_Data_Ptr;
         Cmd : Command_Line;
         Partial_Gnatpp : Boolean := False)
      is
         Lines_Data : Lines_Data_Rec renames Lines_Data_P.all;
         Tabs : Tab_Vector renames Lines_Data.Tabs;

         Saved_New_Tokns : Scanner.Tokn_Vec renames Lines_Data.Saved_New_Tokns;
         New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
         Ignore : Boolean := Scanner.Move_Tokns
           (Target => Saved_New_Tokns, Source => New_Tokns);

         procedure Assign_Insertion_Points;
         --  Assign the Insertion_Point component of each Tab_Rec to point to
         --  the corresponding token in Saved_New_Tokns.

         procedure Calculate_Num_Blanks;
         --  Compute the number of spaces that need to be inserted for each
         --  tab.

         procedure Do_Insertions;
         --  Do the actual insertions of spaces, based on the computation done
         --  by Calculate_Num_Blanks.

         procedure Assign_Insertion_Points is
            New_Tok : Tokn_Cursor := First (Saved_New_Tokns'Access);
         begin
            while not After_Last (New_Tok) loop
               if Kind (New_Tok) = Tab_Token then
                  Tabs (Tab_Token_Index (New_Tok)).Insertion_Point :=
                    New_Tok;
               end if;

               Next (New_Tok);
            end loop;

            --  Make sure last (sentinel) tab has an Insertion_Point that is
            --  after any others.

            Tabs (Last_Index (Tabs)).Insertion_Point :=
              Last (Saved_New_Tokns'Access);
         end Assign_Insertion_Points;

         procedure Calculate_Num_Blanks is

            --  Note on Col and Num_Blanks components of Tab_Rec: Col is
            --  initialized to a bogus value, and Num_Blanks to 0. Process_Line
            --  sets Col to the correct value. Flush_Para uses Col, and possibly
            --  changes Num_Blanks to some positive value. After the call to
            --  Calculate_Num_Blanks, Num_Blanks is used to insert the correct
            --  number of ' ' characters. Thus, Col is temporary, used only within
            --  Calculate_Num_Blanks, to communicate information from Process_Line
            --  to Flush_Para.

            Paragraph_Tabs : Tab_In_Line_Vector_Vectors.Vector;
            --  One Tab_In_Line_Vector for each line in the current paragraph

            procedure Put_Paragraph_Tabs;

            procedure Process_Line;
            --  Process a single line. Collect together all relevant tabs in
            --  Cur_Line_Tabs. All tabs in Cur_Line_Tabs must have the same
            --  Tree (that of the first tab on the line). Other tabs (for more
            --  nested constructs) are skipped. So for example:
            --
            --     X : T (Discrim => 123) := (This | That => 345);
            --
            --  we collect two tabs for ':' and ':=', which have the same Tree
            --  (a variable declaration tree). The '|' and '=>' characters in
            --  the discriminant constraint and the aggregate also have tabs, but
            --  these are skipped, because their Tree is different (more nested).
            --  If there are no tabs on the line, then of course Cur_Line_Tabs
            --  will be empty. In addition, if we have something like:
            --
            --     A := (1 | 2 | 3 => ...);
            --
            --  the '|' and '=>' tabs will have the same Index_In_Line, in which
            --  case we give up (set Tab_Mismatch to True, and set Cur_Line_Tabs
            --  to empty). Those tabs are only of use if we end up enabling line
            --  breaks after the '|'s.
            --
            --  Handling of "insertion points".
            --
            --  Let's pretend the template for assignment_statement is
            --
            --     ! ^:= !
            --
            --  which means insert the left-hand side, followed by " := ",
            --  followed by the right-hand side. (It's actually more complicated;
            --  this is just an example.) There is a tab before ":=", so multiple
            --  assignment_statements line up like this:
            --
            --     Long_Name        := 1;
            --     X                := 10_000;
            --     Even_Longer_Name := 1_000_000;
            --
            --  If we add a tab at the end (just before the ";"): "! ^:= !^2", we
            --  get this:
            --
            --     Long_Name        := 1        ;
            --     X                := 10_000   ;
            --     Even_Longer_Name := 1_000_000;
            --
            --  If in addition we add an insertion point before the right-hand
            --  side, so the template is: "! ^:= &2!^2", then the blanks are
            --  inserted before the right-hand side, resulting in right-justified
            --  expressions:
            --
            --     Long_Name        :=         1;
            --     X                :=    10_000;
            --     Even_Longer_Name := 1_000_000;
            --
            --  (We currently do not right-justify those expressions; this is just
            --  an example to show how "&" works. "&" is actually used in
            --  Do_Component_Clause.)

            procedure Flush_Para;
            --  Called at the end of a "tabbing paragraph", i.e. a group of one or
            --  more lines that each represents similar constructs that should be
            --  treated together for alignment purposes.

            procedure Flush_Para is
               Num_Lines : constant Tab_In_Line_Vector_Index'Base :=
                 Last_Index (Paragraph_Tabs);
            begin

               --  Here we have Paragraph_Tabs set to a sequence of lines (or the
               --  tabs in those lines, really). For example, if the input text
               --  was (*1):
               --
               --     package P is
               --
               --        X                    : T                := 1;
               --        A_Long_Variable_Name : T                := 2;
               --        Y                    : A_Long_Type_Name := 3;
               --
               --     end P;
               --     ^
               --     |
               --     column 1
               --
               --  then previous passes will have turned that into (*2):
               --
               --     package P is
               --
               --        X ^1: T ^2:= 1;
               --        A_Long_Variable_Name ^1: T ^2:= 2;
               --        Y ^1: A_Long_Type_Name ^2:= 3;
               --
               --     end P;
               --
               --  The tabs are shown as ^1 and ^2 in (*2) above, although they
               --  are really kept in a separate data structure (Tabs) rather than
               --  in the text itself, and take up zero columns in the buffer.
               --  The "paragraph" we're talking about consists of the three
               --  variable-declaration lines. Note that the alignment from the
               --  input has been forgotten; we would get the same thing if the
               --  input were unaligned. Our job is to align the ":" and ":="
               --  symbols, whether or not they were originally aligned.
               --
               --  ^1 means Index_In_Line = 1; ^2 means Index_In_Line = 2 (see
               --  type Tab_Rec). The Col of each tab is currently set to the
               --  column in which it appears in (*2), and the Num_Blanks is
               --  currently set to 0. The following code sets the Col of each tab
               --  to the column in which it WILL appear, and the Num_Blanks to
               --  the number of blanks to expand the tab to in order to achieve
               --  that.
               --
               --  We first loop through all the ^1 tabs, and calculate the max
               --  Col, which will be the ":" of the A_Long_Variable_Name line.
               --  We then loop through those again, and set the Num_Blanks to be
               --  the number of blanks needed to reach that max column. For each
               --  such ^1 tab, we loop from that ^1, through ^2 and ^3 and so
               --  on (we have no ^3... in this example), adjusting their Col
               --  accordingly.
               --
               --  Then we loop through all the ^2 tabs in the same way, and so on
               --  for ^3, etc.
               --
               --  So in this example, we loop down through the ^1 tabs to
               --  calculate where to put the ":"'s. Then down through the ^1 tabs
               --  again to adjust the Num_Blanks for the ^1 tabs, and loop across
               --  to adjust the Col for the ^1 and ^2 tabs. Then down through the
               --  ^2 tabs to calculate where to put the ":="'s.
               --
               --  Then down through the ^2 tabs to adjust the Num_Blanks for the
               --  ^2 tabs, and loop across to adjust the Col for the ^2 tabs.
               --  Note that adjusting the Col for the ":"'s affects where
               --  we're going to put the ":="'s -- that's the reason for the
               --  "loop across" part.
               --
               --  The end result is to calculate the Num_Blanks so that when
               --  we expand the tabs, (*2) above will be turned (back) into
               --  the (*1).

               --  We must not process a zero-line paragraph. For efficiency, we
               --  can avoid processing a one-line paragraph (leaving all tabs, if
               --  any with Num_Blanks = 0). Multi-line paragraphs always have at
               --  least one tab per line, and all lines have the same number of
               --  tabs.

               if Num_Lines = 0 then
                  return;
               end if;

               if Num_Lines = 1 then
                  Clear (Paragraph_Tabs);
                  return;
               end if;

               pragma Debug (Put_Paragraph_Tabs);
               pragma Assert (Last_Index (Paragraph_Tabs (1)) /= 0);

               for Index_In_Line in 1 .. Last_Index (Paragraph_Tabs (1)) loop
                  declare
                     Max_Col : Positive := 1;
                  begin

                     for Line of Paragraph_Tabs loop

                        declare
                           Tab_I : constant Tab_Index := Line (Index_In_Line);
                           Tab : Tab_Rec renames Tabs (Tab_I);
                        begin
                           Max_Col := Positive'Max (Max_Col, Tab.Col);
                        end;
                     end loop;

                     for Line of Paragraph_Tabs loop
                        declare
                           Tab_I : constant Tab_Index := Line (Index_In_Line);
                           Tab : Tab_Rec renames Tabs (Tab_I);
                        begin
                           if Tab.Is_Fake then
                              Tab.Col := Max_Col;
                           end if;
                           Tab.Num_Blanks := Max_Col - Tab.Col;
                           pragma Assert (if Tab.Is_Fake then Tab.Num_Blanks = 0);

                           for X_In_Line in Index_In_Line .. Last_Index (Line)
                           loop
                              declare
                                 Tab_J : constant Tab_Index := Line (X_In_Line);
                                 Tab_2 : Tab_Rec renames Tabs (Tab_J);
                              begin
                                 Tab_2.Col := Tab_2.Col + Tab.Num_Blanks;
                              end;
                           end loop;

                           pragma Assert (Tab.Col = Max_Col);

                           pragma Assert
                             (if Num_Lines = 1 then Tab.Num_Blanks = 0);
                           --  Because of that fact, we can skip all this for
                           --  1-line paragraphs.
                        end;
                     end loop;
                  end;
               end loop;
               pragma Debug (Put_Paragraph_Tabs);

               Clear (Paragraph_Tabs);
            end Flush_Para;

            New_Tok : Tokn_Cursor := First (Saved_New_Tokns'Access);
            Cur_Tab_Index : Tab_Index := 1;
            function Cur_Tab return Tab_Rec is (Tabs (Cur_Tab_Index));
            pragma Assert (not Cur_Tab.Deleted);

            First_Line_Tabs, Cur_Line_Tabs : Tab_In_Line_Vector;
            --  Tabs for first line of paragraph and for current line.

            procedure Process_Line is
               Tab_Mismatch : Boolean := False;
               First_Time : Boolean := True;
               Tree : Ada_Node;
            begin
               while Kind (New_Tok) not in End_Of_Input | Enabled_LB_Token loop

                  --  We can have two tabs at the same place if the second one is
                  --  fake. Also for implicit 'in' mode, etc. Hence 'while', not
                  --  'if' here:

                  while Cur_Tab.Insertion_Point = New_Tok loop
                     pragma Assert (not Cur_Tab.Deleted);
                     if First_Time then
                        pragma Assert (Is_Empty (Cur_Line_Tabs));
                        First_Time := False;
                        Tree := Cur_Tab.Tree;
                     end if;

                     if Cur_Tab.Tree = Tree then
                        --  Ignore if too many tabs in one line:

                        if not Cur_Tab.Is_Insertion_Point
                          and then Last_Index (Cur_Line_Tabs) <
                            Tab_Index_In_Line'Last
                        then
                           Append (Cur_Line_Tabs, Cur_Tab_Index);

                           if Cur_Tab.Index_In_Line /=
                             Last_Index (Cur_Line_Tabs)
                           then
                              Tab_Mismatch := True;
                           end if;

                           Tabs (Cur_Tab_Index).Col := Sloc (New_Tok).Col;
                        end if;
                     end if;

                     loop
                        Cur_Tab_Index := Cur_Tab_Index + 1;
                        exit when not Cur_Tab.Deleted;
                     end loop;
                  end loop;

                  Next_ss (New_Tok);
               end loop;

               if Tab_Mismatch then
                  Clear (Cur_Line_Tabs);
               end if;
            end Process_Line;

            procedure Check_Tokens_Match (X, Y : Tab_In_Line_Vector);
            --  If two lines come from the same construct, then the tokens should
            --  match. Raise an exception if they don't.

            procedure Check_Tokens_Match (X, Y : Tab_In_Line_Vector) is
            begin
               pragma Assert (not Is_Empty (X) and then not Is_Empty (Y));
               for J in 1 .. Last_Index (X) loop
                  declare
                     XX : constant Tab_Index := X (J);
                     YY : constant Tab_Index := Y (J);
                     XT : constant Symbol   := Tabs (XX).Token;
                     YT : constant Symbol   := Tabs (YY).Token;
                  begin
                     if XT /= YT then
                        --  "=>" matches a preceding "|", and vice versa
                        if (XT = Name_Arrow and then YT = Name_Bar)
                          or else (XT = Name_Bar and then YT = Name_Arrow)
                        then
                           null;
                        else
                           Put_Token (New_Tok);
                           raise Program_Error with
                             "Tab token mismatch: " &
                             Str (XT).S & " " & Str (YT).S;
                        end if;
                     end if;
                  end;
               end loop;
            end Check_Tokens_Match;

            procedure Put_Tab_In_Line_Vector
              (Name : String;
               X    : Tab_In_Line_Vector);

            procedure Put_Tab_In_Line_Vector
              (Name : String;
               X    : Tab_In_Line_Vector)
            is
               pragma Unreferenced (Name);
            begin
               if Is_Empty (X) then
                  return;
               end if;

--               Dbg_Out.Put ("\1: \t", Name);
--
--               for J in 1 .. Last_Index (X) loop
--                  if J /= 1 then
--                     Dbg_Out.Put ("; ");
--                  end if;
--                  Dbg_Out.Put ("\1", Tab_Image (Out_Buf, Tabs, X (J)));
--               end loop;
--               Dbg_Out.Put ("\n");
            end Put_Tab_In_Line_Vector;

            procedure Put_Paragraph_Tabs is
            begin
               Dbg_Out.Put
                 ("\1 Paragraph_Tabs\n",
                  Image (Integer (Last_Index (Paragraph_Tabs))));

               for X of Paragraph_Tabs loop
                  Put_Tab_In_Line_Vector ("", X);
               end loop;
               Dbg_Out.Put ("end Paragraph_Tabs\n");
            end Put_Paragraph_Tabs;

            F_Tab, C_Tab : Tab_Rec;

         --  Start of processing for Calculate_Num_Blanks

         begin
   --  Debug printouts commented out for efficiency
            while Kind (New_Tok) /= End_Of_Input loop
               declare
   --               First_Char_In_Line : constant Natural :=
   --                 Sloc (New_Tok).First - Sloc (New_Tok).Col + 1;
               begin
                  Process_Line;

   --               Dbg_Out.Put ("<<");
   --
   --               for X in First_Char_In_Line .. Sloc (New_Tok).First - 1 loop
   --                  for Tab of Cur_Line_Tabs loop
   --                     if X = Position (Out_Buf, Tabs (Tab).Mark) then
   --                        Dbg_Out.Put ("^");
   --                     end if;
   --                  end loop;
   --                  Dbg_Out.Put ("\1", To_UTF8 ((1 => Char_At (Out_Buf, X))));
   --               end loop;
   --               Dbg_Out.Put (">>\n");
   --               Put_Tab_In_Line_Vector ("First", First_Line_Tabs);
   --               Put_Tab_In_Line_Vector ("Cur", Cur_Line_Tabs);

                  if Partial_Gnatpp and then Kind (New_Tok) = End_Of_Input
                  then
                     --  In order to avoid constraint errors, in partial
                     --  gnatpp mode, when a selection needs to be reformatted
                     --  the end of input might be reached after process line
                     --  and no next token is present.
                     null;
                  else
                     Next_ss (New_Tok);
                  end if;

                  --  Consume the newline
                  if Is_Empty (Cur_Line_Tabs) then
   --                  Dbg_Out.Put ("Flush_Para -- no tabs\n");
                     Flush_Para;
                     --  Leave tabs from this line with Num_Blanks = 0.
                     Clear (First_Line_Tabs);

                  else
                     if Is_Empty (First_Line_Tabs) then
                        First_Line_Tabs := Cur_Line_Tabs;
                     else
                        --  If the Parents don't match, we're at the end of a
                        --  paragraph. We also end the paragraph if the line-tab
                        --  arrays are of different length, which can only
                        --  happen if a comment occurs in the middle of a
                        --  tabable construct (e.g. before ":=" in a variable
                        --  declaration), thus forcing a tab onto the next line.

                        F_Tab := Element (Tabs, First_Line_Tabs (1));
                        C_Tab := Element (Tabs, Cur_Line_Tabs (1));

                        if C_Tab.Parent = F_Tab.Parent
                          and then
                            Last_Index (Cur_Line_Tabs) =
                            Last_Index (First_Line_Tabs)
                        then
                           pragma Debug
                             (Check_Tokens_Match
                                (Cur_Line_Tabs,
                                 First_Line_Tabs));
                        else
   --                        Dbg_Out.Put ("Flush_Para -- parent mismatch\n");
                           Flush_Para;
                           First_Line_Tabs := Cur_Line_Tabs;

                        end if;
                        pragma Warnings (Off);
                        F_Tab := (others => <>);
                        C_Tab := (others => <>);
                        pragma Warnings (On);
                     end if;

                     Append (Paragraph_Tabs, Cur_Line_Tabs);
                     Clear (Cur_Line_Tabs);
                  end if;

                  --  This is to handle last line of the selection in partial
                  --  mode and compute accurately the alignment spaces.
                  if Partial_Gnatpp and then Kind (New_Tok) = End_Of_Input then
                     Set_Length (Cur_Line_Tabs, 0);

                     --  Recompute the tabs taking into account all the lines,
                     --  namely the last line which in case of the partial
                     --  formatting might be omitted.
                     Flush_Para;
                     Clear (First_Line_Tabs);
                     Append (Paragraph_Tabs, Cur_Line_Tabs);
                     Clear (Cur_Line_Tabs);
                  end if;
               end;
   --            Dbg_Out.Put ("\n");
            end loop;

            pragma Assert (Cur_Tab_Index = Last_Index (Tabs));
         end Calculate_Num_Blanks;

         procedure Do_Insertions is
            --  Go through the tokens, inserting Spaces for tabs that should
            --  be expanded. Don't expand a tab if it would make the line too
            --  long. Remove the Tab_Tokens from the token sequence.

            New_Tok : Tokn_Cursor := First (Saved_New_Tokns'Access);
            Next_Line_Break : Tokn_Cursor := Next (New_Tok);
            --  Sloc_Col of this is the length of the current line, not
            --  counting any spaces we have inserted.
            pragma Assert (Kind (Next_Line_Break) = Enabled_LB_Token);
            Num_Spaces : Natural := 0;
            --  Number of spaces inserted in the current line so far
            Cur_Line_Num  : Positive  := 1; -- for errors
         begin
            Check_Comment_Length := False;

            while not After_Last (New_Tok) loop

               case Kind (New_Tok) is
                  when Enabled_LB_Token =>
                     Cur_Line_Num := Cur_Line_Num + 1;
                     Error_Sloc := (Slocs.Line_Number (Cur_Line_Num), 1);
                     Num_Spaces := 0;
                     pragma Assert (New_Tok = Next_Line_Break);
                     loop
                        Next (Next_Line_Break);
                        exit when Kind (Next_Line_Break) in
                          Enabled_LB_Token | End_Of_Input;
                     end loop;

                  when Tab_Token =>
                     declare
                        Tab_X : constant Tab_Index := Tab_Token_Index (New_Tok);
                        Tab : Tab_Rec renames Tabs (Tab_X);
                        Next_Tab : Tab_Rec renames Tabs (Tab_X + 1);
                     begin
                        if not Tab.Deleted then
                           --  If Tab is an insertion point, then the very next
                           --  tab is the corresponding "real" tab; take its
                           --  Num_Blanks.

                           if Tab.Is_Insertion_Point then

                              pragma Assert (Tab.Num_Blanks = 0);
                              Tab.Num_Blanks := Next_Tab.Num_Blanks;
                              Next_Tab.Num_Blanks := 0;
                           end if;

                           declare
                              Line_Len : constant Natural :=
                                Sloc_Col (Next_Line_Break) + Num_Spaces - 1;
                              --  Current length of current line
                              New_Line_Len : constant Natural :=
                                Line_Len + Tab.Num_Blanks;
                              --  Length the current line will be after we expand
                              --  this tab. Don't do it if it will be too long.
                           begin
                              if New_Line_Len <= Arg (Cmd, Max_Line_Length) then
                                 Append_Spaces
                                   (New_Tokns, Count => Tab.Num_Blanks,
                                    Existing_OK => True);
                                 Num_Spaces := Num_Spaces + Tab.Num_Blanks;
                              end if;
                           end;
                        end if;
                     end;
                     goto Skip_Append;

                  when Disabled_LB_Token => pragma Assert (False);
                  when others => null;
               end case;

               Append_Tokn (New_Tokns, New_Tok);
               <<Skip_Append>>
               Next (New_Tok);
            end loop;

            Check_Comment_Length := True;
         end Do_Insertions;

      --  Start of processing for Insert_Alignment_Helper

      begin
         Assign_Insertion_Points;

         --  Go through the tabs and set their Num_Blanks field to the
         --  appropriate value. Tabs that are not expanded at all will
         --  have Num_Blanks left equal to zero.

         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "before Calculate_Num_Blanks"));
         Calculate_Num_Blanks;
         pragma Debug
           (Format_Debug_Output
              (Lines_Data, "after Calculate_Num_Blanks"));

         --  Then do the actual insertions:
         Do_Insertions;

         Clear (Saved_New_Tokns);
         Clear (Tabs);

      end Insert_Alignment_Helper;

   end Tok_Phases;

end Pp.Formatting;
