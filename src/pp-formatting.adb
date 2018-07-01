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

with Ada.Strings.Unbounded;
with Ada.Wide_Text_IO;

with GNATCOLL.Paragraph_Filling;

with Utils.Symbols; use Utils.Symbols;
with Text_IO;

with Langkit_Support.Slocs; use Langkit_Support;

with Pp.Command_Lines; use Pp.Command_Lines;
with Pp.Error_Slocs; use Pp.Error_Slocs;
with Pp.Scanner.Lines;

package body Pp.Formatting is
   use Utils.Command_Lines;

--   use Common_Flag_Switches, Common_String_Switches,
--     Common_String_Seq_Switches, Common_Nat_Switches;

   pragma Warnings (Off);
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

   procedure Tokns_To_Buffer
     (Buf : in out Buffer; Tokns : Scanner.Tokn_Vec;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Turns a sequence of tokens back into text. Overwrites Buf, and leaves
   --  'point' at the beginning. Whole_Line_Comment takes their indentation
   --  from the previous Spaces token, if any.

   procedure Check_Tokens
     (Out_Buf : Buffer;
      Out_Tokns, New_Tokns : Scanner.Tokn_Vec;
      Message : String;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Check that Out_Tokns and New_Tokns consist of the same sequence of
   --  tokens; it's a bug otherwise.

   procedure Check_Tokens
     (Out_Buf : in out Buffer;
      New_Tokns : Scanner.Tokn_Vec;
      Message : String;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Same as the other one, except we compute Out_Tokns from Out_Buf, and we
   --  use New_Tokns.

   procedure Check_Tokens
     (Out_Buf : in out Buffer;
      New_Tokns : Scanner.Tokn_Vec;
      Message : String;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      Out_Tokns : Scanner.Tokn_Vec;
      Ignored : Boolean := Scanner.Get_Tokns
        (Out_Buf, Out_Tokns, Utils.Ada_Version);
   begin
      Check_Tokens (Out_Buf, Out_Tokns, New_Tokns, Message, Cmd);
   end Check_Tokens;

   procedure Check_Tokens
     (Out_Buf : Buffer;
      Out_Tokns, New_Tokns : Scanner.Tokn_Vec;
      Message : String;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      use Scanner;
      New_Out_Buf, New_New_Buf : Buffer;
      New_Out_Tokns, New_New_Tokns : Scanner.Tokn_Vec;
   begin
      if Arg (Cmd, Comments_Gnat_Beginning) then
         --  We get false positives in this case, because we change from one
         --  space to two ("-- " to "--  "), which can cause comment lines to
         --  be combined into paragraphs differently.

         return;
      end if;

      Check_Same_Tokens
        (Out_Tokns, New_Tokns, Message, "Out_Tokns", "New_Tokns");

      Tokns_To_Buffer (New_Out_Buf, Out_Tokns, Cmd);
      Tokns_To_Buffer (New_New_Buf, New_Tokns, Cmd);
      declare
         New_Out_Elems : W_Str renames
           Elements (New_Out_Buf) (1 .. Last_Position (New_Out_Buf));
         New_New_Elems : W_Str renames
           Elements (New_New_Buf) (1 .. Last_Position (New_New_Buf));
         Out_Elems : W_Str renames
           Elements (Out_Buf) (1 .. Last_Position (Out_Buf));
      begin
         pragma Assert (New_Out_Elems = New_New_Elems);
         pragma Assert (New_New_Elems = Out_Elems);
         Get_Tokns (New_Out_Buf, New_Out_Tokns, Utils.Ada_Version);
         Get_Tokns (New_New_Buf, New_New_Tokns, Utils.Ada_Version);
         Check_Same_Tokens
           (Out_Tokns, New_Out_Tokns, Message, "Out_Tokns", "new Out_Tokns");
         Check_Same_Tokens
           (New_Tokns, New_New_Tokns, Message, "New_Tokns", "new New_Tokns");
      end;
   end Check_Tokens;

   --  The following Next_ss/Prev_ss are the same as Scanner.Next/Prev, except
   --  they skip the Spaces token, if present. The "ss" stands for "skip
   --  spaces".

   function Next_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor;
   procedure Next_ss (Cur : in out Scanner.Tokn_Cursor);
   function Prev_ss (Cur : Scanner.Tokn_Cursor) return Scanner.Tokn_Cursor;
   procedure Prev_ss (Cur : in out Scanner.Tokn_Cursor);

   procedure Next_ss (Cur : in out Scanner.Tokn_Cursor) is
      use Scanner;
   begin
      Next (Cur);
      if Kind (Cur) = Spaces then
         Next (Cur);
         pragma Assert
           (Kind (Cur) not in Line_Break_Token | End_Of_Line | Spaces);
      end if;
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
   function T_Img (Tree : Ada_Node) return String is
     (Libadalang.Analysis.Short_Image (Tree));

   Op_Sym_Table : constant array (Positive range <>) of Symbol :=
     (Name_Q_And,
      Name_Q_Or,
      Name_Q_Xor,
      Name_Q_Mod,
      Name_Q_Rem,
      Name_Q_Abs,
      Name_Q_Not);

   function Is_Op_Sym_With_Letters (N : Symbol) return Boolean is
     (for some Op of Op_Sym_Table => Case_Insensitive_Equal (N, Op));
      --  True if N looks like a string literal that can be used as an operator
      --  symbol containing letters, so case might matter. N should be in all
      --  lower case.

   function Sname_83 (Tok : Scanner.Tokn_Cursor) return Boolean;
   --  True if Tok can be a simple_name (in Ada 83).
   --  This includes reserved words that were added to the language
   --  after Ada 83. Needed because we don't necessarily know
   --  which language version is being used.

   function Sname_83 (Tok : Scanner.Tokn_Cursor) return Boolean is
      use Scanner;
   begin
      return Kind (Tok) in Ident | String_Lit
        or else
          (Kind (Tok) in Reserved_Word_2012
             and then Kind (Tok) not in Reserved_Word_83);
   end Sname_83;

   procedure Insert_Comment_Text
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Utils.Command_Lines.Command_Line;
      Comment_Tok : Scanner.Tokn_Cursor);
   --  Insert the text of the comment into Out_Buf, including the initial
   --  "--" and leading blanks.
   --  This will eventually be replaced by Comment_Tokn_To_Buf.

   procedure Append_Temp_Line_Break
     (Lines_Data : in out Lines_Data_Rec; Org : String;
      Internal_To_Comment : Boolean := False);

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
        (Str (Src_S).S, From => (1 => ASCII.CR), To => "")
       = Str (Out_S).S;
   end Equal_Ignoring_CR;

   ----------------

   procedure Collect_Enabled_Line_Breaks
     (Lines_Data : in out Lines_Data_Rec; Syntax_Also : Boolean)
   is
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Enabled_LBI : Line_Break_Index_Vector renames Lines_Data.Enabled_LBI;
      Syntax_LBI : Line_Break_Index_Vector renames Lines_Data.Syntax_LBI;
   begin
      pragma Assert (Is_Empty (Enabled_LBI));
      pragma Assert (Is_Empty (Syntax_LBI));

      --  We always include the last one, even though it has Length = 0

      for J in 1 .. Last_Index (All_LBI) loop
         if All_LB (All_LBI (J)).Enabled then
            Append (Enabled_LBI, All_LBI (J));
            if Syntax_Also and then All_LB (All_LBI (J)).Hard then
               if All_LB (All_LBI (J)).Length /= 0
                 or else J = Last_Index (All_LBI)
               then
                  Append (Syntax_LBI, All_LBI (J));
               end if;
            end if;
         end if;
      end loop;
   end Collect_Enabled_Line_Breaks;

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
         Result := Result + 1;
         Last   := All_LB (All_LBI (Result));
      end loop;

--???      pragma Assert (First.Level = Last.Level);
      return Result;
   end Next_Enabled;

   function Is_Empty_Line
     (Out_Buf : Buffer;
      Lines_Data : Lines_Data_Rec;
      F, L : Line_Break_Index_Index) return Boolean
   is
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      First : Line_Break renames All_LB (All_LBI (F));
      Last  : Line_Break renames All_LB (All_LBI (L));
      FP    : Positive            := Position (Out_Buf, First.Mark);
      LP    : constant Positive   := Position (Out_Buf, Last.Mark);

      procedure Assert (Result : Boolean);
      procedure Assert (Result : Boolean) is
         S : constant W_Str := Slice (Out_Buf, FP, LP - 1);
      begin
         if Result then
            pragma Assert (S = "");
         else
            pragma Assert (S /= "");
            pragma Assert (S /= " ");
            if True then -- slow
               pragma Assert (for some C of S => C /= ' ');
            end if;
         end if;
      end Assert;

   begin
      --  Take into account the fact that a hard line break occupies one
      --  character (the NL), whereas a soft line break does not, and the fact
      --  that a soft line break can be preceded or followed by a single blank
      --  (but not both).

      if First.Hard then
         FP := FP + 1;
      end if;
      if FP < LP and then Char_At (Out_Buf, FP) = ' ' then
         FP := FP + 1;
      end if;
      pragma Assert (FP <= LP);

      return Result : constant Boolean := FP = LP do
         pragma Debug (Assert (Result));
      end return;
   end Is_Empty_Line;

   ----------------

   procedure Raise_Token_Mismatch
     (Message              : String;
      Lines_Data           : Lines_Data_Rec;
      Src_Buf              : Buffer;
      Src_Tok, Out_Tok     : Scanner.Tokn_Cursor);
   --  Called when either Insert_Comments_And_Blank_Lines or Final_Check finds
   --  a mismatch. Prints debugging information and raises Token_Mismatch.

   procedure Final_Check_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line);
   procedure Final_Check
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line);
   --  Final pass: check that we have not damaged the input source text.
   --  Parameters and Out_Buf are as for Insert_Comments_And_Blank_Lines,
   --  except that comments are now included in Out_[Tokens|Buf], and this
   --  checks that they match the ones in Src_Tokens. Final_Check simply
   --  calls Final_Check_Helper, plus asserts that Out_Buf wasn't modified.

   --  The code in Final_Check[_Helper] is parallel to the code in
   --  Insert_Comments_And_Blank_Lines, so there's a bit of code duplication.
   --  It is worth it to keep Final_Check[_Helper] as simple as possible. If
   --  you make changes to one, consider making similar changes to the other.

   function Num_Lits_Match
     (Src_Tok, Out_Tok : Scanner.Tokn_Cursor;
      Cmd : Utils.Command_Lines.Command_Line) return Boolean;
   --  Called by the two Match functions for Numeric_Literal

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

   procedure Assert_No_Trailing_Blanks (Buf : Buffer) is
   begin
      for X in 2 .. Last_Position (Buf) loop
         declare
            C : constant W_Char := Char_At (Buf, X);
         begin
            pragma Assert (if C /= ' ' then not Is_Space (C));
            if C = NL then
               pragma Assert (Char_At (Buf, X - 1) /= ' ');
            end if;
         end;
      end loop;
      pragma Assert (Char_At (Buf, Last_Position (Buf)) = NL);
   end Assert_No_Trailing_Blanks;

   procedure Append_Temp_Line_Break
     (Lines_Data : in out Lines_Data_Rec; Org : String;
      Internal_To_Comment : Boolean := False)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      Next_Line_Break_Unique_Id : Modular
        renames Lines_Data.Next_Line_Break_Unique_Id;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Temp_LBI : Line_Break_Index_Vector renames Lines_Data.Temp_LBI;
      M : Marker;
   begin
      pragma Assert (Lookback (Out_Buf) /= ' '); -- no trailing blanks
      Insert_NL (Out_Buf);
      M := Mark_Previous (Out_Buf, Name => '-');

      if False then -- Too slow, but we keep it for documentation
         for L of All_LBI loop
            pragma Assert (M /= All_LB (L).Mark);
         end loop;
      end if;

      pragma Assert (if not Is_Empty (All_LB) then
        All_LB (Last_Index (All_LB)).Mark /= M);
      Append
        (All_LB,
         Line_Break'
           (Mark        => M,
            Hard        => True,
            Affects_Comments => False,
            Enabled     => True,
            Level       => 1,
            Indentation => Cur_Indentation,
            Length      => <>,
--            Kind        => Not_An_Element,
            Internal_To_Comment => Internal_To_Comment,
            UID         => Next_Line_Break_Unique_Id));
      Next_Line_Break_Unique_Id := Next_Line_Break_Unique_Id + 1;
      Append (Temp_LBI, Last_Index (All_LB));
      pragma Assert (Char_At (Out_Buf, M) = NL);

      if not Internal_To_Comment then
         Scanner.Lines.Append_Line_Break_Tokn
           (Newer_Tokns, Enabled => True,
            Index => Last_Index (All_LB), Org => Org);
      end if;
   end Append_Temp_Line_Break;

   procedure Insert_Comment_Text
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Utils.Command_Lines.Command_Line;
      Comment_Tok : Scanner.Tokn_Cursor)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;
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

      Leading_Blanks : constant Natural :=
        (if Arg (Cmd, Comments_Gnat_Beginning) and
           Kind (Comment_Tok) = Fillable_Comment
         then
           Natural'Max (Scanner.Leading_Blanks (Comment_Tok), 2)
         else Scanner.Leading_Blanks (Comment_Tok));
      --  In Comments_Only mode, we need to indent "by hand" here. In normal
      --  mode, Cur_Indentation will be heeded by the line breaks.
      Indentation : constant W_Str :=
         (if Arg (Cmd, Comments_Only)
            then (1 .. Cur_Indentation => ' ')
            else "");
      First_Line_Prelude : constant W_Str :=
          "--" & (1 .. Leading_Blanks => ' ');
      --  String that precedes the comment Text (first line)
      Subsequent_Prelude : constant W_Str := Indentation & First_Line_Prelude;
      --  String that precedes subsequent line of the comment Text
      Do_Filling : constant Boolean :=
        Comment_Filling_Enabled (Cmd)
          and then Kind (Comment_Tok) = Fillable_Comment;
      Text : constant W_Str :=
        (if Do_Filling then Filled_Text (Comment_Tok, Leading_Blanks)
         else To_W_Str (Scanner.Text (Comment_Tok)));

   --  Start of processing for Insert_Comment_Text

   begin
      if Arg (Cmd, Comments_Only) then
         Insert (Out_Buf, First_Line_Prelude);
      else
         Insert (Out_Buf, Subsequent_Prelude);
      end if;

      pragma Assert (Text (Text'Last) = NL);
      for X in Text'First .. Text'Last - 1 loop -- skip last NL
         if Text (X) = NL then
            if Arg (Cmd, Comments_Only) then
               Insert_NL (Out_Buf);
            else
               pragma Assert (Kind (Comment_Tok) = Fillable_Comment);
               Append_Temp_Line_Break -- Calls Insert_NL
                 (Lines_Data, Org => "Append_Temp_ in Insert_Comment_Text",
                  Internal_To_Comment => True);
            end if;

            Insert (Out_Buf, Subsequent_Prelude);
         else
            Insert (Out_Buf, Text (X));
         end if;
      end loop;

      Append_Comment_Text
        (Newer_Tokns, Comment_Tok, Text,
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
        (if Arg (Cmd, Comments_Gnat_Beginning) and
           Kind (Comment_Tok) = Fillable_Comment then
             Scanner.Leading_Blanks (Comment_Tok) >= 2);
      Prev_Tok : constant Tokn_Cursor := Prev (Comment_Tok);
      pragma Assert (if Kind (Comment_Tok) in Whole_Line_Comment then
        Kind (Prev_Tok) in Spaces | End_Of_Line | Line_Break_Token);
      Indentation : constant W_Str :=
        (if Kind (Comment_Tok) in Whole_Line_Comment then
          (if Kind (Prev_Tok) = Spaces then To_W_Str (Text (Prev_Tok)) else "")
         else "");
      pragma Assert
        (if Kind (Comment_Tok) in Whole_Line_Comment then
           Indentation'Length = Sloc_Col (Comment_Tok) - 1);
      First_Line_Prelude : constant W_Str :=
          "--" & (1 .. Scanner.Leading_Blanks (Comment_Tok) => ' ');
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
               pragma Assert (Point (Buf) = Sloc_First (Cur));
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
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      use Scanner;
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      Src_Toks : aliased Tokn_Vec;
      pragma Assert (Is_Empty (Src_Toks));
      Ignored : Boolean :=
        Get_Tokns (Src_Buf, Src_Toks, Utils.Ada_Version);
      Cur_Tok : Tokn_Cursor :=
        Next (First (Src_Toks'Access)); -- skip sentinel

      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;

   --  Start of processing for Do_Comments_Only

   begin
      Scanner.Freeze_Tokns (New_Tokns);
      Insert_NL (Out_Buf);
      Append_Tokn (Newer_Tokns, Start_Of_Input);
      Append_Tokn (Newer_Tokns, True_End_Of_Line);

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

            Insert_Comment_Text (Lines_Data, Cmd, Cur_Tok);
            Cur_Indentation := 0;
         else
            Append_Tokn (Newer_Tokns, Cur_Tok, Org => "only, other");
         end if;

         loop
            if Kind (Cur_Tok) not in Comment_Kind then
               Insert_Any (Out_Buf, Cur (Src_Buf));
            end if;
            Move_Forward (Src_Buf);
            pragma Assert
              (At_Point (Src_Buf, Sloc (Cur_Tok).Lastx) =
                 At_Point (Src_Buf, Next_Sloc_First (Cur_Tok)));
            exit when At_Point (Src_Buf, Sloc (Cur_Tok).Lastx);
         end loop;

         Next (Cur_Tok);
      end loop;

      pragma Assert (At_End (Src_Buf));
      Reset (Src_Buf);
      Reset (Out_Buf);

      Append_Tokn (Newer_Tokns, End_Of_Input);
      Scanner.Melt_Tokns (New_Tokns);
      Move_Tokns (Target => New_Tokns, Source => Newer_Tokns);
      Scanner.Freeze_Tokns (New_Tokns);

      pragma Debug (Check_Tokens
        (Out_Buf, New_Tokns, "Do_Comments_Only", Cmd));

      Final_Check (Lines_Data, Src_Buf, Cmd);
      Scanner.Melt_Tokns (New_Tokns);
   end Do_Comments_Only;

   Post_Tree_Phases_Done : exception;

   procedure Check_Text_So_Far
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line; Message : String);
   --  This is for debugging. It is called for every token transferred from
   --  New_Buf to Newer_Buf. It turns the part of Newer_Buf constructed so
   --  far back into text, and compares it with Out_Buf. The text should be
   --  identical. This is probably too slow to be enabled by default, even
   --  in dev mode; it makes the overall algorithm quadratic in the length
   --  of the file.

   procedure Check_Text_So_Far_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line; Message : String);

   procedure Check_Text_So_Far
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line; Message : String) is
   begin
      if Arg (Cmd, Comments_Gnat_Beginning) then
         --  See Check_Tokens
         return;
      end if;

      if Debug.Debug_Flag_Q then
         Check_Text_So_Far_Helper (Lines_Data, Cmd, Message);
      end if;
   end Check_Text_So_Far;

   procedure Check_Text_So_Far_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line; Message : String)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;
      Newer_Buf : Buffer;

      procedure Compare (Out_Elems, Newer_Elems : W_Str);
      procedure Compare (Out_Elems, Newer_Elems : W_Str) is
      begin
         if Out_Elems /= Newer_Elems then
            Dbg_Out.Output_Enabled := True;
            Dbg_Out.Put ("Check_Text_So_Far in \1:\n", Message);
            Dbg_Out.Put ("Out_Elems:\n\1\n", To_UTF8 (Out_Elems));
            Dbg_Out.Put ("----------------end Out_Elems\n");
            Dbg_Out.Put ("Newer_Elems:\n\1\n", To_UTF8 (Newer_Elems));
            Dbg_Out.Put ("----------------end Newer_Elems\n");

            raise Program_Error;
         end if;
      end Compare;

   --  Start of processing for Check_Text_So_Far_Helper

   begin
      Tokns_To_Buffer (Newer_Buf, Newer_Tokns, Cmd);
      declare
         Out_Elems : constant W_Str :=
           Slice (Out_Buf, 1, Point (Out_Buf) - 1);
         Newer_Elems : W_Str renames
           Elements (Newer_Buf) (1 .. Last_Position (Newer_Buf));
      begin
         if Message = "Insert_Comments_And_Blank_Lines" then
            --  In this case, we have to ignore NLs, because the NL
            --  characters for enabled soft line breaks are not yet
            --  present in Out_Buf.
            Compare
              (Replace_All (Out_Elems, From => (1 => NL), To => ""),
               Replace_All (Newer_Elems, From => (1 => NL), To => ""));
         elsif Message = "Insert_NLs_And_Indentation" then
            Compare (Out_Elems, Newer_Elems);
         else
            raise Program_Error;
         end if;
      end;
   end Check_Text_So_Far_Helper;

   procedure Split_Lines
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line;
      First_Time : Boolean);
   --  Enable soft line breaks as necessary to prevent too-long lines.
   --  First_Time is for debugging.

   procedure Enable_Line_Breaks_For_EOL_Comments
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line);
   --  For all end-of-line comments that occur at a soft line break, enable
   --  the line break. Note that this does not modify the Out_Buf.

   procedure Insert_Comments_And_Blank_Lines
     (Src_Buf : in out Buffer;
      Messages : out Scanner.Source_Message_Vector;
      Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line;
      Pp_Off_Present : in out Boolean);
   --  Src_Tokens is the tokens from the original source file. Out_Tokens
   --  is the newly-generated tokens. Out_Buf contains the corresponding
   --  characters to Out_Tokens. Out_[Tokens|Buf] doesn't contain any
   --  comments; they are inserted into the output from Src_Tokens.
   --
   --  This procedure also does some work in preparation for
   --  Copy_Pp_Off_Regions. In particular, it checks that OFF/ON commands are
   --  in the proper sequence, and it sets the Pp_Off_Present flag.
   --
   --  True if there is at least one Pp_Off_Comment. We don't care about
   --  Pp_On_Comments, because it's an error to have a Pp_On_Comment without a
   --  preceding Pp_Off_Comment. Set True if appropriate by
   --  Insert_Comments_And_Blank_Lines. This allows us to skip the
   --  Copy_Pp_Off_Regions pass as an optimization.

   procedure Insert_NLs_And_Indentation
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line);

   procedure Insert_Alignment
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line);
   --  Expand tabs as necessary to align things

   procedure Keyword_Casing
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line);
   --  Convert reserved words to lower/upper case based on command-line
   --  options.

   procedure Insert_Form_Feeds
     (Lines_Data : in out Lines_Data_Rec;
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
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec;
      Pp_Off_Present : Boolean);
   --  Out_Buf is fully formatted at this point, including regions where
   --  pretty printing is supposed to be turned off. This replaces those
   --  regions of Out_Buf with the corresponding regions of Src_Buf. Note
   --  that this destroys any markers that might be pointing to Out_Buf.

   procedure Split_Lines
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line;
      First_Time : Boolean)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;

      function Line_Length (F, L : Line_Break_Index) return Natural;
      --  F and L are the first and last index forming a line; returns the
      --  length of the line, not counting new-lines. F must be enabled.

      function Worthwhile_Line_Break
        (F, X : Line_Break_Index) return Boolean;
      --  Called for X = the first so-far-disabled line break on a line.
      --  Returning False means don't bother enabling it. F is the previous
      --  one.

      procedure Remove_Duplicates;
      --  Remove soft line breaks that have the same Mark as other line
      --  break(s). This is necessary because we don't want line breaks to
      --  form blank lines.

      procedure Assert;
      --  Assert that the line Length has been set if and only if the line
      --  break is enabled.

      procedure Assert is
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
                     Line_Length
                       (All_LBI (X),
                        All_LBI (Next_Enabled (Lines_Data, X))));
                  pragma Assert
                    (Break.Mark /=
                     All_LB (All_LBI (Next_Enabled (Lines_Data, X))).Mark);
               else
                  pragma Assert (Break.Length = Natural'Last);
               end if;
            end;
         end loop;

         Assert_No_Trailing_Blanks (Out_Buf);
         pragma Assert
           (Position
              (Out_Buf, All_LB (All_LBI (Last (All_LBI))).Mark) =
            Last_Position (Out_Buf));
      end Assert;

      procedure Remove_Duplicates is
         Temp : Line_Break_Index_Vector;
      --  ???If we have duplicates with different Indentation, should we choose
      --  the least indented? If we remove a line break for a '[', should we
      --  remove the corresponding one for ']', and vice-versa?
      begin
         Append (Temp, All_LBI (1));

         for X in 2 .. Last_Index (All_LBI) loop
            if All_LB (All_LBI (X)).Enabled
              or else not Is_Empty_Line (Out_Buf, Lines_Data, X - 1, X)
            then
               Append (Temp, All_LBI (X));
            else
               pragma Assert (not All_LB (All_LBI (X)).Hard);
            end if;
         end loop;
         Move (Target => All_LBI, Source => Temp);
      end Remove_Duplicates;

      function Line_Length (F, L : Line_Break_Index) return Natural is
         pragma Assert (All_LB (F).Enabled);
         First : Line_Break renames All_LB (F);
         Last  : Line_Break renames All_LB (L);
         F_Pos : constant Natural := Position (Out_Buf, First.Mark);
         L_Pos : constant Natural := Position (Out_Buf, Last.Mark);

         NL_Count      : constant Natural := (if First.Hard then 1 else 0);
         Leading_Blank : constant Natural :=
           (if L_Pos > F_Pos + 1 and then Char_At (Out_Buf, F_Pos) = ' ' then 1
            else 0);
         Trailing_Blank : constant Natural :=
           (if
              L_Pos > F_Pos + 2 and then Char_At (Out_Buf, L_Pos - 1) = ' '
            then
              1
            else 0);
         Without_Indent : constant Natural :=
           Position (Out_Buf, Last.Mark) -
           Position (Out_Buf, First.Mark) -
           NL_Count -
           Leading_Blank -
           Trailing_Blank;
      --  The length without the indentation is just the difference between the
      --  two marks, except that if the first one is hard, we don't count the
      --  NL character. If it's soft, there is no NL character yet. Also, if
      --  the first or last character is ' ', it doesn't count.

      begin
         --  If the line is blank, we ignore the indentation; we won't be
         --  putting blanks in the output. Otherwise, the length is the
         --  indentation plus the length without the indentation as
         --  calculated above.

         if Without_Indent = 0 then
            return 0;

         else
            return First.Indentation + Without_Indent;
         end if;
      end Line_Length;

      function Worthwhile_Line_Break
        (F, X : Line_Break_Index) return Boolean
      is
         This : constant Positive := Position (Out_Buf, All_LB (X).Mark);
         Prev : Positive := Position (Out_Buf, All_LB (F).Mark);
--            More : constant Boolean := -- more to be enabled to the right
--              X < Last_Index (All_LBI)
--              and then not All_LB (All_LBI (X + 1)).Enabled;
--            Threshold : constant Positive :=
--              (if True then PP_Indent_Continuation (Cmd) -- ????
--              else Positive'Max (PP_Indent_Continuation (Cmd) - 1,
--                                 (if More then 6 -- arbitrary
--                                 else 1)));
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

      Greedy : constant Boolean := True;
      --  True if we are using the "greedy" algorithm, which packs as many
      --  subtrees on the same line as possible. This seems to look much
      --  better, but we're using this flag in case people complain, so we
      --  can add a switch to go back to the old algorithm. The old
      --  algorithm was: if we enable a soft line break at a certain level,
      --  we enable all soft line breaks at the same level (within a line we
      --  are splitting).

   --  Start of processing for Split_Lines

   begin
      if not Arg (Cmd, Insert_Line_Breaks) then
         return;
      end if;
      pragma Debug
        (Format_Debug_Output
           (Lines_Data, "before Split_Lines " & Again));
      Remove_Duplicates;

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
            --  ???It would be good to set Error_Sloc in this loop, but we
            --  currently don't have that data conveniently available.
            L   := Next_Enabled (Lines_Data, F);
            Len := Line_Length (All_LBI (F), All_LBI (L));
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
            begin
               --  Loop through line breaks at current level

               for X in 1 .. Last_Index (LB) - 1 loop
                  LL := LB (X);
                  pragma Assert (All_LB (LL).Level = Level);

                  --  Don't enable the first one, unless it's "worthwhile"
                  --  according to the heuristic.

                  if LL = All_LBI (F + 1)
                    and then not Worthwhile_Line_Break (All_LBI (F), LL)
                  then
                     null;
                  else
                     if not Greedy
                       or else Line_Length (FF, LB (X + 1)) >
                          Arg (Cmd, Max_Line_Length)
                     then
                        pragma Assert (not All_LB (LL).Enabled);
                        All_LB (LL).Enabled := True;
                        FF := LL;
                     end if;
                  end if;
               end loop; -- through line breaks at current level
            end;

            Level := Level + 1;
         end loop; -- through levels

         All_LB (All_LBI (F)).Length := Len;
         pragma Assert
           (All_LB (All_LBI (F)).Length =
              Line_Length (All_LBI (F),
                           All_LBI (Next_Enabled (Lines_Data, F))));
         F := L;
      end loop; -- through line breaks

      All_LB (All_LBI (F)).Length := 0; -- last line

      pragma Debug
        (Format_Debug_Output
           (Lines_Data, "after Split_Lines " & Again));
      pragma Debug (Assert);
   end Split_Lines;

   procedure Enable_Line_Breaks_For_EOL_Comments
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      use Scanner;

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
      --  True if the tokens have the same kind and same text, except that the
      --  matching is case insensitive for identifiers, reserved words, and
      --  string literals that could be operator symbols. The source locations
      --  are ignored.

      procedure Move_Past_Char;
      procedure Move_Past_Out_Tok;

      procedure Do_End_Of_Line_Comment;
      --  Found an End_Of_Line_Comment comment; enable line breaks as
      --  appropriate.

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
                  when Line_Break_Token | Tab_Token | End_Of_Line | Spaces |
                    Comment_Kind =>
                     raise Program_Error;

                  when Start_Of_Input | End_Of_Input | Other_Lexeme =>
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
            else
               R := False;
            end if;

            if Debug_Mode then
               Dbg_Out.Put ("\1\n", (if R then "yes" else "No!"));
            end if;
         end return;
      end Match;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns, Utils.Ada_Version);
      pragma Debug (Check_Tokens
        (Out_Buf, Out_Tokns, New_Tokns,
         "Enable_Line_Breaks_For_EOL_Comments",
         Cmd));

      Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
      Out_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      --  Cursors into Src_Tokns and Out_Tokns, respectively. Skip the
      --  first Start_Of_Input token, which is just a sentinel.

      All_Cur_Line : Line_Break_Index_Index := 2;

      procedure Move_Past_Char is
         P : constant Positive := Point (Out_Buf);
      begin
         pragma Assert
           (P <= Position (Out_Buf, All_LB (All_LBI (All_Cur_Line)).Mark));

         --  Step past All_LBI at the current position

         while All_Cur_Line <= Last_Index (All_LBI)
           and then At_Point (Out_Buf, All_LB (All_LBI (All_Cur_Line)).Mark)
         loop
            All_Cur_Line := All_Cur_Line + 1;
         end loop;

         --  Step past character

         Move_Forward (Out_Buf);
      end Move_Past_Char;

      procedure Move_Past_Out_Tok is
      begin
         pragma Assert
           (Point (Out_Buf) <= Position (Out_Buf, Sloc (Out_Tok).Lastx));
         loop
            Move_Past_Char;
            pragma Assert
              (At_Point (Out_Buf, Sloc (Out_Tok).Lastx) =
               At_Point (Out_Buf, Next_Sloc_First (Out_Tok)));
            exit when At_Point (Out_Buf, Sloc (Out_Tok).Lastx);
         end loop;
      end Move_Past_Out_Tok;

      procedure Do_End_Of_Line_Comment is
      begin
         --  If an end-of-line comment appears at a place where there is a
         --  soft line break, we enable that line break. We also enable
         --  previous line breaks that are at the same level, or that belong
         --  to '('. We stop when we see a hard line break.

         if At_Point (Out_Buf, All_LB (All_LBI (All_Cur_Line)).Mark) then
            for Break in reverse 1 .. All_Cur_Line loop
               exit when All_LB (All_LBI (Break)).Hard;
               declare
                  LB : Line_Break renames All_LB (All_LBI (All_Cur_Line));
                  Prev_LB : Line_Break renames All_LB (All_LBI (Break));
                  Prev_Pos : constant Positive :=
                    Position (Out_Buf, Prev_LB.Mark);
               begin
                  if Prev_LB.Level = LB.Level
                    or else
                     (Prev_LB.Level < LB.Level
                        and then
                          (Char_At (Out_Buf, Prev_Pos + 1) = '('
                            or else
                           Char_At (Out_Buf, Prev_Pos + 1) = ','))
                  then
                     Prev_LB.Enabled := True;
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
      pragma Debug
        (Format_Debug_Output
           (Lines_Data, "before Enable_Line_Breaks_For_EOL_Comments"));
      pragma Assert (Cur (Out_Buf) = NL);
      Move_Forward (Out_Buf); -- skip sentinel

      --  Skip initial End_Of_Line token
      pragma Assert (Kind (Out_Tok) in End_Of_Line);
      Next_ss (Out_Tok);

      --  This loop is similar to the one in
      --  Insert_Comments_And_Blank_Lines; see that for commentary.

      loop
         Error_Sloc := To_Langkit (Scanner.Sloc (Src_Tok));

         if Kind (Src_Tok) not in End_Of_Line
           and then
           (Match (Src_Tok, Out_Tok)
            or else (Kind (Src_Tok) = '!' and then Kind (Out_Tok) = '|'))
         then
            exit when Kind (Src_Tok) = End_Of_Input;
            --  i.e. exit when both Src and Out are at end of input

            Move_Past_Out_Tok;
            Next_ss (Src_Tok);
            Next_ss (Out_Tok);

         else
            --  Check for "end;" --> "end Some_Name;" case

            if Kind (Src_Tok) = ';'
              and then
                Kind (Prev_Lexeme (Src_Tok)) = Res_End
              and then Sname_83 (Out_Tok)
            then
               loop -- could be "end A.B.C;"
                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);

                  exit when Kind (Out_Tok) /= '.';

                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);
                  pragma Assert (Sname_83 (Out_Tok));
               end loop;
               pragma Assert
                 (Disable_Final_Check or else Kind (Src_Tok) = ';');

            --  Check for "end Some_Name;" --> "end;" case. This only happens
            --  when the --no-end-id switch was given. Here, the name was
            --  present in the source, so we insert it.

            elsif not Arg (Cmd, End_Id)
              and then Kind (Out_Tok) = ';'
              and then
                Kind (Prev_Lexeme (Out_Tok)) = Res_End
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
                 (Disable_Final_Check or else Kind (Src_Tok) = ';');

            --  Check for "private end" --> "end" case, with a possible
            --  comment between "private" and "end".

            elsif Kind (Src_Tok) = Res_Private
              and then Kind (Out_Tok) = Res_End
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

            elsif Kind (Src_Tok) in End_Of_Line then
               if Is_Blank_Line (Src_Tok) then
                  loop
                     Next_ss (Src_Tok);
                     exit when Kind (Src_Tok) not in End_Of_Line
                       or else not Arg (Cmd, Insert_Line_Breaks)
                       or else Preserve_Blank_Lines (Cmd);
                  end loop;
               else
                  Next_ss (Src_Tok);
               end if;

            elsif Kind (Src_Tok) in Whole_Line_Comment then
               Next_ss (Src_Tok);

            elsif Kind (Out_Tok) in End_Of_Line then
               Move_Past_Out_Tok;
               Next_ss (Out_Tok);

            elsif Disable_Final_Check then
               Next_ss (Src_Tok);
               if At_Last (Src_Tok) then
                  while not At_End (Out_Buf) loop
                     Move_Forward (Out_Buf);
                  end loop;

                  goto Done;
               end if;
            else
               Raise_Token_Mismatch
                 ("eol_comments", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
            end if;
         end if;
      end loop;

      if Cur (Out_Buf) = Token_Separator then
         pragma Assert (False); -- Not yet implemented
         pragma Assert (not Arg (Cmd, Insert_Line_Breaks));
         Move_Past_Char;
      end if;

      pragma Assert (Point (Out_Buf) = Last_Position (Out_Buf) + 1);
      pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));

      pragma Assert (At_Last (Src_Tok));
      pragma Assert (At_Last (Out_Tok));
      pragma Assert (At_End (Out_Buf) and then Lookback (Out_Buf) = NL);

      <<Done>> null;

      pragma Assert (Disable_Final_Check or else Qual_Nesting = 0);
      Reset (Out_Buf);
      Clear (Out_Tokns);
      pragma Assert (At_Beginning (Src_Buf));
   end Enable_Line_Breaks_For_EOL_Comments;

   procedure Insert_Comments_And_Blank_Lines
     (Src_Buf : in out Buffer;
      Messages : out Scanner.Source_Message_Vector;
      Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line;
      Pp_Off_Present : in out Boolean)
   is
      pragma Assert (not Pp_Off_Present); -- initialized by caller
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Cur_Indentation : Natural renames Lines_Data.Cur_Indentation;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Temp_LBI : Line_Break_Index_Vector renames Lines_Data.Temp_LBI;
      Enabled_LBI : Line_Break_Index_Vector renames Lines_Data.Enabled_LBI;
      Syntax_LBI : Line_Break_Index_Vector renames Lines_Data.Syntax_LBI;
      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;
      use Scanner;

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
      --  True if the tokens have the same kind and same text, except that the
      --  matching is case insensitive for identifiers, reserved words, and
      --  string literals that could be operator symbols. The source locations
      --  are ignored.

      procedure Move_Past_Char;
      procedure Move_Past_Out_Tok;

      procedure New_To_Newer (Expect : Opt_Token_Kind := Nil);
      --  Copy New_Tok to Newer_Tokns, and move New_Tok one forward in
      --  New_Tokns. If Expect is not Nil, then New_Tok should be of that
      --  kind.

      procedure Insert_End_Of_Line_Comment;
      --  Found an End_Of_Line_Comment comment; copy it to the buffer. If it
      --  is too long to fit on the line, turn it into a Whole_Line_Comment,
      --  taking care to indent.

      --  Note that the Subtree_To_Ada pass already inserted indentation, so we
      --  mostly keep the indentation level at zero. The exception is comments,
      --  which Subtree_To_Ada didn't see. For comments, we temporarily set the
      --  indentation to that of the surrounding code.

      procedure Insert_Whole_Line_Comment;
      --  Found a Whole_Line_Comment; copy it to the buffer, taking care to
      --  indent, except that if the comment starts in column 1, we assume
      --  the user wants to keep it that way.

      procedure Insert_Private;
         --  If a private part has no declarations, the earlier passes
         --  don't insert "private", whether or not it was in the source code.
         --  If Do_Inserts is True, and there is a comment, this re-inserts
         --  "private" before the comment, to avoid messing up the formatting.

      function Extra_Blank_On_Return return Boolean;
      --  This is to deal with something like:
      --     function Some_Function
      --       (A_Parameter       : A_Parameter_Type;
      --        Another_Parameter : Another_Parameter_Type)
      --        return Result_Type;
      --       ^ Need to insert an extra blank there.
      --  Returns true if done.

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
                  when End_Of_Line | Spaces | Comment_Kind =>
                     raise Program_Error;

                  when Start_Of_Input | End_Of_Input | Line_Break_Token |
                    Tab_Token | Other_Lexeme =>
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
            else
               R := False;
            end if;

            if Debug_Mode then
               Dbg_Out.Put ("\1\n", (if R then "yes" else "No!"));
            end if;
         end return;
      end Match;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns, Utils.Ada_Version);
      pragma Debug (Check_Tokens
        (Out_Buf, Out_Tokns, New_Tokns,
         "Insert_Comments_And_Blank_Lines",
         Cmd));

      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;

      Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
      Out_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      New_Tok : Tokn_Cursor := Next_ss (First (New_Tokns'Access));
      --  Cursors into Src_Tokns and Out_Tokns, respectively. Skip the
      --  first Start_Of_Input token, which is just a sentinel.

      Start_Line_Src_Tok : Tokn_Cursor := First (Src_Tokns'Access);
      --  Token at the beginning of the previous line, but never a comment

      function L_Paren_Indentation_For_Preserve return Natural;
      --  In --preserve-line-breaks mode, this is the amount of additional
      --  indentation needed to make lines start just after a "(" on the
      --  previous line. For example, if the input is:
      --     X := Foo (Y,
      --               Z);
      --  we would normally put that all on one line. In
      --  --preserve-line-breaks mode, we want a line break between
      --  "," and "Z", and we want Z to line up after "(".

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

      Line_Start_Out : Tokn_Cursor := Next (First (Out_Tokns'Access));
      --  Used only in --preserve-line-breaks mode. The first token on the
      --  current line in Out_Tokns. Since Split_Lines has not yet been run,
      --  there is no indentation present in Out_Buf. ???Perhaps we should
      --  use Firstx/Lastx instead of First/Last in various places.

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
      --  comments.

      Cur_Line     : Line_Break_Index_Index := 2;
      EOL_Cur_Line : Line_Break_Index_Index := 2; -- for end-of-line comment
      All_Cur_Line : Line_Break_Index_Index := 2;

      Prev_Out_Tok : Tokn_Cursor := First (Out_Tokns'Access);
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
      begin
         if Out_Tok = Prev_Out_Tok then
            return;
         end if;

         Prev_Out_Tok := Out_Tok;

         --  We push the stack for "(" and pop for ")".

         --  Normally, Extra_Indent_For_Preserved_Line is True, but we set
         --  it False when we're inside an unclosed "(..." that is on the
         --  same line as the previous Enabled line break. We're talking
         --  about line breaks from previous phases, not "preserved" ones
         --  added in this phase.

         if Kind (Out_Tok) = '(' then
            if Is_Empty (Paren_Stack) then
               declare
                  Last_En : Line_Break_Index_Index := All_Cur_Line - 1;
                  Enabled_Line_Start, This_Line_Start : Positive;
               begin
                  while not All_LB (All_LBI (Last_En)).Enabled loop
                     Last_En := Last_En - 1;
                  end loop;

                  Enabled_Line_Start :=
                    Position (Out_Buf, All_LB (All_LBI (Last_En)).Mark) + 1;
                  This_Line_Start :=
                    Position (Out_Buf, Sloc (Line_Start_Out).Firstx);
                  if Enabled_Line_Start = This_Line_Start then
                     Extra_Indent_For_Preserved_Line := False;
                  end if;
               end;
            end if;

            Push
              (Paren_Stack,
               (Indent =>
                  Sloc (Out_Tok).First - Sloc (Line_Start_Out).First + 1));

         elsif Kind (Out_Tok) = ')' then
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

      procedure Move_Past_Char is
         P : constant Positive := Point (Out_Buf);
      begin
         pragma Assert
           (P <= Position (Out_Buf, All_LB (Syntax_LBI (Cur_Line)).Mark));
         pragma Assert
           (P <= Position (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark));
         pragma Assert
           (P <= Position (Out_Buf, All_LB (All_LBI (All_Cur_Line)).Mark));

         --  Step past Syntax_LBI at the current position

         while Cur_Line <= Last_Index (Syntax_LBI)
           and then At_Point (Out_Buf, All_LB (Syntax_LBI (Cur_Line)).Mark)
         loop
            Cur_Line := Cur_Line + 1;
         end loop;

         --  Step past Enabled_LBI at the current position

         while EOL_Cur_Line <= Last_Index (Enabled_LBI)
           and then At_Point
             (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark)
         loop
            EOL_Cur_Line := EOL_Cur_Line + 1;
         end loop;

         --  Step past All_LBI at the current position

         while All_Cur_Line <= Last_Index (All_LBI)
           and then At_Point (Out_Buf, All_LB (All_LBI (All_Cur_Line)).Mark)
         loop
            All_Cur_Line := All_Cur_Line + 1;
         end loop;

         --  Step past character

         Move_Forward (Out_Buf);
      end Move_Past_Char;

      procedure Move_Past_Out_Tok is
      begin
         pragma Assert
           (Point (Out_Buf) <= Position (Out_Buf, Sloc (Out_Tok).Lastx));
         loop
            Move_Past_Char;
            exit when At_Point (Out_Buf, Sloc (Out_Tok).Lastx);
         end loop;
      end Move_Past_Out_Tok;

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
            Pending : Tokn_Cursor := Next (First (Pending_Tokns'Access));
         begin
            while not After_Last (Pending) loop
               Append_Tokn (Newer_Tokns, Pending, Org => "Pending");
               Next (Pending);
            end loop;
            Clear (Pending_Tokns);
            Append_Tokn (Pending_Tokns, Start_Of_Input);
         end;

         pragma Assert (Kind (New_Tok) not in End_Of_Line);
         if Kind (New_Tok) in Enabled_LB_Token then
            if Kind (Last (Newer_Tokns'Access)) in Enabled_LB_Token then
               if Kind (Prev (Last (Newer_Tokns'Access))) in Comment_Kind then
                  null; -- Append_Temp_Line_Break already put one
               else
                  if not Is_Blank_Line (Last (Newer_Tokns'Access))
                    or else Preserve_Blank_Lines (Cmd)
                  then
                     Append_Tokn
                       (Newer_Tokns, New_Tok, Org => "New_To_Newer 1");
                  end if;
               end if;
            else
               Append_Tokn (Newer_Tokns, New_Tok, Org => "New_To_Newer 2");
            end if;
         else
            Append_Tokn (Newer_Tokns, New_Tok, Org => "New_To_Newer 3");
         end if;
         Next (New_Tok);

         while Kind (New_Tok) in Disabled_LB_Token | Tab_Token | Spaces loop
            Append_Tokn (Pending_Tokns, New_Tok);
            Next (New_Tok);
         end loop;

         pragma Debug
           (Check_Same_Token
              (Out_Tok, New_Tok, "New_To_Newer", "Out_Tok", "New_Tok"));
         pragma Debug (Check_Text_So_Far
           (Lines_Data, Cmd, "Insert_Comments_And_Blank_Lines"));
      end New_To_Newer;

      function Extra_Blank_On_Return return Boolean is
      begin
         if Kind (Out_Tok) = Res_Return then
            declare
               Paren : constant Tokn_Cursor := Prev_ss (Out_Tok);
               LB    : Line_Break renames All_LB (Enabled_LBI (EOL_Cur_Line));
            begin
               --  If the function has no parameters, or if this is the
               --  "return" of a return_statement, then there will be no ")",
               --  and we won't do anything. If there is a comment between ")"
               --  and "return", we do nothing.
               if Kind (Paren) = ')' then
                  if not LB.Hard -- will be hard if comment present
                    and then LB.Enabled
                    and then At_Point (Out_Buf, LB.Mark)
                  then
                     if Cur (Out_Buf) = Token_Separator then
                        pragma Assert (False); -- Not yet implemented
                        pragma Assert (not Arg (Cmd, Insert_Line_Breaks));
                        Move_Past_Char;
                     end if;
                     pragma Assert (Cur (Out_Buf) = ' ');
                     Move_Past_Char;
                     pragma Assert (To_Lower (Cur (Out_Buf)) = 'r');
                     Insert (Out_Buf, ' '); -- before "return"
                     Move_Past_Out_Tok;
                     --  No need to insert ' ' after "return"

                     --  Increase the length of the pending Spaces token
                     --  (which must exist) by 1.

                     declare
                        New_Pending_Tokns : Tokn_Vec;
                        Pending : Tokn_Cursor := First (Pending_Tokns'Access);
                        Spaces_Found : Boolean := False;
                     begin
                        while not After_Last (Pending) loop
                           if Kind (Pending) = Spaces then
                              Spaces_Found := True;
                              Append_Spaces
                                (New_Pending_Tokns,
                                 Count => Str (Text (Pending)).Length + 1);
                           else
                              Append_Tokn (New_Pending_Tokns, Pending,
                                           Org => "Pending for extra b");
                           end if;
                           Next (Pending);
                        end loop;
                        pragma Assert (Spaces_Found);
                        Pending_Tokns := New_Pending_Tokns;
                     end;

                     return True;
                  end if;
               end if;
            end;
         end if;
         return False;
      end Extra_Blank_On_Return;

      Prev_EOL_Comment_Src_Col : Natural := 0;
      --  If the previous line had an end-of-line comment, this is its column
      --  in the original source; otherwise 0.
      Prev_EOL_Comment_Out_Col : Natural := 0;
      --  If the previous line had an end-of-line comment, this is its column
      --  in the output; otherwise 0.

      procedure Insert_End_Of_Line_Comment is
         pragma Assert (EOL_Cur_Line > 1);
         Indentation : constant Natural :=
           All_LB (Enabled_LBI (EOL_Cur_Line - 1)).Indentation;
         Prev_Src_Tok : constant Tokn_Cursor := Prev_ss (Src_Tok);
         pragma Assert (Sloc (Src_Tok).Line = Sloc (Prev_Src_Tok).Line);
         Preceding_Blanks : Natural :=
           Sloc_First (Src_Tok) - Sloc_Last (Prev_Src_Tok) - 1;
         --  Number of blanks between the previous token and this comment.
         --  Note that tab characters have been expanded into spaces in
         --  Src_Buf.
      begin
         --  If we're just before a blank followed by NL, move past the blank,
         --  so we won't add a new NL below.

         if not At_Point (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark)
           and then Cur (Out_Buf) = ' '
         then
            Move_Past_Char;
            pragma Assert (Cur (Out_Buf) /= ' ');
            pragma Assert (Preceding_Blanks > 0);
            if Preceding_Blanks > 0 then
               Preceding_Blanks := Preceding_Blanks - 1;
            end if;

            --  Remove the Spaces token (which must exist) from
            --  Pending_Tokns.

            declare
               New_Pending_Tokns : Tokn_Vec;
               Pending : Tokn_Cursor := First (Pending_Tokns'Access);
               Spaces_Found : Boolean := False;
            begin
               while not After_Last (Pending) loop
                  if Kind (Pending) = Spaces then
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
            Append_Spaces (Newer_Tokns, Count => 1);
         end if;

         --  If this comment is lined up with one on the previous line in the
         --  source, then line it up in the output. Otherwise, just preserve
         --  Preceding_Blanks. ???Disabled for now.

         if False and then Sloc (Src_Tok).Col = Prev_EOL_Comment_Src_Col then
            while Cur_Column (Out_Buf) < Prev_EOL_Comment_Out_Col loop
               Insert (Out_Buf, ' ');
            end loop;
         else
            for J in 1 .. Preceding_Blanks loop
               Insert (Out_Buf, ' '); -- Avoid making line too long???
            end loop;

            Append_Spaces
              (Newer_Tokns, Count => Preceding_Blanks, Existing_OK => True);
         end if;
         if False then -- ???Disabled for now.
            --  This doesn't work, because Cur_Column is wrong, because Out_Buf
            --  does not yet contain any NLs. Also, we presumably need to reset
            --  these variables to 0 when we see a line without a comment.
            Prev_EOL_Comment_Src_Col := Sloc (Src_Tok).Col;
            Prev_EOL_Comment_Out_Col := Cur_Column (Out_Buf);
         end if;
         Insert_Comment_Text (Lines_Data, Cmd, Src_Tok);

         --  In the usual case, the end-of-line comment is at a natural
         --  (hard) line break, like this:
         --      X := X + 1; -- Increment X
         --  so we don't need another one. But if the original was:
         --      X := -- Increment X
         --        X + 1;
         --  we need to add a line break after the comment.

         if not At_Point
           (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark)
         then
            pragma Assert (Cur (Out_Buf) /= NL);
            Cur_Indentation := Indentation;
            if Arg (Cmd, Insert_Line_Breaks) then
               Append_Temp_Line_Break
                 (Lines_Data,
                  Org => "Append_Temp_ in Insert_End_Of_Line_Comment");
            end if;
            Cur_Indentation := 0;
         else
            Append_Tokn (Newer_Tokns, False_End_Of_Line, "eol extra");
            --  This is needed because every comment in Newer_Tokns must be
            --  followed by End_Of_Line.
         end if;
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
      function After_Indentation return Natural;
      --  Same as "All_LB (Syntax_LBI (Cur_Line)).Indentation", except we
      --  skip Syntax_LBI with Affects_Comments = False. In other words,
      --  this is the current/next line-breaks indentation which should
      --  affect comments.

      function Before_Indentation return Natural is
         X : Line_Break_Index_Index := Cur_Line - 1;
      begin
         while X > 1 and then
           not All_LB (Syntax_LBI (X)).Affects_Comments
         loop
            X := X - 1;
         end loop;
         return All_LB (Syntax_LBI (X)).Indentation;
      end Before_Indentation;

      function After_Indentation return Natural is
         X : Line_Break_Index_Index := Cur_Line;
      begin
         while X < Last_Index (Syntax_LBI)
           and then not All_LB (Syntax_LBI (X)).Affects_Comments
         loop
            X := X + 1;
         end loop;
         return All_LB (Syntax_LBI (X)).Indentation;
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
            if Kind (Out_Tok) = End_Of_Input then
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
            return Kind (Out_Tok) not in
              Res_Begin | Res_Else | Res_Elsif | Res_When;
         end Look_Before;

         Indentation : Natural;

         procedure Set_Cur_Indent is
         begin
            if Sloc (Src_Tok).Col = 1
              or else Kind (Src_Tok) = Special_Comment
              or else Arg (Cmd, Comments_Unchanged)
            then
               Cur_Indentation := Sloc (Src_Tok).Col - 1; -- Keep as in input

            else
               Cur_Indentation := Indentation;

               --  Try to make comment fit on line. If we're filling it, then
               --  rely on that to make it fit. If Cur_Indentation pushes
               --  it past Max_Line_Length, and the comment would fit if
               --  not indented, then reduce the indentation.

               declare
                  W : constant Positive := Width (Src_Tok);
               begin
                  if (not Comment_Filling_Enabled (Cmd)
                     or else Kind (Src_Tok) /= Fillable_Comment)
                    and then
                      Cur_Indentation + W >
                      Arg (Cmd, Max_Line_Length)
                    and then W <= Arg (Cmd, Max_Line_Length)
                  then
                     Cur_Indentation :=
                       Good_Column (PP_Indentation (Cmd),
                                    Arg (Cmd, Max_Line_Length) - W);
                     pragma Assert
                       ((Cur_Indentation mod PP_Indentation (Cmd)) = 0);
                  end if;
               end;
            end if;
         end Set_Cur_Indent;

         use Source_Message_Vectors;

         Other_Sloc : constant String := Sloc_Image (Sloc (Last_Pp_Off_On));
         Message : Source_Message := (Sloc (Src_Tok), others => <>);

      --  Start of processing for Insert_Whole_Line_Comment

      begin
         --  Processing in preparation for Copy_Pp_Off_Regions. That depends on
         --  an alternating sequence: OFF, ON, OFF, ON, .... So we check that
         --  here, and abort processing if it's not true.

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

         if Point (Out_Buf) = 2 then
            Indentation := 0;

         --  Otherwise, we indent as for the max of the preceding and following
         --  line breaks, except when Look_Before is False (as it is for this
         --  comment, which is followed by "else").

         else
            Indentation := After_Indentation;

            if Look_Before then
               Indentation := Natural'Max (Indentation, Before_Indentation);
            end if;
         end if;

         --  Make sure Indentation is a multiple of PP_Indentation; otherwise
         --  style checking complains "(style) bad column".

         Indentation :=
           (Indentation / PP_Indentation (Cmd)) * PP_Indentation (Cmd);
         pragma Assert ((Indentation mod PP_Indentation (Cmd)) = 0);

         --  If we're inside something parenthesized, add an extra level

         if Kind (Out_Tok) = ')' then
            Indentation := Indentation + PP_Indentation (Cmd);
         end if;

         Set_Cur_Indent;
         if Is_Blank_Line (Prev_ss (Src_Tok))
           or else Lookback (Out_Buf) /= NL
         then
            if Arg (Cmd, Insert_Line_Breaks) then
               if True then
                  Append_Temp_Line_Break
                    (Lines_Data,
                     Org => "Append_Temp_ in Insert_Whole_Line_Comment");
               end if;
            end if;
         end if;

         loop
            --  ???Handle blank lines here, too?
            Insert_Comment_Text (Lines_Data, Cmd, Src_Tok);
            Next_ss (Src_Tok);
            pragma Assert (Kind (Src_Tok) in End_Of_Line);
            Next_ss (Src_Tok);

            exit when Kind (Src_Tok) not in
              Special_Comment | Fillable_Comment | Other_Whole_Line_Comment;

            Set_Cur_Indent;
            if Arg (Cmd, Insert_Line_Breaks) then
               Append_Temp_Line_Break
                 (Lines_Data,
                  Org => "Append_Temp_ in Insert_Whole_Line_Comment 2");
            end if;
         end loop;

         --  If we don't have an enabled line break here, we need to add one.

         if not Insert_Blank_Lines (Cmd)
           and then not Preserve_Blank_Lines (Cmd)
         then
            pragma Assert
              ((Cur (Out_Buf) = NL) =
                 (At_Point (Out_Buf, All_LB (Syntax_LBI (Cur_Line)).Mark)));
            pragma Assert
              (if
                 Cur (Out_Buf) = NL
                 then
              At_Point (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark));
         end if;

         --  A line break is needed after the comment. If one is not already
         --  there, add one.

         declare
            LB_Pos : constant Positive :=
              Position (Out_Buf, All_LB (Enabled_LBI (EOL_Cur_Line)).Mark);
            P : constant Positive := Point (Out_Buf);
         begin
            if LB_Pos = P
              or else (Cur (Out_Buf) = ' ' and then LB_Pos = P + 1)
            then
               Append_Tokn
                 (Newer_Tokns, False_End_Of_Line, "whole line extra");
               --  This is needed because every comment in Newer_Tokns must
               --  be followed by End_Of_Line.
            else
               Cur_Indentation := Indentation;
               if Arg (Cmd, Insert_Line_Breaks) then
                  Append_Temp_Line_Break
                    (Lines_Data,
                     Org => "Append_Temp_ in Insert_Whole_Line_Comment 3");
               end if;
            end if;
         end;

         Cur_Indentation := 0;
      end Insert_Whole_Line_Comment;

      procedure Insert_Private is
         Out_Tok_Pos : constant Positive :=
           Position (Out_Buf, Sloc (Out_Tok).Firstx);
         LB_Pos : constant Positive :=
           Position (Out_Buf, All_LB (Syntax_LBI (Cur_Line)).Mark);
         Prev_LB_Pos : constant Positive :=
           Position (Out_Buf, All_LB (Syntax_LBI (Cur_Line - 1)).Mark);

      begin
         --  Either the current or previous line break is just before "end";
         --  that's the indentation we want for "private".

         if LB_Pos = Out_Tok_Pos - 1 then
            Cur_Indentation := All_LB (Syntax_LBI (Cur_Line)).Indentation;
         elsif Prev_LB_Pos = Out_Tok_Pos - 1 then
            Cur_Indentation := All_LB (Syntax_LBI (Cur_Line - 1)).Indentation;
         else
            pragma Assert (False);
         end if;

         if Arg (Cmd, Insert_Line_Breaks) then
            Append_Temp_Line_Break
              (Lines_Data, Org => "Append_Temp_ private 1");
         end if;
         Insert (Out_Buf, "private");
         Append_Tokn (Newer_Tokns, Res_Private);
         Append_Temp_Line_Break
           (Lines_Data, Org => "Append_Temp_ private 2");
         Cur_Indentation := 0;
         Next_ss (Src_Tok);
      end Insert_Private;

      function Line_Break_LT (X, Y : Line_Break_Index) return Boolean;

      function Line_Break_LT (X, Y : Line_Break_Index) return Boolean is
      begin
         return Mark_LT (Out_Buf, All_LB (X).Mark, All_LB (Y).Mark);
      end Line_Break_LT;

      package Line_Break_Sorting is
        new Line_Break_Index_Vectors.Generic_Sorting ("<" => Line_Break_LT);

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
      pragma Assert (Cur (Out_Buf) = NL);
      Move_Forward (Out_Buf); -- skip sentinel

      Append_Tokn (Pending_Tokns, Start_Of_Input);

      pragma Assert (Is_Empty (Newer_Tokns));
      Scanner.Clear (Newer_Tokns);
      Append_Tokn (Newer_Tokns, Start_Of_Input);

      --  Skip initial End_Of_Line token
      pragma Assert (Kind (Out_Tok) in End_Of_Line);
      Next_ss (Out_Tok);
      pragma Assert (Kind (New_Tok) = Enabled_LB_Token);
      New_To_Newer;
      Line_Start_Out := Out_Tok;

      Collect_Enabled_Line_Breaks (Lines_Data, Syntax_Also => True);
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
      --     We normalize by removing "declare" from a block statement with no
      --     declarative items. We put the "declare" back in here.
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
         pragma Assert (Kind (Out_Tok) not in Comment_Kind);
         Manage_Paren_Stack;

         --  The order of the if/elsif's below is important in some
         --  cases. Blank lines must be handled late, even if they match.
         --  End_Of_Line_Comments must be handled before blank lines,
         --  because they need to appear at the end of the preceding line.
         --  Whole_Line_Comments must be handled after blank lines, because
         --  the blank line should precede the comment.

         if Kind (Src_Tok) not in End_Of_Line
           and then
           (Match (Src_Tok, Out_Tok)
            or else (Kind (Src_Tok) = '!' and then Kind (Out_Tok) = '|'))
         then
            exit when Kind (Src_Tok) = End_Of_Input;
            --  i.e. exit when both Src and Out are at end of input

            if Extra_Blank_On_Return then
               null; -- Extra_Blank_On_Return took care of it
            else
               Move_Past_Out_Tok;
            end if;

            if Kind (Out_Tok) in End_Of_Line then
               Line_Start_Out := Out_Tok;
            end if;

            Next_ss (Src_Tok);
            Next_ss (Out_Tok);
            New_To_Newer;

         else
            --  Check for "end;" --> "end Some_Name;" case

            if Kind (Src_Tok) = ';'
              and then
                Kind (Prev_Lexeme (Src_Tok)) = Res_End
              and then Sname_83 (Out_Tok)
            then
               loop -- could be "end A.B.C;"
                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);
                  New_To_Newer;

                  exit when Kind (Out_Tok) /= '.';

                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);
                  New_To_Newer ('.');
                  pragma Assert (Sname_83 (Out_Tok));
               end loop;
               pragma Assert
                 (Disable_Final_Check or else Kind (Src_Tok) = ';');

            --  Check for "end Some_Name;" --> "end;" case. This only happens
            --  when the --no-end-id switch was given. Here, the name was
            --  present in the source, so we insert it.

            elsif not Arg (Cmd, End_Id)
              and then Kind (Out_Tok) = ';'
              and then
                Kind (Prev_Lexeme (Out_Tok)) = Res_End
              and then Kind (Src_Tok) in Ident | String_Lit
            then
               Insert (Out_Buf, " ");
               Append_Tokn (Newer_Tokns, Spaces, Name_Space);
               loop -- could be "end A.B.C;"
                  Insert (Out_Buf, To_W_Str (Text (Src_Tok)));
                  Append_Tokn (Newer_Tokns, Src_Tok);
                  Next_ss (Src_Tok);

                  exit when Kind (Src_Tok) /= '.';

                  Insert (Out_Buf, To_W_Str (Text (Src_Tok)));
                  Append_Tokn (Newer_Tokns, Src_Tok);
                  Next_ss (Src_Tok);
                  pragma Assert
                    (Kind (Src_Tok) in Ident | String_Lit);
               end loop;
               pragma Assert
                 (Disable_Final_Check or else Kind (Src_Tok) = ';');

            --  Check for "private end" --> "end" case, with a possible
            --  comment between "private" and "end".

            elsif Kind (Src_Tok) = Res_Private
              and then Kind (Out_Tok) = Res_End
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
               Insert (Out_Buf, '(');
               Append_Tokn (Newer_Tokns, Src_Tok);
               Next_ss (Src_Tok);
            elsif Qual_Nesting > 0
              and then Kind (Src_Tok) = ')'
              and then Kind (Prev_Lexeme (Src_Tok)) = ')'
            then
               Qual_Nesting := Qual_Nesting - 1;
               Insert (Out_Buf, ')');
               Append_Tokn (Newer_Tokns, Src_Tok);
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

            elsif Is_Blank_Line (Src_Tok) then
               declare
                  pragma Assert (Kind (Prev_ss (Src_Tok)) in End_Of_Line);
                  Prev_Prev_Tok_Kind : constant Token_Kind :=
                    Kind (Prev_ss (Prev_ss (Src_Tok)));
               begin
                  loop
                     Next_ss (Src_Tok);
                     exit when Kind (Src_Tok) not in End_Of_Line
                       or else not Arg (Cmd, Insert_Line_Breaks)
                       or else Preserve_Blank_Lines (Cmd);
                  end loop;
                  declare
                     Next_Tok_Kind : constant Opt_Token_Kind :=
                       (if At_Last (Src_Tok)
                          then Nil
                          else Kind (Next_ss (Src_Tok)));
                  begin
                     if not Arg (Cmd, Insert_Line_Breaks)
                       or else Preserve_Blank_Lines (Cmd)
                       or else (not Insert_Blank_Lines (Cmd)
                                  and then Kind (Src_Tok) /= End_Of_Input)
                       or else Prev_Prev_Tok_Kind in Comment_Kind
                       or else Next_Tok_Kind in Comment_Kind
                     then
                        Append_Temp_Line_Break
                          (Lines_Data, Org => "Append_Temp_ blank line");
                     end if;
                  end;
               end;

            --  Normally, we simply ignore End_Of_Line in the input. But
            --  for --preserve-line-breaks mode, if we see a line break in
            --  the input that is not yet in the output, we copy it
            --  over. We set the indentation to take into account
            --  surrounding indentation, plus line continuation if
            --  appropriate, plus "("-related indentation. If the next
            --  character in the output is already ' ', we subtract one
            --  from the indentation to make up for that. (There can never
            --  be two in a row.)

            elsif Kind (Src_Tok) in End_Of_Line then
               pragma Assert (not Is_Blank_Line (Src_Tok));
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

                     if Cur (Out_Buf) = ' ' then
                        pragma Assert (Lookahead (Out_Buf) /= ' ');
                        Indentation := Indentation - 1;
                     end if;

                     if Kind (Out_Tok) not in End_Of_Line then
                        Indentation :=
                          Indentation + L_Paren_Indentation_For_Preserve;
                        Cur_Indentation := Indentation;
                        Append_Temp_Line_Break
                          (Lines_Data,
                           Org => "Append_Temp_ Preserve_Line_Breaks");
                        Cur_Indentation := 0;
                        Line_Start_Out := Out_Tok;
                     end if;
                  end;
               end if;

            elsif Kind (Src_Tok) in Whole_Line_Comment then
               Insert_Whole_Line_Comment;

            elsif Kind (Out_Tok) in End_Of_Line then
               Move_Past_Out_Tok;
               Next_ss (Out_Tok);
               New_To_Newer;
               Line_Start_Out := Out_Tok;

            --  Else print out debugging information and crash. This
            --  avoids damaging the source code in case of bugs. However,
            --  if the Disable_Final_Check debug flag is set, try to
            --  continue by skipping one source token, or one output
            --  token.

            elsif Disable_Final_Check then
               Next_ss (Src_Tok);
               if At_Last (Src_Tok) then
                  while not At_End (Out_Buf) loop
                     Move_Forward (Out_Buf);
                  end loop;

                  goto Done;
               end if;
            else
               Raise_Token_Mismatch
                 ("Inserting", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
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

      if Cur (Out_Buf) = Token_Separator then
         pragma Assert (False); -- Not yet implemented
         pragma Assert (not Arg (Cmd, Insert_Line_Breaks));
         Move_Past_Char;
      end if;

      pragma Assert (Is_Empty (Paren_Stack));

      pragma Assert (Point (Out_Buf) = Last_Position (Out_Buf) + 1);
      pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));

      pragma Assert (Cur_Indentation = 0);

      pragma Assert (At_Last (Src_Tok));
      pragma Assert (At_Last (Out_Tok));
      Append_Tokn (Newer_Tokns, End_Of_Input);
      Scanner.Melt_Tokns (New_Tokns);
      Move_Tokns (Target => New_Tokns, Source => Newer_Tokns);
      Scanner.Freeze_Tokns (New_Tokns);
      pragma Assert (At_End (Out_Buf) and then Lookback (Out_Buf) = NL);
      pragma Assert (Cur_Line = Last_Index (Syntax_LBI) + 1);
      pragma Assert (EOL_Cur_Line = Last_Index (Enabled_LBI) + 1);
      pragma Assert (All_Cur_Line = Last_Index (All_LBI) + 1);

      <<Done>> null;

      pragma Assert (Line_Break_Sorting.Is_Sorted (All_LBI));
      pragma Assert (Line_Break_Sorting.Is_Sorted (Temp_LBI));
      Line_Break_Sorting.Merge (Target => All_LBI, Source => Temp_LBI);
      pragma Assert (Is_Empty (Temp_LBI));
      pragma Assert (Line_Break_Sorting.Is_Sorted (All_LBI));
      pragma Assert (Disable_Final_Check or else Qual_Nesting = 0);
      Reset (Out_Buf);
      Clear (Out_Tokns);
      pragma Assert (At_Beginning (Src_Buf));

      Clear (Enabled_LBI);
      Clear (Syntax_LBI);

      --  We can't Check_Tokens here, because Out_Buf doesn't have an NL
      --  character after every comment. This happens when there is a soft
      --  enabled line break after the token; Insert_NLs_And_Indentation
      --  will insert the NL.
   end Insert_Comments_And_Blank_Lines;

   procedure Insert_NLs_And_Indentation
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Enabled_LBI : Line_Break_Index_Vector renames Lines_Data.Enabled_LBI;
      Syntax_LBI : Line_Break_Index_Vector renames Lines_Data.Syntax_LBI;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;

      --  We loop through Out_Buf, and for each character, take care of
      --  the Line_Break at that character, if any. The Line_Breaks are in
      --  Enabled_LBI. Enabled_LBI cannot have duplicates (two
      --  elements at the same Mark), because hard line breaks take up space in
      --  Out_Buf (there is an NL), and we never enable two soft line breaks in
      --  a row.

      At_Line_Start : Boolean := True;
      Indentation   : Natural := 0;

      Cur_Line : Line_Break_Index_Index := 1;

      use Scanner;
      New_Tok : Tokn_Cursor := First (New_Tokns'Access);
      Newer_Tokns : Scanner.Tokn_Vec renames Lines_Data.Newer_Tokns;

      procedure New_To_Newer_2;
      --  Copy New_Tok to Newer_Tokns

      Comment_Seen_Flag : Boolean := False;

      procedure New_To_Newer_2 is
      begin
         pragma Debug (Check_Text_So_Far
           (Lines_Data, Cmd, "Insert_NLs_And_Indentation"));

         --  We can get Tab_Token here in case of record rep clauses
         while Kind (New_Tok) in Disabled_LB_Token | Tab_Token loop
            if Kind (New_Tok) = Tab_Token then -- discard Disabled_LB_Token
               Append_Tokn (Newer_Tokns, New_Tok);
            end if;
            Next (New_Tok);
         end loop;

         if Kind (New_Tok) = Spaces then
            if Kind (Last (Newer_Tokns'Access)) in Comment_Kind then
               --  This can happen if a comment occurs in the middle of an
               --  expression, as in:
               --      Put ("Hello " &
               --      -- Here is a comment.
               --      "world.");
               Comment_Seen_Flag := True;
               Scanner.Lines.Append_Line_Break_Tokn
                 (Newer_Tokns, Enabled => True,
                  Index => 999999, -- Index doesn't matter
                  Org => "NLs/Ind, after comment");
               pragma Assert (Cur (Out_Buf) = ' ');
               Replace_Cur (Out_Buf, NL);
            else
               Append_Spaces
                 (Newer_Tokns,
                  Count => Str (Text (New_Tok)).Length,
                  Existing_OK => True,
                  Org => "NLs/Ind combined space");
            end if;

         else
            if Kind (New_Tok) in Comment_Kind then
               --  The indentation will be different (zero earlier),
               --  so we need to recompute
               Append_Comment_Text
                 (Newer_Tokns, New_Tok, To_W_Str (Text (New_Tok)),
                  Recompute_Length => True,
                  Comments_Only => False,
                  Comments_Gnat_Beginning =>
                    Arg (Cmd, Comments_Gnat_Beginning),
                  Indent => Indentation,
                  Org => "NLs/Ind, comment");
            else
               Append_Tokn
                 (Newer_Tokns, New_Tok, Org => "NLs/Ind New_To_Newer_2");
            end if;
         end if;

         Next (New_Tok);

         if not After_Last (New_Tok) then
            while Kind (New_Tok) in Disabled_LB_Token | Tab_Token loop
               if Kind (New_Tok) = Tab_Token then -- discard Disabled_LB_Token
                  Append_Tokn (Newer_Tokns, New_Tok);
               end if;
               Next (New_Tok);
            end loop;
            if Kind (New_Tok) = False_End_Of_Line then
               Next (New_Tok);
               while Kind (New_Tok) in Disabled_LB_Token | Tab_Token loop
                  if Kind (New_Tok) = Tab_Token then
                     --  discard Disabled_LB_Token
                     Append_Tokn (Newer_Tokns, New_Tok);
                  end if;
                  Next (New_Tok);
               end loop;
            end if;
         end if;
      end New_To_Newer_2;

      Internal_To_Comment : Boolean := False;

      use Scanner.Lines;

   --  Start of processing for Insert_NLs_And_Indentation

   begin
      pragma Assert (Is_Empty (Newer_Tokns));
      Scanner.Clear (Newer_Tokns);

      --  Skip initial Start_Of_Input token
      pragma Assert (Kind (New_Tok) = Start_Of_Input);
      New_To_Newer_2;

      Collect_Enabled_Line_Breaks (Lines_Data, Syntax_Also => False);

      Char_Loop : loop
         pragma Assert
           (Point (Out_Buf) <=
            Position (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark));
--         if At_Point (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark) then
--            Dbg_Out.Put ("\n");
--         end if;

         --  Even though Enabled_LBI cannot have duplicates, we still
         --  need 'while' (not 'if'), because in one case we Move_Forward
         --  below.

         while At_Point (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark) loop
            Error_Sloc := (Slocs.Line_Number (Cur_Line), 1);
            pragma Assert
              (Point (Out_Buf) =
               Position (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark));
--            Dbg_Out.Put ("Point = \1, break = ", Image (Point (Out_Buf)));
--            Dump_Marker (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark);

            At_Line_Start := True;

            --  A hard line break already has NL; for a soft one, we need to
            --  add NL.

            if All_LB (Enabled_LBI (Cur_Line)).Hard then
--               Dbg_Out.Put
--                 ("\1: hard line break\n",
--                  Image (Integer (Cur_Line)));
               Internal_To_Comment :=
                 All_LB (Enabled_LBI (Cur_Line)).Internal_To_Comment;
               if not Internal_To_Comment then
                  if Kind (New_Tok) in Line_Break_Token then
                     New_To_Newer_2;
                  else
                     Append_Line_Break_Tokn
                       (Newer_Tokns, Enabled => True,
                        Index => 888888, -- Index doesn't matter
                        Org => "NLs/Ind, after comment 2");
                  end if;
               end if;
               pragma Assert (Cur (Out_Buf) = NL);
               Move_Forward (Out_Buf);

            else
               --  A soft line break can be preceded or followed by a blank,
               --  but never both, and never more than one. If there is a
               --  blank, we replace it with NL, otherwise we insert NL.

               if Lookback (Out_Buf) = ' ' then
--                  Dbg_Out.Put
--                    ("\1: soft line break Replace_Previous\n",
--                     Image (Integer (Cur_Line)));
                  pragma Assert (Kind (Prev (New_Tok)) = Disabled_LB_Token);
                  declare
                     L : constant Token := Delete_Last (Newer_Tokns);
                     pragma Assert (L.Kind = Spaces);
                     pragma Assert (Text (L) = Name_Space);
                  begin
                     --  Previous can be End_Of_Line if a blank line occurs
                     --  in the middle of an expression.
                     Append_Line_Break_Tokn
                       (Newer_Tokns, Enabled => True,
                        Index => Line_Break_Token_Index (Prev (New_Tok)),
                        Org => "NLs/Ind soft, 1 space after");
                  end;
                  pragma Assert (Cur (Out_Buf) /= ' ');
                  Replace_Previous (Out_Buf, NL);
                  pragma Assert
                    (not At_Point
                       (Out_Buf, All_LB (Enabled_LBI (Cur_Line + 1)).Mark));

               elsif Cur (Out_Buf) = ' ' then
--                  Dbg_Out.Put
--                    ("\1: soft line break Replace_Cur\n",
--                     Image (Integer (Cur_Line)));
                  pragma Assert (Kind (New_Tok) = Spaces);
                  pragma Assert (Kind (Prev (New_Tok)) = Disabled_LB_Token);
                  case Tokn_Length (New_Tok) is
                     when 1 =>
                        Append_Line_Break_Tokn
                          (Newer_Tokns, Enabled => True,
                           Index => Line_Break_Token_Index (Prev (New_Tok)),
                           Org => "NLs/Ind soft, 1 space after");
                     when 2 =>
                        --  This case happens when Extra_Blank_On_Return
                        --  returns True.
                        Append_Line_Break_Tokn
                          (Newer_Tokns, Enabled => True,
                           Index => Line_Break_Token_Index (Prev (New_Tok)),
                           Org => "NLs/Ind soft, 2 spaces after");
                        Append_Spaces
                          (Newer_Tokns,
                           Count => 1,
                           Org => "NLs/Ind extra ret");
                     when others => raise Program_Error;
                  end case;
                  Next (New_Tok); -- skip Spaces token

                  Replace_Cur (Out_Buf, NL);
                  pragma Assert
                    (not At_Point
                       (Out_Buf, All_LB (Enabled_LBI (Cur_Line + 1)).Mark));
                  Move_Forward (Out_Buf);

               else
                  pragma Assert (Kind (Prev (New_Tok)) = Disabled_LB_Token);
                  if not Comment_Seen_Flag then
   --                  Dbg_Out.Put
   --                    ("\1: soft line break insert\n",
   --                     Image (Integer (Cur_Line)));
                     Insert_NL (Out_Buf);
                     pragma Assert
                       (not At_Point
                          (Out_Buf, All_LB (Enabled_LBI (Cur_Line + 1)).Mark));
                     Append_Line_Break_Tokn
                       (Newer_Tokns, Enabled => True,
                        Index => Line_Break_Token_Index (Prev (New_Tok)),
                        Org => "NLs/Ind soft");
                  end if;
               end if;
            end if;
            Indentation := All_LB (Enabled_LBI (Cur_Line)).Indentation;

            pragma Assert
              (At_End (Out_Buf) = (Cur_Line = Last_Index (Enabled_LBI)));
            exit Char_Loop when Cur_Line = Last_Index (Enabled_LBI);

            Cur_Line := Cur_Line + 1;
--            Dbg_Out.Put
--              ("    point = \1, next break = ",
--               Image (Point (Out_Buf)));
--            Dump_Marker (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark);
--            Dbg_Out.Put ("\n");
            pragma Assert
              (Point (Out_Buf) <=
               Position (Out_Buf, All_LB (Enabled_LBI (Cur_Line)).Mark));
         end loop; -- through Enabled_LBI table

         Comment_Seen_Flag := False;

         --  We can't be At_End, because we would have done "exit Char_Loop"
         --  above.

         pragma Assert (not At_End (Out_Buf));
         pragma Assert (Cur (Out_Buf) not in NL | W_NUL);

         pragma Assert (Point (Out_Buf) <= New_Sloc_First (Newer_Tokns));
         if Point (Out_Buf) = New_Sloc_First (Newer_Tokns) then
            if At_Line_Start then
               Append_Spaces
                 (Newer_Tokns, Count => Indentation,
                  Org => "NLs/Ind indent");
            end if;

            if At_Line_Start then
               pragma Assert (not Internal_To_Comment);
               for J in 1 .. Indentation loop
                  Insert (Out_Buf, ' ');
               end loop;
               At_Line_Start := False;
            end if;

            pragma Assert (Point (Out_Buf) = New_Sloc_First (Newer_Tokns));
            New_To_Newer_2;
         end if;

         if At_Line_Start then
            if Internal_To_Comment then
               Internal_To_Comment := False;
            else
               Append_Spaces
                 (Newer_Tokns, Count => Indentation,
                  Existing_OK => True,
                  Org => "NLs/Ind indent 2");
            end if;
            for J in 1 .. Indentation loop
               Insert (Out_Buf, ' ');
            end loop;
            At_Line_Start := False;

            if Point (Out_Buf) = New_Sloc_First (Newer_Tokns) then
               New_To_Newer_2;
            end if;
         end if;
         Move_Forward (Out_Buf);
      end loop Char_Loop;

      pragma Assert (At_End (Out_Buf));
      pragma Assert (Cur_Line = Last_Index (Enabled_LBI));
      Reset (Out_Buf);
      pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));

      Clear (All_LB);
      Clear (All_LBI);
      Clear (Enabled_LBI);
      Clear (Syntax_LBI);
      Assert_No_LB (Lines_Data);

      Append_Tokn (Newer_Tokns, End_Of_Input);
      Scanner.Melt_Tokns (New_Tokns);
      Move_Tokns (Target => New_Tokns, Source => Newer_Tokns);
      Scanner.Freeze_Tokns (New_Tokns);

      pragma Debug (Check_Tokens (Out_Buf, New_Tokns,
         "end of Insert_NLs_And_Indentation", Cmd));
   end Insert_NLs_And_Indentation;

   procedure Insert_Alignment_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line);

   procedure Insert_Alignment
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line) is
   begin
      if Alignment_Enabled (Cmd) then
         Insert_Alignment_Helper (Lines_Data, Cmd);
      end if;
   end Insert_Alignment;

   procedure Insert_Alignment_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Out_Buf_Line_Ends : Marker_Vector renames Lines_Data.Out_Buf_Line_Ends;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
      Tabs : Tab_Vector renames Lines_Data.Tabs;

      procedure Calculate_Num_Blanks;

      procedure Calculate_Num_Blanks is
         use Scanner;

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

         Ignored : Boolean := Scanner.Get_Tokns
            (Out_Buf, Out_Tokns, Utils.Ada_Version,
             Line_Ends => Out_Buf_Line_Ends'Unchecked_Access);
         Cur_Tok : Tokn_Cursor := First (Out_Tokns'Access);
         Cur_Tab_Index : Tab_Index := 1;
         function Cur_Tab return Tab_Rec is (Tabs (Cur_Tab_Index));

         First_Line_Tabs, Cur_Line_Tabs : Tab_In_Line_Vector;
         --  Tabs for first line of paragraph and for current line.

         procedure Process_Line;
         --  Process a single line in Out_Buf. Collect together all relevant
         --  tabs in Cur_Line_Tabs. All tabs in Cur_Line_Tabs must have the
         --  same Tree (that of the first tab on the line). Other tabs (for
         --  more nested constructs) are skipped. So for example:
         --     X : T (Discrim => 123) := (This | That => 345);
         --  we collect two tabs for ':' and ':=', which have the same Tree
         --  (a variable declaration tree). The '|' and '=>' characters in
         --  the discriminant constraint and the aggregate also have tabs, but
         --  these are skipped, because their Tree is different (more nested).
         --  If there are no tabs on the line, then of course Cur_Line_Tabs
         --  will be empty. In addition, if we have something like:
         --     A := (1 | 2 | 3 => ...);
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

         procedure Process_Line is
            Tab_Mismatch : Boolean := False;
            First_Time : Boolean := True;
            Tree : Ada_Node;
            Insertion_Point : Marker;
            Have_Insertion_Point : Boolean := False;
            IP_Index_In_Line : Tab_Index_In_Line;
         begin
            while Kind (Cur_Tok) not in End_Of_Input | End_Of_Line
            loop
               pragma Assert
                 (Sloc (Cur_Tok).First <= Position (Out_Buf, Cur_Tab.Mark));
               --  We can have two tabs at the same place if the second one is
               --  fake. Also for implicit 'in' mode, etc. Hence 'while', not
               --  'if' here:
               pragma Assert
                 ((Sloc (Cur_Tok).First = Position (Out_Buf, Cur_Tab.Mark))
                = (Sloc (Cur_Tok).Firstx = Cur_Tab.Mark));

               while Sloc (Cur_Tok).Firstx = Cur_Tab.Mark loop
                  if First_Time then
                     pragma Assert (Is_Empty (Cur_Line_Tabs));
                     First_Time := False;
                     Tree := Cur_Tab.Tree;
                  end if;
                  if Cur_Tab.Tree = Tree then
                     if Cur_Tab.Is_Insertion_Point then
                        pragma Assert (not Have_Insertion_Point);
                        Have_Insertion_Point := True;
                        Insertion_Point := Cur_Tab.Mark;
                        IP_Index_In_Line := Cur_Tab.Index_In_Line;

                     --  Ignore if too many tabs in one line:

                     elsif Last_Index (Cur_Line_Tabs) <
                       Tab_Index_In_Line'Last
                     then
                        Append (Cur_Line_Tabs, Cur_Tab_Index);
                        if Cur_Tab.Index_In_Line /=
                          Last_Index (Cur_Line_Tabs)
                        then
                           Tab_Mismatch := True;
                        end if;

                        Tabs (Cur_Tab_Index).Col := Sloc (Cur_Tok).Col;
                        if Have_Insertion_Point then
                           Have_Insertion_Point := False;
                           pragma Assert
                             (Cur_Tab.Index_In_Line = IP_Index_In_Line);
                           Tabs (Cur_Tab_Index).Mark := Insertion_Point;
                        end if;
                     end if;
                  end if;

                  Cur_Tab_Index := Cur_Tab_Index + 1;
               end loop;

               Next_ss (Cur_Tok);
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
                        Put_Token (Cur_Tok);
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
         begin
            if Is_Empty (X) then
               return;
            end if;

            Dbg_Out.Put ("\1: \t", Name);

            for J in 1 .. Last_Index (X) loop
               if J /= 1 then
                  Dbg_Out.Put ("; ");
               end if;
               Dbg_Out.Put ("\1", Tab_Image (Out_Buf, Tabs, X (J)));
            end loop;
            Dbg_Out.Put ("\n");
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
         while Kind (Cur_Tok) /= End_Of_Input loop
            declare
--               First_Char_In_Line : constant Natural :=
--                 Sloc (Cur_Tok).First - Sloc (Cur_Tok).Col + 1;
            begin
               Process_Line;

--               Dbg_Out.Put ("<<");
--
--               for X in First_Char_In_Line .. Sloc (Cur_Tok).First - 1 loop
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

               Next_ss (Cur_Tok);
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
                     F_Tab := (others => <>);
                     C_Tab := (others => <>);
                  end if;
                  Append (Paragraph_Tabs, Cur_Line_Tabs);
                  Clear (Cur_Line_Tabs);
               end if;
            end;
--            Dbg_Out.Put ("\n");
         end loop;

         pragma Assert (Cur_Tab_Index = Last_Index (Tabs));
      end Calculate_Num_Blanks;

   --  Start of processing for Insert_Alignment_Helper

   begin
      Clear (Out_Buf_Line_Ends);

      --  First go through the tabs and set their Num_Blanks field to the
      --  appropriate value. Tabs that are not expanded at all will have
      --  Num_Blanks left equal to zero.

      pragma Debug
        (Format_Debug_Output
           (Lines_Data, "before Calculate_Num_Blanks"));
      Calculate_Num_Blanks;
      pragma Debug
        (Format_Debug_Output
           (Lines_Data, "after Calculate_Num_Blanks"));

      --  Now go through the buffer, inserting blanks for tabs that should be
      --  expanded. Don't expand a tab if it would make the line too long.

      declare
         Cur_Tab_Index : Tab_Index := 1;
         Cur_Tab       : Tab_Rec   := Tabs (Cur_Tab_Index);
         Cur_Line_Num  : Positive  := 1;
      begin
         while not At_End (Out_Buf) loop
            pragma Assert
              (Point (Out_Buf) <= Position (Out_Buf, Cur_Tab.Mark));

            while At_Point (Out_Buf, Cur_Tab.Mark) loop
               Error_Sloc := (Slocs.Line_Number (Cur_Line_Num), 1);
               if Scanner.Line_Length
                   (Out_Buf,
                    Out_Buf_Line_Ends,
                    Cur_Line_Num) + Cur_Tab.Num_Blanks <=
                 Arg (Cmd, Max_Line_Length)
               then
                  for J in 1 .. Cur_Tab.Num_Blanks loop
                     Insert (Out_Buf, ' ');
                  end loop;
               end if;
               Cur_Tab_Index := Cur_Tab_Index + 1;
               Cur_Tab       := Tabs (Cur_Tab_Index);
            end loop;
            if Cur (Out_Buf) = NL then
               Cur_Line_Num := Cur_Line_Num + 1;
            end if;
            Move_Forward (Out_Buf);
         end loop;
         pragma Assert (Cur_Tab_Index = Last_Index (Tabs));
      end;

      Reset (Out_Buf);
      pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));
      Clear (Tabs);
   end Insert_Alignment_Helper;

   procedure Keyword_Casing
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
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
               Ignored : Boolean := Get_Tokns
                 (Out_Buf, Out_Tokns, Utils.Ada_Version);
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

                     exit when At_Point (Out_Buf, Sloc (Out_Tok).Lastx);
                  end loop;
               end loop Outer_Loop;
               Reset (Out_Buf);
            end;
      end case;
   end Keyword_Casing;

   procedure Insert_Form_Feeds_Helper
     (Lines_Data : in out Lines_Data_Rec);

   procedure Insert_Form_Feeds
     (Lines_Data : in out Lines_Data_Rec;
      Cmd : Command_Line) is
   begin
      if Arg (Cmd, Ff_After_Pragma_Page) then
         Insert_Form_Feeds_Helper (Lines_Data);
      end if;
   end Insert_Form_Feeds;

   procedure Insert_Form_Feeds_Helper
     (Lines_Data : in out Lines_Data_Rec)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;
      use Scanner;
      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns, Utils.Ada_Version);
      Prev_Prev_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      Prev_Tok : Tokn_Cursor := Next_ss (Prev_Prev_Tok);
      Out_Tok : Tokn_Cursor := Next_ss (Prev_Tok);
      --  Out_Tok skips sentinel and first 3 tokens
   begin
      while not At_Last (Out_Tok) loop
         Error_Sloc := To_Langkit (Sloc (Out_Tok));
         loop
            Move_Forward (Out_Buf);
            exit when At_Point (Out_Buf, Sloc (Out_Tok).Lastx);
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
   end Insert_Form_Feeds_Helper;

   procedure Copy_Pp_Off_Regions_Helper
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec);

   procedure Copy_Pp_Off_Regions
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec;
      Pp_Off_Present : Boolean) is
   begin
      --  Optimize by skipping this phase if there are no Pp_Off_Comments
      if Pp_Off_Present then
         Copy_Pp_Off_Regions_Helper (Src_Buf, Lines_Data);
      end if;
   end Copy_Pp_Off_Regions;

   procedure Copy_Pp_Off_Regions_Helper
     (Src_Buf : in out Buffer;
      Lines_Data : in out Lines_Data_Rec)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;

      --  The Src_Buf contains a sequence of zero or more OFF and ON
      --  commands. The first must be OFF, then ON, then OFF and so on,
      --  alternating. If that weren't true, we would have gotten an error in
      --  Insert_Comments_And_Blank_Lines, in which case we don't get here.
      --  The final End_Of_Input acts as an ON or OFF as appropriate.
      --  The Out_Buf contains a corresponding sequence with the same
      --  number of OFF's and ON's.

      --  Pretty printing is ON between the beginning and the first OFF, then
      --  OFF until the next ON, and so on.

      use Scanner;

      New_Buf : Buffer;
      --  Buffers don't support deletion, so we need to build up a whole new
      --  Buffer. This will be moved into Out_Buf when we are done.

      procedure Get_Next_Off_On
        (Tok : in out Tokn_Cursor;
         Expect : Pp_Off_On_Comment);
      --  Get the next OFF or ON (or End_Of_Input). The token itself is
      --  returned in Tok. Expect is purely for assertions; it alternates
      --  between OFF and ON; Tok must be as expected (or End_Of_Input).

      procedure Copy (Buf : in out Buffer; Up_To : Marker);
      --  Copy from Buf to New_Buf, up to the given marker.

      procedure Skip (Buf : in out Buffer; Up_To : Marker);
      --  Move forward in Buf, up to the given marker, ignoring the characters.

      procedure Get_Next_Off_On
        (Tok : in out Tokn_Cursor;
         Expect : Pp_Off_On_Comment) is
      begin
         loop
            Next_ss (Tok);
            Error_Sloc := To_Langkit (Scanner.Sloc (Tok));
            exit when Kind (Tok) in Pp_Off_On_Comment | End_Of_Input;
         end loop;
         pragma Assert (Kind (Tok) in Expect | End_Of_Input);
         pragma Assert
           (Kind (Prev_ss (Tok)) in Start_Of_Input | End_Of_Line);
      end Get_Next_Off_On;

      procedure Copy (Buf : in out Buffer; Up_To : Marker) is
      begin
         while not At_Point (Buf, Up_To) loop
            Insert_Any (New_Buf, Cur (Buf));
            Move_Forward (Buf);
         end loop;
      end Copy;

      procedure Skip (Buf : in out Buffer; Up_To : Marker) is
      begin
         while not At_Point (Buf, Up_To) loop
            Move_Forward (Buf);
         end loop;
      end Skip;

      Ignored : Boolean := Get_Tokns
        (Out_Buf, Out_Tokns, Utils.Ada_Version);
      Src_Tok : Tokn_Cursor := First (Src_Tokns'Access);
      Out_Tok : Tokn_Cursor := First (Out_Tokns'Access);

   --  Start of processing for Copy_Pp_Off_Regions_Helper

   begin
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
         Copy (Out_Buf, Up_To => Sloc (Prev_ss (Out_Tok)).Lastx);
         Get_Next_Off_On (Src_Tok, Expect => Pp_Off_Comment);
         Skip (Src_Buf, Up_To => Sloc (Prev_ss (Src_Tok)).Lastx);

         pragma Assert
           ((Kind (Out_Tok) = End_Of_Input) = (Kind (Src_Tok) = End_Of_Input));
         exit when Kind (Out_Tok) = End_Of_Input;

         Get_Next_Off_On (Src_Tok, Expect => Pp_On_Comment);
         Copy (Src_Buf, Up_To => Sloc (Src_Tok).Lastx);
         Get_Next_Off_On (Out_Tok, Expect => Pp_On_Comment);
         Skip (Out_Buf, Up_To => Sloc (Out_Tok).Lastx);

         pragma Assert
           ((Kind (Out_Tok) = End_Of_Input) = (Kind (Src_Tok) = End_Of_Input));
         exit when Kind (Out_Tok) = End_Of_Input;
      end loop;

      Reset (Src_Buf);
      Reset (Out_Buf);
      Reset (New_Buf);

      Move (Target => Out_Buf, Source => New_Buf);
   end Copy_Pp_Off_Regions_Helper;

   package Tok_Phases with Unreferenced is
      --  ???The plan is to implement the new phases here, and get rid of
      --  Tok_Phases once it's all working.
   end Tok_Phases;

   procedure Post_Tree_Phases
     (Lines_Data : in out Lines_Data_Rec;
      Messages : out Scanner.Source_Message_Vector;
      Src_Buf : in out Buffer;
      Cmd : Command_Line;
      Partial : Boolean)
   is

      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      New_Tokns : Scanner.Tokn_Vec renames Lines_Data.New_Tokns;

      Pp_Off_Present : Boolean := False;
   begin
      --  ????We probably want to do more, but this is for gnatstub
      --  for now.
      if Partial then
         Clear (All_LB);
         Clear (All_LBI);
         Clear (Lines_Data.Tabs);
         return;
      end if;

      Scanner.Freeze_Tokns (New_Tokns);
      Split_Lines (Lines_Data, Cmd, First_Time => True);
      Enable_Line_Breaks_For_EOL_Comments (Src_Buf, Lines_Data, Cmd);
      Insert_Comments_And_Blank_Lines
        (Src_Buf, Messages, Lines_Data, Cmd, Pp_Off_Present);
      Split_Lines (Lines_Data, Cmd, First_Time => False);
      Insert_NLs_And_Indentation (Lines_Data, Cmd);
      Insert_Alignment (Lines_Data, Cmd);
      Keyword_Casing (Lines_Data, Cmd);
      Insert_Form_Feeds (Lines_Data, Cmd);
      Copy_Pp_Off_Regions (Src_Buf, Lines_Data, Pp_Off_Present);
      Scanner.Melt_Tokns (New_Tokns);

      --  The following pass doesn't modify anything; it just checks that the
      --  sequence of tokens we have constructed matches the original source
      --  code (with some allowed exceptions).

      Final_Check (Lines_Data, Src_Buf, Cmd);

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
   begin
      Error_Sloc := To_Langkit (Scanner.Sloc (Src_Tok));

      if Enable_Token_Mismatch then
         declare
            use Scanner;
            Num_Toks : constant Tokn_Index := 8;
            --  Number of tokens before and after the mismatch to print
            First_Src_Tok : constant Tokn_Cursor := Pred (Src_Tok, Num_Toks);
            Last_Src_Tok : constant Tokn_Cursor := Succ (Src_Tok, Num_Toks);
            First_Out_Tok : constant Tokn_Cursor := Pred (Out_Tok, Num_Toks);
            Last_Out_Tok : constant Tokn_Cursor := Succ (Out_Tok, Num_Toks);
         begin
            Utils.Dbg_Out.Output_Enabled := True;
            Text_IO.Put_Line ("Src_Buf:");
            Dump_Buf (Src_Buf);
            Text_IO.Put_Line ("Out_Buf:");
            Dump_Buf (Out_Buf);

            Text_IO.Put_Line
              (Text_IO.Standard_Output,
               Message &
                 ": Token mismatch: " &
                 Str (Text (Src_Tok)).S &
                 " --> " &
                 Str (Text (Out_Tok)).S);
            Text_IO.Put_Line (Text_IO.Standard_Output, "Src tokens:");
            Put_Tokens
              (First     => First_Src_Tok,
               After_Last => Next (Last_Src_Tok),
               Highlight => Src_Tok);
            Text_IO.Put_Line
              (Text_IO.Standard_Output,
               "========================================");
            Text_IO.Put_Line (Text_IO.Standard_Output, "Out tokens:");
            Put_Tokens
              (First     => First_Out_Tok,
               After_Last => Next (Last_Out_Tok),
               Highlight => Out_Tok);

            Text_IO.Put_Line (Text_IO.Standard_Output, "Src text:");
            Wide_Text_IO.Put
              (Wide_Text_IO.Standard_Output, Slice (Src_Buf,
                      Sloc (First_Src_Tok).First,
                      Sloc (Last_Src_Tok).Last,
                      Lines => True));
            Text_IO.Put_Line (Text_IO.Standard_Output, "Out text:");
            Wide_Text_IO.Put
              (Wide_Text_IO.Standard_Output, Slice (Out_Buf,
                      Sloc (First_Out_Tok).First,
                      Sloc (Last_Out_Tok).Last,
                      Lines => True));
         end;
      end if;

      raise Token_Mismatch;
   end Raise_Token_Mismatch;

   procedure Final_Check_Helper
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      Src_Tokns : Scanner.Tokn_Vec renames Lines_Data.Src_Tokns;
      Out_Tokns : Scanner.Tokn_Vec renames Lines_Data.Out_Tokns;

      use Scanner;

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean;
      --  Similar to Match in Insert_Comments_And_Blank_Lines, but here we need
      --  to deal with comments.

      procedure Move_Past_Char;
      procedure Move_Past_Out_Tok;
      --  These are similar to the procedures in
      --  Insert_Comments_And_Blank_Lines, but here we don't need to keep
      --  track of line breaks.

      generic
         Tok    : in out Tokn_Cursor;
         Result : in out WChar_Vector;
         Is_Out : Boolean;
      procedure Collect_Comments;
      --  Collect up all the text of a sequence of Whole_Line_Comments,
      --  ignoring changes made by paragraph filling. Paragraph_Filling might
      --  have changed blank to NL and vice versa, and it turns a series of
      --  blanks into a single one. Similarly needed if Comments_Gnat_Beginning
      --  is True.
      --
      --  The reason this is generic is that when Is_Out is True, it calls
      --  Move_Past_Out_Tok, so we are evilly depending on the fact that
      --  Out_Tok and Tok are the same object.

      function Match (Src_Tok, Out_Tok : Tokn_Cursor) return Boolean is
      begin
         return R : Boolean do
            if Kind (Src_Tok) = Kind (Out_Tok) then
               case Kind (Src_Tok) is
                  when End_Of_Line | Spaces =>
                     raise Program_Error;

                  when Start_Of_Input | End_Of_Input | Line_Break_Token |
                    Tab_Token | Other_Lexeme =>
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

                  when Comment_Kind =>
                     R :=
                       (Arg (Cmd, Comments_Gnat_Beginning)
                        or else Leading_Blanks (Src_Tok) =
                                Leading_Blanks (Out_Tok))
                       and then Text (Src_Tok) = Text (Out_Tok);
               end case;

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
        (Out_Buf, Out_Tokns, Utils.Ada_Version);

      Src_Tok : Tokn_Cursor := Next_ss (First (Src_Tokns'Access));
      Out_Tok : Tokn_Cursor := Next_ss (First (Out_Tokns'Access));
      --  Cursors into Src_Tokns and Out_Tokns, respectively. Skip the
      --  first Start_Of_Input token, which is just a sentinel.

      procedure Move_Past_Char is
      begin
         --  Step past character

         Move_Forward (Out_Buf);
      end Move_Past_Char;

      procedure Move_Past_Out_Tok is
      begin
         --  ???Make sure we're not moving past multiple tokens here. Move past
         --  whitespace, then assert we're at token start, then move to end. Or
         --  something like that.
         loop
            pragma Assert
              (Point (Out_Buf) < Position (Out_Buf, Sloc (Out_Tok).Lastx));
            Move_Past_Char;
            exit when At_Point (Out_Buf, Sloc (Out_Tok).Lastx);
         end loop;
      end Move_Past_Out_Tok;

      procedure Collect_Comments is
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
               if Is_Out then
                  Move_Past_Out_Tok;
                  --  Here is where we depend on the fact that if Is_Out, then
                  --  Tok and Out_Tok are the same object.
               end if;
               Next_ss (Tok);
               exit when Kind (Tok) not in End_Of_Line;
--  ???Should we skip Spaces? exit when Kind (Tok) not in End_Of_Line | Spaces;
            end loop;
         end loop;
      end Collect_Comments;

   --  Start of processing for Final_Check_Helper

   begin
      pragma Assert (Cur (Out_Buf) = NL);
      Move_Forward (Out_Buf); -- skip sentinel

      --  Skip initial End_Of_Line token
      pragma Assert (Kind (Out_Tok) in End_Of_Line);
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

         if Kind (Src_Tok) not in End_Of_Line
           and then
           (Match (Src_Tok, Out_Tok)
            or else (Kind (Src_Tok) = '!' and then Kind (Out_Tok) = '|'))
         then
            exit when Kind (Src_Tok) = End_Of_Input;
            --  i.e. exit when both Src and Out are at end of input

            Move_Past_Out_Tok;

            Next_ss (Src_Tok);
            Next_ss (Out_Tok);

         else
            --  If we're filling comments, then the comments might not match
            --  up. For example, a line break could be added such that the
            --  first line is too short to be considered part of a fillable
            --  comment paragraph, thus turning one comment into two. So
            --  we collect them all together and check that their text
            --  more-or-less matches.
            --
            --  Similarly, we do this if Comments_Gnat_Beginning. For example,
            --  if one comment starts with a single blank and the next starts
            --  with two, then they will not look like a single paragraph
            --  during Insert_Comments_And_Blank_Lines, but here they will,
            --  because an extra blank has been added to the first.
            --
            --  Actually, we need to do this in any case: if two comments in
            --  the input are not indented the same, they will be indented the
            --  same in the output, and thus appear to be a fillable paragraph.

            if Kind (Src_Tok) in Whole_Line_Comment
              and then Kind (Out_Tok) in Whole_Line_Comment
            then
               declare
                  Src_Comments : WChar_Vector;
                  Out_Comments : WChar_Vector;

                  procedure Collect_Comments_Src is new Collect_Comments
                    (Src_Tok, Src_Comments, Is_Out => False);

                  procedure Collect_Comments_Out is new Collect_Comments
                    (Out_Tok, Out_Comments, Is_Out => True);

               begin
                  Collect_Comments_Src;
                  Collect_Comments_Out;
                  if Src_Comments /= Out_Comments then
                     if Enable_Token_Mismatch then
                        Text_IO.Put_Line
                          (Text_IO.Standard_Output,
                           To_UTF8 (To_Array (Src_Comments)) &
                           " --> " &
                           To_UTF8 (To_Array (Out_Comments)));
                     end if;
                     Raise_Token_Mismatch
                       ("Final_Check 1",
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
                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);

                  exit when Kind (Out_Tok) /= '.';

                  Move_Past_Out_Tok;
                  Next_ss (Out_Tok);
                  if not Sname_83 (Out_Tok) then
                     Raise_Token_Mismatch
                       ("Final_Check 2",
                        Lines_Data, Src_Buf, Src_Tok, Out_Tok);
                  end if;
               end loop;
               if Kind (Src_Tok) /= ';' then
                  Raise_Token_Mismatch
                    ("Final_Check 3", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
               end if;

            elsif Kind (Src_Tok) in End_Of_Line then
               Next_ss (Src_Tok);

            elsif Kind (Out_Tok) in End_Of_Line then
               Move_Past_Out_Tok;
               Next_ss (Out_Tok);

            --  Else print out debugging information and crash. This avoids
            --  damaging the source code in case of bugs.

            else
               Raise_Token_Mismatch
                 ("Final_Check 4", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
            end if;
         end if;
      end loop;

      while not At_End (Out_Buf) loop
         if not Is_Line_Terminator (Cur (Out_Buf)) then
            Raise_Token_Mismatch
              ("Final_Check 5", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
         end if;

         Move_Forward (Out_Buf);
      end loop;

      Reset (Out_Buf);

      if not At_Last (Src_Tok) or else not At_Last (Out_Tok) then
         Raise_Token_Mismatch
           ("Final_Check 6", Lines_Data, Src_Buf, Src_Tok, Out_Tok);
      end if;
   end Final_Check_Helper;

   procedure Final_Check
     (Lines_Data : in out Lines_Data_Rec;
      Src_Buf : in out Buffer;
      Cmd : Utils.Command_Lines.Command_Line)
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
   begin
      if Disable_Final_Check then
         return;
      end if;
      if Enable_Token_Mismatch then
         declare
            Old_Out_Buf : constant WChar_Vector := To_Vector (Out_Buf);
         begin
            Final_Check_Helper (Lines_Data, Src_Buf, Cmd);
            pragma Assert (To_Vector (Out_Buf) = Old_Out_Buf);
            pragma Debug (Assert_No_Trailing_Blanks (Out_Buf));
         end;
      else
         Final_Check_Helper (Lines_Data, Src_Buf, Cmd);
      end if;
   end Final_Check;

   ----------------

   --  Debugging:

   function Line_Text
     (Lines_Data : Lines_Data_Rec;
      F, L : Line_Break_Index_Index) return W_Str
   is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      First  : constant Line_Break := All_LB (All_LBI (F));
      Last   : constant Line_Break := All_LB (All_LBI (L));
      Result : constant W_Str      := Slice (Out_Buf, First.Mark, Last.Mark);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Line_Text;

   function Tab_Image
     (Out_Buf : Buffer; Tabs : Tab_Vector; X : Tab_Index) return String
   is
      Tab : constant Tab_Rec := Tabs (X);
   begin
      return "Tabs(" &
        Image (Integer (X)) &
        ") = ^" &
        Image (Integer (Tab.Index_In_Line)) &
        Str (Tab.Token).S &
        ASCII.HT &
        " at " &
        Image (Position (Out_Buf, Tab.Mark)) &
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

   procedure Put_Buf_With_Marks (Lines_Data : Lines_Data_Rec) is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
      All_LB : Line_Break_Vector renames Lines_Data.All_LB;
      All_LBI : Line_Break_Index_Vector renames Lines_Data.All_LBI;
      Tabs : Tab_Vector renames Lines_Data.Tabs;

      Cur_Line : Line_Break_Index_Index := 1;
      Cur_Tab  : Tab_Index        := 1;
      S        : constant String  := To_String (Out_Buf);

      use Dbg_Out;
   begin
      if not Dbg_Out.Output_Enabled then
         return;
      end if;

      for Cur_Char in S'Range loop
         while Cur_Tab <= Last_Index (Tabs)
           and then Position (Out_Buf, Tabs (Cur_Tab).Mark) = Cur_Char
         loop
            Put_Char ('^');
            Cur_Tab := Cur_Tab + 1;
         end loop;

         declare
            Indentation : Natural := 0;
         begin
            while Cur_Line <= Last_Index (All_LBI)
              and then
                Position (Out_Buf, All_LB (All_LBI (Cur_Line)).Mark) =
                Cur_Char
            loop
               declare
                  Break : Line_Break renames All_LB (All_LBI (Cur_Line));
               begin
                  if Break.Enabled then
                     Indentation := Break.Indentation;
                  end if;

                  if Break.Hard then
                     Put_Char ('$');
                  elsif Break.Enabled then
                     Put_Char ('!');
                  else
                     Put_Char ('?');
                  end if;
                  Put ("\1", Image (Integer (Break.Level)));
               end;
               Cur_Line := Cur_Line + 1;
            end loop;

            Put_Char (S (Cur_Char));

            for J in 1 .. Indentation loop
               Put_Char ('_');
            end loop;
         end;
      end loop;
   end Put_Buf_With_Marks;

   procedure Put_Line_Break (Out_Buf : Buffer; Break : Line_Break) is
      use Dbg_Out;
   begin
      Put
        ("\1\2, \3, \4, \5\n",
         String'(1 .. Break.Indentation => '_'),
         Image (Position (Out_Buf, Break.Mark)),
         (if Break.Enabled then "hard" else "soft"),
         (if Break.Enabled then "enabled" else "disabled"),
         (if At_Point (Out_Buf, Break.Mark) then "at point"
          elsif
            Position (Out_Buf, Break.Mark) = Point (Out_Buf) - 1
          then
            "just before point"
          else "not at point"));
   end Put_Line_Break;

   procedure Put_Line_Breaks (Lines_Data : Lines_Data_Rec) is
      Out_Buf : Buffer renames Lines_Data.Out_Buf;
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
           ("\1:\t\2\3\4",
            Image (Line_Num),
            String'(1 .. All_LB (All_LBI (Cur_Line)).Indentation => '_'),
            Image (Position (Out_Buf, All_LB (All_LBI (Cur_Line)).Mark)),
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
            Put ("\t<<\1>>",
                 To_UTF8 (Line_Text (Lines_Data, Cur_Line, L)));
         end if;

         Put ("#\1", Image (All_LB (All_LBI (Cur_Line)).UID));
         Put ("\n");
      end loop;
      for Cur_Line in 1 .. Last_Index (All_LBI) loop
         Put_Line_Break (Out_Buf, All_LB (All_LBI (Cur_Line)));
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
         Put ("\1\n", Tab_Image (Out_Buf, Tabs, X));
      end loop;

      Put_Buf_With_Marks (Lines_Data);

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

   package body Tok_Phases is
      pragma Style_Checks ("M82"); -- because these will eventually be unnested
   end Tok_Phases;

end Pp.Formatting;
