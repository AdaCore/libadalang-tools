------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . S C A N N E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Utils.Char_Vectors;
with Utils.Vectors;
with Utils.Symbols;

with Pp.Buffers; use Pp.Buffers;
use Pp.Buffers.Marker_Vectors;
--  use all type Pp.Buffers.Marker_Vector;

package Pp.Scanner is

   --  This package provides a simple lexical scanner for Ada tokens. There are
   --  some unusual things about this scanner:
   --
   --     We don't distinguish most of the different kinds of tokens; most are
   --     lumped together under the Lexeme kind, and reserved words are lumped
   --     together under Reserved_Word. We only distinguish where we need to.
   --
   --     We do not ignore comments; a comment is considered to be a token.
   --
   --     We do not ignore blank lines. We do ignore a single line break,
   --     if Ignore_Single_Line_Breaks is True. Other whitespace (blanks and
   --     tabs) between tokens is always ignored.
   --
   --     We don't check for errors, because we're in ASIS, where Ada code is
   --     known to be legal.

   package Syms renames Utils.Symbols;

   type Token_Kind is
     (Nil,
      Start_Of_Input,
      End_Of_Input,
      Identifier,
      Reserved_Word,
      String_Literal,
      Numeric_Literal,
      Lexeme, -- misc lexemes as defined in the RM
      Pp_Off_Comment,
      --  A whole-line comment that matches the --pp-off string
      Pp_On_Comment,
      --  A whole-line comment that matches the --pp-on string
      Special_Comment,
      --  A "special" comment; that is, one that should not be formatted in any
      --  way. Special comments are not fillable.
      Fillable_Comment,
      --  A fillable whole-line comment; that is, one that should be
      --  filled if filling is turned on.
      Other_Whole_Line_Comment,
   --  A comment that appears by itself on a line. Multiple comments that may
   --  be filled as a "paragraph" are combined into a single Whole_Line_Comment
   --  token. This comment is a Whole_Line_Comment.
      End_Of_Line_Comment,
   --  A comment that appears at the end of a line, after some other
   --  program text. The above comment starting "misc lexemes" is an
   --  End_Of_Line_Comment.
      End_Of_Line, -- First in a series of one or more NLs.
      Blank_Line); -- Second, third, ... in a series of one or more NLs.

   subtype Whole_Line_Comment is Token_Kind with
     Predicate => Whole_Line_Comment in
       Pp_Off_Comment | Pp_On_Comment | Special_Comment |
       Fillable_Comment | Other_Whole_Line_Comment;

   subtype Comment_Kind is Token_Kind with
        Predicate => Comment_Kind in Whole_Line_Comment | End_Of_Line_Comment;

   subtype Pp_Off_On_Comment is Token_Kind with
        Predicate => Pp_Off_On_Comment in Pp_Off_Comment | Pp_On_Comment;

   type Source_Location is record
      Line, Col : Positive; -- 1-based line and column numbers
      First     : Positive;
      Last      : Natural;

      Firstx, Lastx : Marker;
      --  ???Same information as First&Last. These should replace First&Last
      --  eventually. Note that Lastx points one past the last character.
   end record;

   function First_Pos (Input : Buffer; Sloc : Source_Location) return Positive;
   function Last_Pos (Input : Buffer; Sloc : Source_Location) return Natural;
   --  Absolute position in Input (parameter of Get_Tokens) of the start and
   --  end of the token. So the text of the token is exactly equal to the slice
   --  Input (First..Last). Note that Input'First might not be 1.

   function Image
     (Sloc : Source_Location)
      return String is
     (Image (Sloc.Line) &
      ":" &
      Image (Sloc.Col) &
      "(" &
      Image (Sloc.First) &
      ".." &
      Image (Sloc.Last) &
      ")");

   function Sloc_Image
     (Sloc : Source_Location) return String is
       (Image (Sloc.Line) & ":" & Image (Sloc.Col));

   type Source_Message is record
      --  Message attached to a particular source location.
      Sloc : Source_Location; -- the location
      Text : Char_Vectors.Char_Vector; -- the text of the message
   end record;
   type Source_Message_Array is array (Positive range <>) of Source_Message;

   package Source_Message_Vectors is new Utils.Vectors
     (Index_Type => Positive,
      Element_Type => Source_Message,
      Elements_Array => Source_Message_Array);
   subtype Source_Message_Vector is Source_Message_Vectors.Vector;

   function Message_Image
     (File_Name : String; Sloc : Source_Location) return String is
       (File_Name & ":" & Image (Sloc.Line) & ":" & Image (Sloc.Col));

   type Token is record
      Kind : Token_Kind := Nil;
      Text : Syms.Symbol;
      Normalized : Syms.Symbol;
      Leading_Blanks : Natural;
      Width : Natural;
      Sloc : Source_Location;
   end record;

   --  We would like to make type Token private, but then we can't instantiate
   --  Token_Vectors. But we use accessor functions, as if it were private:

   function Kind (X : Token) return Token_Kind;

   function Text (X : Token) return Syms.Symbol;
   --  The text of the token as it appears in the source, with these
   --  exceptions and clarifications:
   --
   --  Start_Of_Input and End_Of_Input have Text = "".
   --
   --  For Blank_Line: does not include the text of the preceding
   --  End_Of_Line or Blank_Line (i.e. it is usually just LF, but could
   --  be CR/LF -- not LF,LF nor CR,LF,CR,LF).
   --
   --  For comments, the text of the comment excluding the initial "--"
   --  and leading and trailing blanks, and followed by an extra NL. For
   --  multi-line comment "paragraphs", used for filling, NL terminates each
   --  line. The NL at the end isn't really part of the comment; the next
   --  token in the stream will be End_Of_Line. The reason for the extra NL
   --  is that GNATCOLL.Paragraph_Filling expects it, so it's simpler and
   --  more efficient this way.

   function Normalized (X : Token) return Syms.Symbol;
   --  Same as Text, or converted to lower case, depending on the Kind.
   --  Comments have Normalized = No_Name, so we can detect specific
   --  reserved words. For example, the "BEGIN" reserved word will have Text
   --  = "BEGIN" and Normalized = "begin". The comment "-- begin" will have
   --  Text = "begin" and Normalized = No_Name.

   function Leading_Blanks (X : Token) return Natural;
   --  For comments, the number of leading blanks, which are blanks after
   --  the initial "--" and before any nonblank characters. For other
   --  tokens, zero.

   function Width (X : Token) return Natural;
   --  For single-line Whole_Line_Comments, this is the width of the token,
   --  i.e. the same as Sloc.Last-Sloc.First+1, and the same as the length of
   --  Text. For multi-line comments, this is the width of the widest line.
   --  The initial "--" and any leading blanks are included, but the NL's are
   --  not.

   function Sloc (X : Token) return Source_Location;

   type Token_Index is new Positive;
   type Token_Array is array (Token_Index range <>) of Token;
   package Token_Vectors is new Utils.Vectors
     (Token_Index,
      Token,
      Token_Array);
   subtype Token_Vector is Token_Vectors.Vector;
   type Token_Vector_Ptr is access all Token_Vector;
   use Token_Vectors;
   --  use all type Token_Vector;

   function Line_Length
     (Input    : in out Buffer;
      Ends     : Marker_Vector;
      Line_Num : Positive)
      return     Natural;
   --  Doesn't count the NL character. This doesn't work for CR/LF line
   --  endings, which is OK, because we only use it for internally-generated
   --  text that always uses a single NL.

   Default_Pp_Off_String : aliased constant W_Str := "--!pp off";
   Default_Pp_On_String : aliased constant W_Str := "--!pp on";

   type Pp_Off_On_Delimiters_Rec is record
      Off : access constant W_Str := Default_Pp_Off_String'Access;
      On : access constant W_Str := Default_Pp_On_String'Access;
      --  Text of comments for turning pretting printing off and on, including
      --  the leading '--'. For example, if the user specified --pp-off='pp-',
      --  then Off will be "--pp-". A whole-line comment of the form "--pp-"
      --  will disable pretty printing.
      --  We do not want these comments to be fillable.
   end record;

   Gen_Plus : constant Syms.Symbol := Syms.W_Intern ("--gen+");
   --  (style) two spaces required
   Gen_Minus : constant Syms.Symbol := Syms.W_Intern ("--gen-");
   --  Strings to mark start and end of automatically generated code.

   Token_Separator : constant W_Char := W_Char'Val (1);
   --  If Insert_Line_Breaks is False, this character is used instead of hard
   --  line breaks, because otherwise things like "isbegin" can be run
   --  together.

   procedure Get_Tokens
     (Input                     : in out Buffer;
      Result                    : out Token_Vector;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Ignore_Single_Line_Breaks : Boolean;
      Max_Tokens                : Token_Index       := Token_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null;
      Gen_Regions               : Token_Vector_Ptr  := null);
   --  Return in Result the sequence of tokens in the Input string. The
   --  first one is always Start_Of_Input, and the last one End_Of_Input.
   --  Ignore_Single_Line_Breaks means we should skip any End_Of_Line tokens
   --  (but not Blank_Lines). Max_Tokens places a limit on the number of tokens
   --  (not counting Start_Of_Input); we quit before reaching end of input if
   --  we've gotten that many.
   --
   --  If Line_Ends is non-null, we compute all the line endings in
   --  Line_Ends.all, which is a mapping from line numbers to Markers in the
   --  Input string. Each element points to a NL character in the corresponding
   --  buffer.
   --
   --  Comments starting with Gen_Plus and Gen_Minus, and tokens in between, do
   --  not appear in Result. If Gen_Regions is non-null, we use it to return
   --  the sequence of Gen_Plus and Gen_Minus tokens.  The generated code is in
   --  the slices Gen_Regions(1).Sloc..Gen_Regions(2).Sloc,
   --  Gen_Regions(3).Sloc..Gen_Regions(4).Sloc, and so on.

   function Next_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token;
   --  Returns the next token after Index that is not a blank line or comment

   function Prev_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token;
   --  Returns the previous token before Index that is not a blank line or
   --  comment

   function Get_Token
     (Input : W_Str; Ada_Version : Ada_Version_Type)
     return Token;
   --  Get just one token, ignoring single line breaks

   procedure Check_Same_Tokens (X, Y : Token_Vector);
   --  Checks that X and Y are the same except for Slocs and line breaks; raise
   --  an exception if not.

   function In_Gen_Regions
     (Line : Positive; Gen_Regions : Token_Vector) return Boolean;
   --  True if the line number is within one of the regions of Gen_Regions.
   --  The comments are always on a line by themselves, so we don't have to
   --  worry about column numbers.

   procedure Put_Token (Tok : Token; Index : Token_Index := 1);
   procedure Put_Tokens
     (Tokens    : Token_Array;
      Highlight : Token_Index'Base := 0);
   procedure Put_Tokens
     (Tokens    : Token_Vector;
      First     : Token_Index'Base := 1;
      Last      : Token_Index'Base := Token_Index'Last;
      Highlight : Token_Index'Base := 0);
   --  Put token(s) to standard output (even if Text_IO.Current_Output has been
   --  redirected). The tokens come out in compilable form, one per line, with
   --  the text of the token first, and the other information commented out.
   --  This one-token-per line code can be used for testing the scanner -- it
   --  should have identical semantics to the original Ada code. First and Last
   --  indicate a slice of Tokens, and we tolerate out-of-bounds indices.
   --  We draw a comment line before Highlight.

   procedure Dump_Token (Tok : Token);
   procedure Dump_Tokens (Tokens : Token_Array);

private

   function Kind (X : Token) return Token_Kind is (X.Kind);
   function Text (X : Token) return Syms.Symbol is (X.Text);
   function Normalized (X : Token) return Syms.Symbol is (X.Normalized);
   function Sloc (X : Token) return Source_Location is (X.Sloc);

end Pp.Scanner;
