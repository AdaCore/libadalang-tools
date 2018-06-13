------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . S C A N N E R                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

private with Utils.Var_Length_Ints;

package Pp.Scanner is

   --  This package provides a simple lexical scanner for Ada tokens. There are
   --  some unusual things about this scanner:
   --
   --     We do not ignore comments; a comment is considered to be a token.
   --
   --     We do not ignore line breaks. Blanks between tokens are always
   --     ignored. Other space characters (tabs, NO-BREAK SPACE, etc.) are
   --     removed in Buffers.
   --
   --     We don't check for errors, because we're in ASIS, where Ada code is
   --     known to be legal. ???No longer true.

   package Syms renames Utils.Symbols;

   type Opt_Token_Kind is
     (Nil,
      Identifier,
      Character_Literal,
      String_Literal,
      Numeric_Literal,
      Spaces, -- A sequence of one or more space characters.
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
   --  token.
      End_Of_Line_Comment,
   --  A comment that appears at the end of a line, after some other
   --  program text.

      --  All the rest are in Same_Text_Kind (see body):

      Start_Of_Input,
      End_Of_Input,
      End_Of_Line, -- First in a series of one or more NLs.
      Blank_Line, -- Second, third, ... in a series of one or more NLs.

      '!', '#', '$', '?', '[', '\', ']', '^', '`', '{', '}', '~',
      --  These are not in Ada

      '-', ''', '&', '(', ')', '+', ',', ':', ';', '|', '@',
      '<', '>', '.', '*', '=', '/',
      Arrow, --  =>
      Dot_Dot, --  ..
      Exp_Op, --  **
      Not_Equal, --  /=
      Greater_Or_Equal, --  >=
      Right_Label_Bracket, --  >>
      Less_Or_Equal, --  <=
      Left_Label_Bracket, --  <<
      Box, --  <>
      Colon_Equal, --  :=

      --  Ada 83 reserved words

      Res_Abort,
      Res_Abs,
      Res_Accept,
      Res_Access,
      Res_And,
      Res_All,
      Res_Array,
      Res_At,
      Res_Begin,
      Res_Body,
      Res_Case,
      Res_Constant,
      Res_Declare,
      Res_Delay,
      Res_Delta,
      Res_Digits,
      Res_Do,
      Res_Else,
      Res_Elsif,
      Res_End,
      Res_Entry,
      Res_Exception,
      Res_Exit,
      Res_For,
      Res_Function,
      Res_Generic,
      Res_Goto,
      Res_If,
      Res_In,
      Res_Is,
      Res_Limited,
      Res_Loop,
      Res_Mod,
      Res_New,
      Res_Not,
      Res_Null,
      Res_Of,
      Res_Or,
      Res_Others,
      Res_Out,
      Res_Package,
      Res_Pragma,
      Res_Private,
      Res_Procedure,
      Res_Raise,
      Res_Range,
      Res_Record,
      Res_Rem,
      Res_Renames,
      Res_Return,
      Res_Reverse,
      Res_Select,
      Res_Separate,
      Res_Subtype,
      Res_Task,
      Res_Terminate,
      Res_Then,
      Res_Type,
      Res_Use,
      Res_When,
      Res_While,
      Res_With,
      Res_Xor,

      --  Ada 95 reserved words

      Res_Abstract,
      Res_Aliased,
      Res_Protected,
      Res_Until,
      Res_Requeue,
      Res_Tagged,

      --  Ada 2005 reserved words

      Res_Interface,
      Res_Overriding,
      Res_Synchronized,

      --  Ada 2012 reserved words

      Res_Some
     );

   subtype Token_Kind is Opt_Token_Kind with Predicate => Token_Kind /= Nil;

   subtype Other_Lexeme is Token_Kind range '!' .. Colon_Equal;

   subtype Reserved_Word is Token_Kind range Res_Abort .. Res_Some;
   subtype Reserved_Word_Or_Id is Token_Kind with
     Predicate => Reserved_Word_Or_Id in Identifier | Reserved_Word;
   subtype Reserved_Word_83 is Token_Kind range Res_Abort .. Res_Xor;
   subtype Reserved_Word_95 is Token_Kind range Res_Abort .. Res_Tagged;
   subtype Reserved_Word_2005 is
     Token_Kind range Res_Abort .. Res_Synchronized;
   subtype Reserved_Word_2012 is Token_Kind range Res_Abort .. Res_Some;

   subtype Whole_Line_Comment is Token_Kind with
     Predicate => Whole_Line_Comment in
       Pp_Off_Comment | Pp_On_Comment | Special_Comment |
       Fillable_Comment | Other_Whole_Line_Comment;

   subtype Comment_Kind is Token_Kind with
        Predicate => Comment_Kind in Whole_Line_Comment | End_Of_Line_Comment;

   subtype Pp_Off_On_Comment is Token_Kind with
        Predicate => Pp_Off_On_Comment in Pp_Off_Comment | Pp_On_Comment;

   subtype Same_Text_Kind is Opt_Token_Kind range Start_Of_Input .. Res_Some;
   --  These are the tokens that always have the same text associated with
   --  them (case insensitively), so we don't need to store the text with
   --  each token. Instead, the token text is stored in Token_To_Symbol_Map
   --  below. Example: Every Less_Or_Equal token has the text "<=".

   subtype Stored_Text_Kind is Token_Kind with
     Predicate => Stored_Text_Kind not in Same_Text_Kind;
   --  These are the tokens that have different text.
   --  The token text is stored separately for each token.
   --  Example: Identifier -- one might have text = "Foo",
   --  and another might have "Bar".

   type Source_Location is record
      Line, Col : Positive; -- 1-based line and column numbers
      First     : Positive;
      Last      : Natural;

      Firstx, Lastx : Marker;
      --  ???Same information as First&Last. These should replace First&Last
      --  eventually. Note that Lastx points one past the last character.
   end record;

   --  Define type Token as a variant record. Mostly, we avoid using this type,
   --  and access tokens via type Tokn_Cursor and its accessor functions.
   --  See those accessor functions for documentation on the components
   --  of this record.

   type Opt_Token (Kind : Opt_Token_Kind := Nil) is record
      Sloc : Source_Location;
      case Kind is
         when Same_Text_Kind => null;
         when Stored_Text_Kind =>
            Text : Syms.Symbol;
            case Kind is
               when Comment_Kind =>
                  Leading_Blanks : Natural;
                  case Kind is
                     when Whole_Line_Comment =>
                        Width : Natural;
                     when others => null;
                  end case;
               when others => null;
            end case;
         when others => null;
      end case;
   end record;

   subtype Token is Opt_Token with Predicate => Token.Kind /= Nil;

   --  To add new token kinds:
   --
   --  Decide whether it belongs in Same_Text_Kind or Stored_Text_Kind,
   --  and add the enumeral to Opt_Token_Kind in an appropriate place.
   --
   --  Add code to Get_Tokns.Get_Tokn to parse out the token.
   --
   --  If the token needs additional information, add it to the variant part
   --  of type Token above. In addition, search the body of this package for
   --  "Octet" to find all the places where the additional information needs to
   --  be encoded, decoded, and skipped over. Currently, we only have support
   --  for integer types.

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

   Token_Separator : constant W_Char := W_Char'Val (1);
   --  If Insert_Line_Breaks is False, this character is used instead of hard
   --  line breaks, because otherwise things like "isbegin" can be run
   --  together. ???This doesn't work.

   ----------------

   type Tokn_Vec is limited private;
   --  Growable sequence of tokens
   type Tokn_Cursor (<>) is private;
   --  Pointer into a Tokn_Vec

   type Tokn_Seq (<>) is limited private;
   --  Fixed sequence of tokens
   type Tokn_Seq_Cursor (<>) is private;
   --  Pointer into a Tokn_Seq

   type Tokn_Index is new Positive;

   function Kind (X : Tokn_Cursor) return Token_Kind;
   --  Note that this cannot return Nil

   function Sloc (X : Tokn_Cursor) return Source_Location;

   function Text (X : Tokn_Cursor) return Syms.Symbol;
   --  The text of the token as it appears in the source, with these
   --  exceptions and clarifications:
   --
   --  Start_Of_Input and End_Of_Input have Text = "".
   --
   --  End_Of_Line and Blank_Line have Text equal to a single LF character,
   --  even if it is CR,LF in the input.
   --
   --  For comments, the text of the comment excluding the initial "--"
   --  and leading and trailing blanks, and followed by an extra NL. For
   --  multi-line comment "paragraphs", used for filling, NL terminates each
   --  line. The NL at the end isn't really part of the comment; the next
   --  token in the stream will be End_Of_Line. The reason for the extra NL
   --  is that GNATCOLL.Paragraph_Filling expects it, so it's simpler and
   --  more efficient this way.

   function Leading_Blanks (X : Tokn_Cursor) return Natural with
     Pre => Kind (X) in Comment_Kind;
   --  The number of leading blanks, which are blanks after the initial "--"
   --  and before any nonblank characters.

   function Width (X : Tokn_Cursor) return Natural with
     Pre => Kind (X) in Whole_Line_Comment;
   --  For single-line Whole_Line_Comments, this is the width of the token,
   --  i.e. the same as Sloc.Last-Sloc.First+1, and the same as the length of
   --  Text. For multi-line comments, this is the width of the widest line.
   --  The initial "--" and any leading blanks are included, but the NL's are
   --  not.

   procedure Clear (V : in out Tokn_Vec);
   function First (V : access Tokn_Vec) return Tokn_Cursor;
   function Is_Empty (V : Tokn_Vec) return Boolean;
   function At_Last (Cur : Tokn_Cursor) return Boolean;
   function After_Last (Cur : Tokn_Cursor) return Boolean;
   function Get_Tokn_Index (Cur : Tokn_Cursor) return Tokn_Index;
   function Next (Cur : Tokn_Cursor) return Tokn_Cursor;
   procedure Next (Cur : in out Tokn_Cursor);
   function Prev (Cur : Tokn_Cursor) return Tokn_Cursor;
   procedure Prev (Cur : in out Tokn_Cursor);

   function Succ (Cur : Tokn_Cursor; Count : Tokn_Index) return Tokn_Cursor;
   function Pred (Cur : Tokn_Cursor; Count : Tokn_Index) return Tokn_Cursor;
   --  Returns the cursor Count tokens after/before Cur, stopping if we
   --  reach the end/start.

   subtype Nonlexeme_Kind is Opt_Token_Kind with Predicate =>
     Nonlexeme_Kind in End_Of_Line | Blank_Line | Spaces | Comment_Kind;
   subtype Lexeme_Kind is Opt_Token_Kind with Predicate =>
     Lexeme_Kind not in Nonlexeme_Kind;

   function Next_Lexeme (Cur : Tokn_Cursor) return Tokn_Cursor;
   --  Returns the next token after Cur that is not End_Of_Line, Blank_Line,
   --  Spaces, or Comment_Kind.

   function Prev_Lexeme (Cur : Tokn_Cursor) return Tokn_Cursor;
   --  Returns the previous token before Index that is (as above)

   procedure Append_Tokn (V : in out Tokn_Vec; X : Token);
   procedure Append_Tokn (V : in out Tokn_Vec; X : Tokn_Cursor);
   --  Append X onto the end of V

   function Token_At_Cursor (X : Tokn_Cursor) return Token;

   procedure Get_Tokns
     (Input                     : in out Buffer;
      Result                    : out Tokn_Vec;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Max_Tokens                : Tokn_Index := Tokn_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null);
   --  Return in Result the sequence of tokens in the Input string. The first
   --  one is always Start_Of_Input, and the last one End_Of_Input. Max_Tokens
   --  places a limit on the number of tokens (not counting Start_Of_Input); we
   --  quit before reaching end of input if we've gotten that many.
   --
   --  If Line_Ends is non-null, we compute all the line endings in
   --  Line_Ends.all, which is a mapping from line numbers to Markers in the
   --  Input string. Each element points to a NL character in the corresponding
   --  buffer.

   function Get_Tokns
     (Input                     : in out Buffer;
      Result                    : out Tokn_Vec;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Max_Tokens                : Tokn_Index := Tokn_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null)
     return Boolean;
   --  This is to get around the annoying restriction in Ada that you can't mix
   --  declarations and statements. It does the same thing as the procedure,
   --  and the result is to be ignored.

   function Get_Tokn_Text
     (Input : W_Str; Ada_Version : Ada_Version_Type) return Syms.Symbol with
     Pre => Assert_Enabled; -- This is called only for debugging
   --  Get just one token, ignoring single line breaks, and return the Text

   ----------------

   --  See the above versions for documentation of the following.

   function Kind (X : Tokn_Seq_Cursor) return Token_Kind;
   function Text (X : Tokn_Seq_Cursor) return Syms.Symbol;
   function First (S : access Tokn_Seq) return Tokn_Seq_Cursor;
   function Is_Empty (S : Tokn_Seq) return Boolean;
   function At_Last (Cur : Tokn_Seq_Cursor) return Boolean;
   function After_Last (Cur : Tokn_Seq_Cursor) return Boolean;
   function Next (Cur : Tokn_Seq_Cursor) return Tokn_Seq_Cursor;
   procedure Next (Cur : in out Tokn_Seq_Cursor);
   function Prev (Cur : Tokn_Seq_Cursor) return Tokn_Seq_Cursor;
   procedure Prev (Cur : in out Tokn_Seq_Cursor);
   function Succ
     (Cur : Tokn_Seq_Cursor; Count : Tokn_Index) return Tokn_Seq_Cursor;
   function Pred
     (Cur : Tokn_Seq_Cursor; Count : Tokn_Index) return Tokn_Seq_Cursor;
   function Next_Lexeme (Cur : Tokn_Seq_Cursor) return Tokn_Seq_Cursor;
   function Prev_Lexeme (Cur : Tokn_Seq_Cursor) return Tokn_Seq_Cursor;
   function Leading_Blanks (X : Tokn_Seq_Cursor) return Natural with
     Pre => Kind (X) in Comment_Kind;
   function Width (X : Tokn_Seq_Cursor) return Natural with
     Pre => Kind (X) in Whole_Line_Comment;
   function Sloc (X : Tokn_Seq_Cursor) return Source_Location;

   function To_Tokn_Seq (V : Tokn_Vec) return Tokn_Seq;

   procedure Check_Same_Tokens (X, Y : Tokn_Vec) with Pre => False;
   --  Checks that X and Y are the same except for Slocs and line breaks; raise
   --  an exception if not.????Not yet used.

   procedure Put_Token (Tok : Tokn_Cursor);
   procedure Put_Tokens
     (Tokens    : Tokn_Seq;
      Highlight : Tokn_Index'Base := 0);
   procedure Put_Tokens
     (First     : Tokn_Cursor;
      Last      : Tokn_Cursor;
      Highlight : Tokn_Cursor);
   --  Put token(s) to standard output (even if Text_IO.Current_Output has been
   --  redirected). The tokens come out in compilable form, one per line, with
   --  the text of the token first, and the other information commented out.
   --  This one-token-per line code can be used for testing the scanner -- it
   --  should have identical semantics to the original Ada code. First and Last
   --  indicate a slice of Tokens, and we tolerate out-of-bounds indices.
   --  We draw a comment line before Highlight.
   procedure Put_Tokens (Tokens : Tokn_Vec);

private

   use Utils.Var_Length_Ints;

   type Fixed_Part is record
      Kind : Token_Kind;
      Sloc : Source_Location;
   end record;

   type Fixed_Part_Array is array (Tokn_Index range <>) of Fixed_Part;

   package Fixed_Part_Vectors is new Utils.Vectors
     (Tokn_Index, Fixed_Part, Fixed_Part_Array);
   subtype Fixed_Part_Vector is Fixed_Part_Vectors.Vector;

   type Tokn_Vec is limited record
      Fixed : Fixed_Part_Vector;
      Octets : Octet_Vector;
   end record;

   type Tokn_Cursor (V : access Tokn_Vec) is record
      Fi : Tokn_Index;
      Oc : Octet_Index;
   end record;

   type Tokn_Seq (F_Len : Tokn_Index'Base; O_Len : Octet_Index'Base) is
   limited record
      Fixed : Fixed_Part_Array (1 .. F_Len);
      Octets : Octet_Array (1 .. O_Len);
   end record;

   type Tokn_Seq_Cursor (S : access Tokn_Seq) is record
      Fi : Tokn_Index;
      Oc : Octet_Index;
   end record;

end Pp.Scanner;
