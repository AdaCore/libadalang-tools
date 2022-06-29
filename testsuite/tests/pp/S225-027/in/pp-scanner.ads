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

with Pp.Buffers;

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
      Ident,
      Character_Literal,
      String_Lit,
      Numeric_Literal,
      Preprocessor_Directive, -- line starting with "#"

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
      Enabled_LB_Token,
      Disabled_LB_Token,
      Tab_Token,
      False_End_Of_Line,
      True_End_Of_Line,
      --  False_End_Of_Line and True_End_Of_Line are used only in Src_Tokns;
      --  the scanner produces these when scanning the source.
      --  A False_End_Of_Line follows a comment, and is merely a placeholder
      --  for a line break that will be processed later. All others are
      --  True_End_Of_Line.
      --  Enabled_LB_Token and Disabled_LB_Token are used in New_Tokns,
      --  representing line breaks in the generated output; these contain an
      --  index into the lines break table.

      '!',
      '#',
      '$',
      '?',
      '[',
      '\',
      ']',
      '^',
      '`',
      '{',
      '}',
      '~',
      '_',
      --  These are not in Ada

      '-',
      ''',
      '&',
      '(',
      ')',
      '+',
      ',',
      ':',
      ';',
      '|',
      '@',
      '<',
      '>',
      '.',
      '*',
      '=',
      '/',
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

      Res_Abort, Res_Abs, Res_Accept, Res_Access, Res_And, Res_All, Res_Array,
      Res_At, Res_Begin, Res_Body, Res_Case, Res_Constant, Res_Declare,
      Res_Delay, Res_Delta, Res_Digits, Res_Do, Res_Else, Res_Elsif, Res_End,
      Res_Entry, Res_Exception, Res_Exit, Res_For, Res_Function, Res_Generic,
      Res_Goto, Res_If, Res_In, Res_Is, Res_Limited, Res_Loop, Res_Mod,
      Res_New, Res_Not, Res_Null, Res_Of, Res_Or, Res_Others, Res_Out,
      Res_Package, Res_Pragma, Res_Private, Res_Procedure, Res_Raise,
      Res_Range, Res_Record, Res_Rem, Res_Renames, Res_Return, Res_Reverse,
      Res_Select, Res_Separate, Res_Subtype, Res_Task, Res_Terminate, Res_Then,
      Res_Type, Res_Use, Res_When, Res_While, Res_With, Res_Xor,

      --  Ada 95 reserved words

      Res_Abstract,
      Res_Aliased, Res_Protected, Res_Until, Res_Requeue, Res_Tagged,

      --  Ada 2005 reserved words

      Res_Interface, Res_Overriding, Res_Synchronized,

      --  Ada 2012 reserved words

      Res_Some);

   subtype Token_Kind is Opt_Token_Kind with
       Predicate => Token_Kind /= Nil;

   subtype Other_Lexeme is Token_Kind range '!' .. Colon_Equal;

   subtype Reserved_Word is Token_Kind range Res_Abort .. Res_Some;
   subtype Reserved_Word_Or_Id is Token_Kind with
       Predicate => Reserved_Word_Or_Id in Ident | Reserved_Word;
   subtype Reserved_Word_83 is Token_Kind range Res_Abort .. Res_Xor;
   subtype Reserved_Word_95 is Token_Kind range Res_Abort .. Res_Tagged;
   subtype Reserved_Word_2005 is
     Token_Kind range Res_Abort .. Res_Synchronized;
   subtype Reserved_Word_2012 is Token_Kind range Res_Abort .. Res_Some;

   subtype Whole_Line_Comment is Token_Kind with
       Predicate => Whole_Line_Comment in Pp_Off_Comment | Pp_On_Comment |
            Special_Comment | Fillable_Comment | Other_Whole_Line_Comment;

   subtype Comment_Kind is Token_Kind with
       Predicate => Comment_Kind in Whole_Line_Comment | End_Of_Line_Comment;

   subtype EOL_Token is Token_Kind with
       Predicate => EOL_Token in False_End_Of_Line | True_End_Of_Line;

   subtype Line_Break_Token is Token_Kind with
       Predicate => Line_Break_Token in Enabled_LB_Token | Disabled_LB_Token;

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
   --  Example: Ident -- one might have text = "Foo",
   --  and another might have "Bar".

   type Source_Location is record
      Line, Col : Positive := 9_999; -- 1-based line and column numbers
      First     : Positive := 9_999;
      Last      : Natural  := 9_999;
   end record;

   --  Define type Token as a variant record. Mostly, we avoid using this type,
   --  and access tokens via type Tokn_Cursor and its accessor functions.
   --  See those accessor functions for documentation on the components
   --  of this record.

   type Token (Kind : Opt_Token_Kind := Nil) is private;

   function Text (X : Same_Text_Kind) return Syms.Symbol;
   function Text (X : Token) return Syms.Symbol;

   --  To add new token kinds:
   --
   --  Decide whether it belongs in Same_Text_Kind or Stored_Text_Kind,
   --  and add the enumeral to Opt_Token_Kind in an appropriate place.
   --
   --  Add code to Get_Tokns.Get_Tokn to parse out the token.
   --
   --  If the token needs additional information, add it to the variant part
   --  of type Token above. In addition, search the body of this package for
   --  "Octet" to find all places where the additional information needs to
   --  be encoded, decoded, and skipped over. Token_At_Cursor also needs to
   --  be modified. Currently, we only have support for integer types.

   function Image (Sloc : Source_Location) return String is
     (Image (Sloc.Line) & ":" & Image (Sloc.Col) & "(" & Image (Sloc.First) &
      ".." & Image (Sloc.Last) & ")");

   function Sloc_Image (Sloc : Source_Location) return String is
     (Image (Sloc.Line) & ":" & Image (Sloc.Col));

   type Source_Message is record
      --  Message attached to a particular source location.
      Sloc : Source_Location; -- the location
      Text : Char_Vectors.Char_Vector; -- the text of the message
   end record;
   type Source_Message_Array is array (Positive range <>) of Source_Message;

   package Source_Message_Vectors is new Utils.Vectors (Index_Type => Positive,
      Element_Type => Source_Message, Elements_Array => Source_Message_Array);
   subtype Source_Message_Vector is Source_Message_Vectors.Vector;

   function Message_Image
     (File_Name : String; Sloc : Source_Location) return String is
     (File_Name & ":" & Image (Sloc.Line) & ":" & Image (Sloc.Col));

   Default_Pp_Off_String : aliased constant W_Str := "--!pp off";
   Default_Pp_On_String  : aliased constant W_Str := "--!pp on";

   type Pp_Off_On_Delimiters_Rec is record
      Off : access constant W_Str := Default_Pp_Off_String'Access;
      On  : access constant W_Str := Default_Pp_On_String'Access;
   --  Text of comments for turning pretting printing off and on, including
   --  the leading '--'. For example, if the user specified --pp-off='pp-',
   --  then Off will be "--pp-". A whole-line comment of the form "--pp-"
   --  will disable pretty printing.
   --  We do not want these comments to be fillable.
   end record;

   -------------------------------------
   -- Support for -pp-off and --pp-on --
   -------------------------------------

   Pp_Off_On_Delimiters : Scanner.Pp_Off_On_Delimiters_Rec;

   ----------------

   type Tokn_Vec is private;
   type Tokn_Vec_Ref is access constant Tokn_Vec;
   --  Growable sequence of tokens
   type Tokn_Cursor is private;
   --  Pointer into a Tokn_Vec

   function Is_Nil (X : Tokn_Cursor) return Boolean;
   function Nil_Tokn_Cursor return Tokn_Cursor;

   type Tokn_Index is new Positive;

   function Kind (X : Tokn_Cursor) return Token_Kind;
   --  Note that this cannot return Nil

   function Sloc (X : Tokn_Cursor) return Source_Location;
   function Sloc_Line (X : Tokn_Cursor) return Positive;
   function Sloc_Col (X : Tokn_Cursor) return Positive;
   function Sloc_First (X : Tokn_Cursor) return Positive;
   function Sloc_Last (X : Tokn_Cursor) return Natural;
   function New_Sloc_First (V : Tokn_Vec) return Positive;
   --  Sloc_First of the new token that will be added next, but does not yet
   --  exist in the Tokn_Vec.
   function Next_Sloc_First (X : Tokn_Cursor) return Positive;
   function Tokn_Length (X : Tokn_Cursor) return Natural is
     (Next_Sloc_First (X) - Sloc_First (X));
   --  The text of the token is equal to the slice
   --     Input(Sloc_First..Sloc_Last),
   --  where Input is the parameter of Get_Tokns.
   --  except for comments
   --  and eol/blank_lines that are CR/LF
   --  Beware that it's UtF-8.

   function Text (X : Tokn_Cursor) return Syms.Symbol;
   --  The text of the token as it appears in the source, with these
   --  exceptions and clarifications:
   --
   --  Start_Of_Input and End_Of_Input have Text = "".
   --
   --  EOL_Token have Text equal to a single LF character, even if it is
   --  CR,LF in the input.
   --
   --  For comments, the text of the comment excluding the initial "--"
   --  and leading and trailing blanks, and followed by an extra NL. For
   --  multi-line comment "paragraphs", used for filling, NL terminates each
   --  line. The NL at the end isn't really part of the comment; the next
   --  token in the stream will be EOL_Token. The reason for the extra NL
   --  is that GNATCOLL.Paragraph_Filling expects it, so it's simpler and
   --  more efficient this way.

   function Leading_Blanks (X : Tokn_Cursor) return Natural with
     Pre => Kind (X) in Comment_Kind;
   --  The number of leading blanks, which are blanks after the initial "--"
   --  and before any nonblank characters.

   function Width (X : Tokn_Cursor) return Positive with
     Pre => Kind (X) in Whole_Line_Comment;
   --  For single-line Whole_Line_Comments, this is the width of the token,
   --  i.e. the same as Sloc.Last-Sloc.First+1, and the same as the length of
   --  Text. For multi-line comments, this is the width of the widest line.
   --  The initial "--" and any leading blanks are included, but the NL's are
   --  not.

   function Token_Output_Len (X : Token) return Positive with
     Pre => X.Kind in Comment_Kind;
   function Token_Output_Len (X : Tokn_Cursor) return Positive with
     Pre => Kind (X) in Comment_Kind;
   --  Returns the length of the comment as it will appear in the output. For
   --  fillable comment paragraphs, this uses Sloc_Col as the indentation. The
   --  comment is assumed to start at the initial "--" and go to the end of the
   --  text, so the indentation is included in all but the first line.

   procedure Clear (V : in out Tokn_Vec);
   function First (V : Tokn_Vec_Ref) return Tokn_Cursor;
   function Last (V : Tokn_Vec_Ref) return Tokn_Cursor;
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

   function Tokens_Require_Space (X, Y : Tokn_Cursor) return Boolean;
   --  True if a space is needed between X and Y to keep them from running
   --  together. For example, if X is the identifier "Mumble", and Y is the
   --  reserved word "is", then without a space between, it would look like a
   --  single identifier "Mumbleis". If Y cannot follow X according to the
   --  grammar, then it doesn't matter whether this returns True or False.

   function Get_Num_Tokens (V : Tokn_Vec) return Tokn_Index is
     (Get_Tokn_Index (Last (V'Unrestricted_Access)));

   function Is_Blank_Line (X : Tokn_Cursor) return Boolean is
     ((Kind (X) in EOL_Token and then Kind (Prev (X)) in EOL_Token)
      or else
      (Kind (X) in Enabled_LB_Token
       and then Kind (Prev (X)) in Enabled_LB_Token));

   subtype Nonlexeme_Kind is Opt_Token_Kind with
       Predicate => Nonlexeme_Kind in EOL_Token | Spaces | Comment_Kind;
   subtype Lexeme_Kind is Opt_Token_Kind with
       Predicate => Lexeme_Kind not in Nonlexeme_Kind;

   function Next_Lexeme (Cur : Tokn_Cursor) return Tokn_Cursor;
   --  Returns the next token after Cur that is not EOL_Token, Spaces, or
   --  Comment_Kind.

   function Prev_Lexeme (Cur : Tokn_Cursor) return Tokn_Cursor;
   --  Returns the previous token before Index that is (as above)

   procedure Append_Tokn
     (V   : in out Tokn_Vec; X : Tokn_Cursor;
      Org :        String := "Append Tokn_Cursor");
   procedure Append_Tokn
     (V : in out Tokn_Vec; X : Same_Text_Kind; Org : String := "Append Kind");
   procedure Append_Tokn
     (V   : in out Tokn_Vec; X : Stored_Text_Kind; Tx : Syms.Symbol;
      Org :        String := "Append Kind&Text");
   --  Append X onto the end of V. If an identifier matches a reserved word in
   --  the latest language version, we append a reserved word token.

   --  The Org parameters are for debugging. They are a string that is recorded
   --  with the token, and indicates the "origin" of that token. The origins
   --  can be printed by Put_Token (see Show_Origin below). For debugging, it
   --  is sometimes useful to know where a certain token came from. Not every
   --  call specifies an Org; feel free to add more-specific origins when faced
   --  with a confusing bug.

   procedure Append_Spaces
     (V   : in out Tokn_Vec; Count : Natural; Existing_OK : Boolean := False;
      Org :        String := "Append_Spaces");
   --  Append a Spaces token; Count is the number of blanks.
   --  We don't want two Spaces in a row; by default we blow
   --  up if the previously appended token was a Spaces.
   --  If Existing_OK is True then we combine the two
   --  tokens.

   procedure Append_Comment
     (V : in out Tokn_Vec; X : Tokn_Cursor; Org : String) with
     Pre => Kind (X) in Comment_Kind;
   --  Append a comment token, adjusting the length for zero indentation

   procedure Append_Comment_Text
     (V : in out Tokn_Vec; X : Tokn_Cursor; Tx : W_Str;
      Recompute_Length                       :        Boolean;
      Comments_Only, Comments_Gnat_Beginning : Boolean; Indent : Natural := 0;
      Org                                    :        String) with
     Pre => Kind (X) in Comment_Kind;
   --  Append a comment token, adjusting the length for zero indentation,
   --  and using the possibly-filled text Tx. I don't think it's
   --  zero indentation!

   procedure Delete_Last (V : in out Tokn_Vec);
   function Delete_Last (V : in out Tokn_Vec) return Token;
   --  Removes the last token (and the function returns it)

   function Token_At_Cursor (X : Tokn_Cursor) return Token;
   --  Return the token as a record

   type Language is (Ada_Lang, Template_Lang);
   --  Get_Tokns is used to parse the tokens of two different languages: Ada
   --  and the template language. They are mostly the treated the same, but
   --  this is used to indicate differences.

   type Optional_EOL_Formats is (Nil, CRLF, LF) with
     Default_Value => Nil;
   subtype EOL_Formats is Optional_EOL_Formats with
       Predicate => EOL_Formats in CRLF | LF;
   --  Conventions for ends of lines

   procedure Get_Tokns
     (Input      : in out Buffers.Buffer; Result : out Tokn_Vec;
      EOL_Format :    out EOL_Formats; Ada_Version : Ada_Version_Type;
      Max_Tokens : Tokn_Index := Tokn_Index'Last; Lang : Language := Ada_Lang);
   --  Return in Result the sequence of tokens in the Input string. The first
   --  one is always Start_Of_Input, and the last one End_Of_Input.
   --  EOL_Format is set to the convention used by the input file. Max_Tokens
   --  places a limit on the number of tokens (not counting Start_Of_Input); we
   --  quit before reaching end of input if we've gotten that many.

   function Get_Tokns
     (Input       : in out Buffers.Buffer; Result : out Tokn_Vec;
      Ada_Version :        Ada_Version_Type;
      Max_Tokens  : Tokn_Index := Tokn_Index'Last; Lang : Language := Ada_Lang)
      return Boolean;
   --  This is to get around the annoying restriction in Ada that you can't mix
   --  declarations and statements. It does the same thing as the procedure,
   --  and the result is to be ignored. ???It might make sense to return the
   --  Tokn_Vec instead of a dummy Boolean, but that might be too inefficient.

   procedure Move_Tokns (Target, Source : in out Tokn_Vec);
   function Move_Tokns (Target, Source : in out Tokn_Vec) return Boolean;
   --  Move Source to Target, leaving Source empty

   function Origin (X : Tokn_Cursor) return Syms.Symbol;

   function Same_Token (X, Y : Token) return Boolean;
   procedure Check_Same_Token
     (X, Y : Tokn_Cursor; Message, Name_1, Name_2 : String);
   procedure Check_Same_Tokens
     (X, Y : Tokn_Vec; Message, Name_1, Name_2 : String);
   --  Checks that X and Y are the same except for Slocs and line breaks; raise
   --  an exception if not.

   procedure Put_Token (Tok : Tokn_Cursor);
   procedure Put_Tokens
     (First : Tokn_Cursor; After_Last : Tokn_Cursor; Highlight : Tokn_Cursor);
   --  Put token(s) to standard output (even if Text_IO.Current_Output has been
   --  redirected). The tokens come out in compilable form, one per line, with
   --  the text of the token first, and the other information commented out.
   --  This one-token-per line code can be used for testing the scanner -- it
   --  should have identical semantics to the original Ada code. First and Last
   --  indicate a slice of Tokens, and we tolerate out-of-bounds indices.
   --  We draw a comment line before Highlight.
   procedure Put_Tokens (Highlight : Tokn_Cursor; Num_Toks : Tokn_Index := 8);
   --  Num_Toks is the number of tokens before and after Highlight to print
   procedure Put_Tokens (Tokens : Tokn_Vec);
   procedure Put_Tokns (Tok : Tokn_Cursor);
   --  This puts all the tokens in the vector that Tok points to, highlighting
   --  Tok.

   Show_Origin : Boolean := False with
     Export;
   --  Set to True in debugger to see Origins

   Check_Comment_Length : Boolean := True;
--  True if Append_Tokn should check that the length of a comment is
--  correct. It will not be correct during the Insert_Alignment phase.

private

   type Token (Kind : Opt_Token_Kind := Nil) is record
      Sloc : Source_Location;
      case Kind is
         when Line_Break_Token | Tab_Token =>
            Index : Positive;
         --  This is really of type Line_Break_Index or Tab_Index, but
         --  we don't want to depend on Pp.Formatting, where those are
         --  declared. See child package Lines for a well-typed interface.
            when Stored_Text_Kind =>
            Text : Syms.Symbol;
            case Kind is
               when Comment_Kind =>
                  Leading_Blanks : Natural;
                  case Kind is
                     when Whole_Line_Comment =>
                        Width : Positive;

                        --  The following are used to compute the Sloc of the
                        --  EOL_Token token that follows the comment.

                        Num_Lines : Positive;
                        --  Number of lines in the comment; this can be greater
                        --  than one for comments combined into a fillable
                        --  comment "paragraph".
                        Last_Line_Len : Positive;
                     --  Length of the last line of the comment
                        when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
   end record; -- Token

   subtype Opt_Token is Token;
   --  Opt_Token is used when Kind can be Nil; otherwise Token is used

   use Utils.Var_Length_Ints;

   --  Each token in a Tokn_Vec consists of a fixed-length part, and possibly a
   --  variable-length part stored in Octets. See Var_Length_Ints for the
   --  encoding.

   type Fixed_Part is record
      Kind       : Token_Kind;
      Sloc_Line  : Positive;
      Sloc_Col   : Positive;
      Sloc_First : Positive;
      Origin     : Syms.Symbol;
   end record;

   type Fixed_Part_Array is array (Tokn_Index range <>) of Fixed_Part;

   package Fixed_Part_Vectors is new Utils.Vectors (Tokn_Index, Fixed_Part,
      Fixed_Part_Array);
   subtype Fixed_Part_Vector is Fixed_Part_Vectors.Vector;

   type Tokn_Vec is record
      New_Sloc_First : Positive := 1;
      Fixed          : Fixed_Part_Vector;
      Octets         : Octet_Vector;
   end record;

   type Tokn_Cursor (V : Tokn_Vec_Ref := null) is record
      Fi : Tokn_Index  := 666_666; -- Pointer into V.Fixed
      Oc : Octet_Index := 666_666; -- Pointer into V.Octets
   end record;

   --  The following are private, for use by child package Lines:

   function Index (X : Tokn_Cursor) return Positive with
     Pre => Kind (X) in Line_Break_Token | Tab_Token;

   procedure Append_Tokn_With_Index
     (V   : in out Tokn_Vec; X : Token_Kind; Index : Positive;
      Org :        String := "Append Kind") with
     Pre => X in Line_Break_Token | Tab_Token;

end Pp.Scanner;
