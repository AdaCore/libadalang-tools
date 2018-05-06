------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . S C A N N E R                     --
--                                                                          --
--                                 B o d y                                  --
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

with Text_IO;
with Pp.Error_Slocs; use Pp.Error_Slocs;

package body Pp.Scanner.Seqs is

   use Utils.Predefined_Symbols;

   type Symbol_To_Reserved_Word_Mapping is
     array (Potential_Reserved_Word_Sym) of Reserved_Word_Or_Id;

   function Init_Symbol_To_Reserved_Word_Map
     return Symbol_To_Reserved_Word_Mapping;

   function Init_Symbol_To_Reserved_Word_Map
     return Symbol_To_Reserved_Word_Mapping
   is
   begin
      return R : Symbol_To_Reserved_Word_Mapping := (others => Identifier) do
         for Res in Reserved_Word loop
            R (Token_To_Symbol_Map (Res)) := Res;
         end loop;
      end return;
   end Init_Symbol_To_Reserved_Word_Map;

   Symbol_To_Reserved_Word_Map : constant Symbol_To_Reserved_Word_Mapping :=
     Init_Symbol_To_Reserved_Word_Map;

   function Lookup_Reserved_Word
     (Text : Symbol; Ada_Version : Ada_Version_Type)
      return Reserved_Word_Or_Id;
   --  If Text is a reserved word for the given Ada_Version, return that
   --  Reserved_Word. Otherwise, return Identifier.

   function Lookup_Reserved_Word
     (Text : Symbol; Ada_Version : Ada_Version_Type)
      return Reserved_Word_Or_Id
   is
      Normalized : constant Symbol := Same_Ignoring_Case (Text);
   begin
      if Normalized in Potential_Reserved_Word_Sym'First ..
        Last_Reserved_For_Ada_Version (Ada_Version)
      then
         return Symbol_To_Reserved_Word_Map (Normalized);
      else
         return Identifier;
      end if;
   end Lookup_Reserved_Word;

   procedure Get_Tokens
     (Input                     : in out Buffer;
      Result                    : out Token_Vector;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Ignore_Single_Line_Breaks : Boolean;
      Max_Tokens                : Token_Index       := Token_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null;
      Gen_Regions               : Token_Vector_Ptr  := null)
   is
      pragma Assert (Ada_Version = Ada_2012);
      --  We are not currently taking any command-line switch to set the
      --  Ada_Version.

      procedure Assert;
      --  Assert that Line_Ends is correct

      pragma Assert
        (if
           Line_Ends /= null
         then
           Char_At (Input, Last_Position (Input)) = NL);

      Cur_Line, Cur_Col : Positive := 1;
      Cur_First         : Positive := 1;

      Name_Buffer : Bounded_Str;
      Name_Len : Natural renames Name_Buffer.Length;

      procedure Get;
      --  Move ahead one character in the input

      procedure Get_Token (Tok : out Token; Allow_Short_Fillable : Boolean);
      --  Get one token from the input, and return it in Tok, except that
      --  Tok.Text is not set. Here, Whole_Line_Comment represents a single
      --  comment line; multiple Whole_Line_Comments that may be filled are
      --  combined into a single Whole_Line_Comment after Get_Token returns.

      procedure Append_Tok (Tok : Token);
      --  Add the relevant text to the Name_Buffer. For everything but
      --  comments, this is the entire text of the token; for comments, the
      --  initial "--" and leading blanks are ignored, and an extra NL is
      --  added at the end.

      procedure Finish_Token (Tok : in out Token);
      --  Finish constructing the Token by setting Text and checking for
      --  reserved words.

      procedure Append_To_Result (Tok : in out Token);
      --  Append the token onto Result, and set Preceding_Token and
      --  Preceding_Lexeme as appropriate. Special processing for End_Of_Line:
      --  if it follows an End_Of_Line, turn it into a Blank_Line; otherwise
      --  skip it unless Ignore_Single_Line_Breaks is False.

      procedure Scan_Decimal_Digit_Chars;
      procedure Scan_Identifier_Chars;

      procedure Scan_String_Literal (Q_Char : W_Char);
      --  Scan out a string literal, where Q_Char is the initial quote
      --  character (either '"' or '%'), taking care of doubled quote
      --  characters.

      procedure Scan_Comment
        (Tok : in out Token; Allow_Short_Fillable : Boolean);
      --  Cur (Input) is the first '-' of the start of the comment.
      --  Allow_Short_Fillable indicates that we should allow a comment to be
      --  fillable even if it is short. In the inner loop in Get_Tokens where
      --  we are collecting fillable comments into a single comment paragraph,
      --  we set Allow_Short_Fillable to False for the first comment line, and
      --  True for subsequent lines, so only the first line can be "too short".

      procedure Append_Tok (Tok : Token) is
         Token_Text_First : constant Positive :=
           (if
              Tok.Kind in Comment_Kind
            then
              Tok.Sloc.First + String'("--")'Length + Tok.Leading_Blanks
            else Tok.Sloc.First);
      begin
         Append (Name_Buffer,
                 To_UTF8 (Slice (Input, Token_Text_First, Tok.Sloc.Last)));
         if Tok.Kind in Comment_Kind then
            Append (Name_Buffer, ASCII.LF);
         end if;
      end Append_Tok;

      procedure Get is
      begin
         Cur_Col := Cur_Col + 1;

         Cur_First := Cur_First + 1;
         --  It's OK for this to be greater than Input'Last; in that case, Cur
         --  will return W_NUL, indicating end-of-input.

         Move_Forward (Input);
      end Get;

      procedure Scan_Decimal_Digit_Chars is
      begin
         while Cur (Input) in '0' .. '9' | '_' loop
            Get;
         end loop;
      end Scan_Decimal_Digit_Chars;

      procedure Scan_Identifier_Chars is
      begin
         while Is_Letter (Cur (Input))
           or else Cur (Input) = '_'
           or else Is_Digit (Cur (Input))
           or else Is_Mark (Cur (Input))
         loop
            Get;
         end loop;
      end Scan_Identifier_Chars;

      procedure Scan_String_Literal (Q_Char : W_Char) is
      begin
         loop
            Get;

            pragma Assert (Cur (Input) /= W_NUL);

            if Cur (Input) = Q_Char then
               Get;

               exit when Cur (Input) /= Q_Char;
            end if;
         end loop;
      end Scan_String_Literal;

      Preceding_Lexeme,
      Preceding_Token : Token :=
        (Kind => Nil,
         Text => Name_Empty,
         Leading_Blanks | Width => Natural'Last,
         Sloc =>
           (Line | Col | First | Last => Integer'Last, Firstx | Lastx => <>));
      --  These record the preceding token, which is necessary to properly deal
      --  with single quotes and line breaks. Preceding_Lexeme doesn't count
      --  comments and line breaks; Preceding_Token does.

      procedure Scan_Comment
        (Tok : in out Token; Allow_Short_Fillable : Boolean) is
         function Count_Blanks return Natural;
         --  Skip to next non-blank character, and return the number of blanks
         --  skipped

         procedure Skip_To_EOL;
         --  Move to end of line

         function Too_Short (Tok : Token) return Boolean;
         --  The comment is too short to be considered as the start of a filled
         --  comment block.

         function Count_Blanks return Natural is
         begin
            return Result : Natural := 0 do
               while Is_Space (Cur (Input)) loop
                  Result := Result + 1;
                  Get;
               end loop;
            end return;
         end Count_Blanks;

         procedure Skip_To_EOL is
         begin
            while not Is_Line_Terminator (Cur (Input))
              and then Cur (Input) /= Token_Separator
              and then Cur (Input) /= W_NUL
            loop
               Get;
            end loop;
         end Skip_To_EOL;

         function Too_Short (Tok : Token) return Boolean is
            Too_Short_Comment : constant Positive := 30;
         begin
            return Tok.Width < Too_Short_Comment;
         end Too_Short;

         Is_Fillable_Comment : Boolean;

      --  Start of processing for Scan_Comment

      begin
         if Preceding_Lexeme.Sloc.Line = Cur_Line then
            Tok.Kind := End_Of_Line_Comment;
         else
            Tok.Kind := Other_Whole_Line_Comment;
         end if;
         Get; -- skip '-'
         Get; -- skip '-'

         if Tok.Kind = Other_Whole_Line_Comment
           and then not
             (Is_Letter (Cur (Input))
                or else Is_Digit (Cur (Input))
                or else Is_Space (Cur (Input))
                or else Is_Line_Terminator (Cur (Input))
                or else Cur (Input) = '-')
            --  ???For now, we don't consider "-----" to be special, because
            --  otherwise various comments are messed up.
         then
            Tok.Kind := Special_Comment;
         end if;

         Is_Fillable_Comment := Tok.Kind = Other_Whole_Line_Comment
           and then Is_Space (Cur (Input));
         --  We don't fill comments unless there is at least one leading blank,
         --  because otherwise some special character like "#" could end up at
         --  the start of a line, causing it to turn into a special comment,
         --  thus messing up the Final_Check. Also, for comment lines starting
         --  with "----Blah", we fill as if "--Blah" is the first word.

         Tok.Leading_Blanks := Count_Blanks;
         --  Don't fill if too many leading blanks
         if Tok.Leading_Blanks > 2 then
            Is_Fillable_Comment := False;
         end if;
         Skip_To_EOL;
         --  Don't fill if comment ends with "--" (like a typical copyright
         --  header). Note that this includes the case of an empty comment,
         --  where the initial "--" is immediately followed by NL.
         if Lookback (Input, 2) = '-' and then Lookback (Input, 1) = '-' then
            Is_Fillable_Comment := False;
         end if;

         --  Check for --pp-off/--pp-on comments
         if Tok.Kind in Other_Whole_Line_Comment | Special_Comment then
            declare
               Comment_Text : constant W_Str :=
                 Slice (Input, Tok.Sloc.First, Cur_First - 1);
            begin
               if Has_Prefix
                 (Comment_Text, Prefix => Pp_Off_On_Delimiters.Off.all)
               then
                  Is_Fillable_Comment := False;
                  Tok.Kind := Pp_Off_Comment;
               elsif Has_Prefix
                 (Comment_Text, Prefix => Pp_Off_On_Delimiters.On.all)
               then
                  Is_Fillable_Comment := False;
                  Tok.Kind := Pp_On_Comment;
               end if;
            end;
         end if;

         Tok.Width := Cur_First - Tok.Sloc.First;

         if not Allow_Short_Fillable and then Too_Short (Tok) then
            Is_Fillable_Comment := False;
         end if;

         if Is_Fillable_Comment then
            pragma Assert (Tok.Kind = Other_Whole_Line_Comment);
            Tok.Kind := Fillable_Comment;
         end if;
      end Scan_Comment;

      procedure Get_Token (Tok : out Token; Allow_Short_Fillable : Boolean) is
      begin
         while Is_Space (Cur (Input))
           or else Cur (Input) = Token_Separator
         loop
            Get;
         end loop;

         Tok.Leading_Blanks      := 0;
         Tok.Sloc                :=
           (Line   => Cur_Line,
            Col    => Cur_Col,
            First  => Cur_First,
            Last   => <>,
            Firstx => Mark (Input, '<'),
            Lastx  => <>);

         --  end of line

         if Is_Line_Terminator (Cur (Input)) then
            if Line_Ends /= null then
               Append (Line_Ends.all, Tok.Sloc.Firstx);
            end if;

            if Cur (Input) = W_CR and then Lookahead (Input) = W_LF then
               pragma Assert (Line_Ends = null);
               Get;
            end if;

            Get;
            Tok.Kind := End_Of_Line;
            --  Might be ignored below, or turned into Blank_Line

            Cur_Line := Cur_Line + 1;
            Cur_Col  := 1;

         --  identifier

         elsif Is_Letter (Cur (Input)) then
            Tok.Kind := Identifier;
            --  We will check for reserved words below
            Scan_Identifier_Chars;

         else
            case Cur (Input) is
               when W_NUL =>
                  Tok.Kind := End_Of_Input;

               --  Minus sign or comment

               when '-' =>
                  if Lookahead (Input) = '-' then
                     Scan_Comment (Tok, Allow_Short_Fillable);
                  else
                     Get;
                     Tok.Kind := '-';
                  end if;

               --  numeric literal

               when '0' .. '9' =>
                  Tok.Kind := Numeric_Literal;
                  Scan_Decimal_Digit_Chars;

                  if Cur (Input) in '#' | ':' then
                     loop
                        Get;

                        pragma Assert (Cur (Input) /= W_NUL);
                        exit when Cur (Input) in '#' | ':';
                     end loop;

                     Get;

                  elsif Cur (Input) = '.' then
                     if Lookahead (Input) /= '.' then -- could be ".."
                        Get;
                        Scan_Decimal_Digit_Chars;
                     end if;
                  end if;

                  if To_Lower (Cur (Input)) = 'e' then
                     Get;

                     if Cur (Input) in '+' | '-' then
                        Get;
                     end if;

                     Scan_Decimal_Digit_Chars;
                  end if;

               --  single quote or character literal

               when ''' =>
                  Get;

                  --  We distinguish a single quote token from a character
                  --  literal by looking back at the preceding token. If it's
                  --  not one of the following, then it's a single quote. For
                  --  example, in Character'('''), the first quote follows
                  --  an identifier, so it's a single quote. The second one
                  --  follows a left parenthesis, so it's the start of a
                  --  character literal. The String_Literal case is really
                  --  for operator symbols, as in "+"'Address.

                  if Preceding_Lexeme.Kind in Identifier | String_Literal |
                      Res_Access | Res_All | ')'
                  then
                     Tok.Kind := '''; -- it's a tick
                  else
                     Tok.Kind := Character_Literal;
                     Get;
                     pragma Assert (Cur (Input) = ''');
                     Get;
                  end if;

               --  string literal

               when '"' | '%' =>
                  Tok.Kind := String_Literal;
                  Scan_String_Literal (Q_Char => Cur (Input));

               --  One-character tokens

               when '&' =>
                  Tok.Kind := '&';
                  Get;
               when '(' =>
                  Tok.Kind := '(';
                  Get;
               when ')' =>
                  Tok.Kind := ')';
                  Get;
               when '+' =>
                  Tok.Kind := '+';
                  Get;
               when ',' =>
                  Tok.Kind := ',';
                  Get;
               when ';' =>
                  Tok.Kind := ';';
                  Get;
               when '|' =>
                  Tok.Kind := '|';
                  Get;
               when '!' =>
                  Tok.Kind := '!';
                  Get;
               when '@' =>
                  --  '@' is for target_name (see RM-5.2.1, AI12-0125-3)
                  Tok.Kind := '@';
                  Get;

               --  Multiple-character tokens. We need to distinguish between
               --  "=" and "=>", and between "." and ".." and so forth.

               when '=' =>
                  Get;

                  if Cur (Input) = '>' then
                     Tok.Kind := Arrow;
                     Get;
                  else
                     Tok.Kind := '>';
                  end if;

               when '.' =>
                  Get;

                  if Cur (Input) = '.' then
                     Tok.Kind := Dot_Dot;
                     Get;
                  else
                     Tok.Kind := '.';
                  end if;

               when '*' =>
                  Get;

                  if Cur (Input) = '*' then
                     Tok.Kind := Exp_Op;
                     Get;
                  else
                     Tok.Kind := '*';
                  end if;

               when '/' =>
                  Get;

                  if Cur (Input) = '=' then
                     Tok.Kind := Not_Equal;
                     Get;
                  else
                     Tok.Kind := '=';
                  end if;

               when '>' =>
                  Get;

                  if Cur (Input) = '=' then
                     Tok.Kind := Greater_Or_Equal;
                     Get;
                  elsif Cur (Input) = '>' then
                     Tok.Kind := Right_Label_Bracket;
                     Get;
                  else
                     Tok.Kind := '>';
                  end if;

               when '<' =>
                  Get;

                  if Cur (Input) = '=' then
                     Tok.Kind := Less_Or_Equal;
                     Get;
                  elsif Cur (Input) = '<' then
                     Tok.Kind := Left_Label_Bracket;
                     Get;
                  elsif Cur (Input) = '>' then
                     Tok.Kind := Box;
                     Get;
                  else
                     Tok.Kind := '>';
                  end if;

               when ':' =>
                  Get;

                  if Cur (Input) = '=' then
                     Tok.Kind := Colon_Equal;
                     Get;
                  else
                     Tok.Kind := '=';
                  end if;

               when others =>
                  raise Program_Error
                    with "unrecognized character: " &
                    To_UTF8 ((1 => Cur (Input)));
                  --  All legal token-starting characters are handled above
            end case;
         end if;

         Tok.Sloc.Lastx := Mark (Input, '>');
         Tok.Sloc.Last  := Cur_First - 1;
         pragma Assert (if Tok.Kind in Whole_Line_Comment then
           Tok.Width = Tok.Sloc.Last - Tok.Sloc.First + 1);

         pragma Assert (Tok.Kind /= Nil);
      end Get_Token;

      procedure Finish_Token (Tok : in out Token) is
      begin
         pragma Assert (Name_Len = 0);
         Append_Tok (Tok);
         Tok.Text := Intern (Name_Buffer);
         case Tok.Kind is
            when Comment_Kind =>
               pragma Assert (False);

            when Identifier =>
               --  Check for reserved word. In T'Range, we consider "Range" to
               --  be an identifier, not a reserved word, and similarly for
               --  others.

               if Preceding_Lexeme.Kind /= ''' then
                  Tok.Kind := Lookup_Reserved_Word (Tok.Text, Ada_Version);
               end if;

            when others =>
               null;
         end case;
      end Finish_Token;

      Ignore_Tokens : Boolean := False;
      --  True if Append_To_Result should ignore tokens. Set True between
      --  Gen_Plus and Gen_Minus.

      procedure Append_To_Result (Tok : in out Token) is
      begin
         if Tok.Kind = Other_Whole_Line_Comment then
            declare
               Comment_Text : constant W_Str :=
                 Slice (Input, Tok.Sloc.First, Cur_First - 1);
            begin
               if Has_Prefix (Comment_Text, Prefix => To_W_Str (Gen_Plus)) then
                  if Gen_Regions /= null then
                     Append (Gen_Regions.all, Tok);
                  end if;
                  Ignore_Tokens := True;
                  goto Ignore_It;
               elsif Has_Prefix (Comment_Text, Prefix => To_W_Str (Gen_Minus))
               then
                  if Gen_Regions /= null then
                     Append (Gen_Regions.all, Tok);
                  end if;
                  Ignore_Tokens := False;
                  goto Ignore_It;
               end if;
            end;
         end if;
         if Ignore_Tokens then
            goto Ignore_It;
         end if;

         --  Can't assert for comments because block comments
         pragma Assert
           (if
              Tok.Kind not in Comment_Kind
            then
              To_W_Str (Tok.Text) =
                Slice (Input, Tok.Sloc.Firstx, Tok.Sloc.Lastx));

         if Tok.Kind = End_Of_Line then
            if Preceding_Token.Kind in End_Of_Line | Blank_Line then
               Tok.Kind := Blank_Line;
            elsif Ignore_Single_Line_Breaks then
               goto Ignore_It;
            end if;
         end if;

         Append (Result, Tok);
         <<Ignore_It>>

         Preceding_Token := Tok; -- even if an ignored End_Of_Line
         if Tok.Kind not in Blank_Line | End_Of_Line | Comment_Kind then
            Preceding_Lexeme := Tok;
         end if;
      end Append_To_Result;

      Start_Mark_F  : constant Marker := Mark (Input, '!');
      Start_Mark_L  : constant Marker := Mark (Input, '>');
      Start_Token : constant Token  :=
        (Start_Of_Input,
         Text => Name_Empty,
         Leading_Blanks => 0,
         Width          => 0,
         Sloc =>
           (Line  => Cur_Line,
            Col   => Cur_Col,
            First => Cur_First,
            Last  => 0,
            Firstx => Start_Mark_F,
            Lastx => Start_Mark_L));

      procedure Assert is
      begin
         if Line_Ends /= null then
            for E in 1 .. Last_Index (Line_Ends.all) loop
               pragma Assert
                 (Char_At (Input, Position (Input, Line_Ends.all (E))) = NL);
            end loop;
         end if;
      end Assert;

   --  Start of processing for Get_Tokens

   begin
      pragma Assert (Point (Input) = 1);
      Clear (Result);
      Append (Result, Start_Token);

      if Line_Ends /= null then
         Clear (Line_Ends.all);
      end if;

      if Gen_Regions /= null then
         Clear (Gen_Regions.all);
      end if;

      Outer_Loop : loop
         if Result.Last_Index - 1 = Max_Tokens then
            --  "- 1" because Start_Of_Input doesn't count
            while not At_End (Input) loop
               Move_Forward (Input);
            end loop;
            exit Outer_Loop;
         end if;

         Name_Len := 0;

         --  The purpose of the following rather complicated code is to combine
         --  multiple Whole_Line_Comments that may be filled into a single
         --  Whole_Line_Comment "paragraph". Unfillable comments never combine
         --  into a paragraph. All comments in a paragraph must start at
         --  the same column in the input, and must contain the same number
         --  of leading blanks. The first comment of a paragraph cannot be
         --  too short; subsequent lines may be. End_Of_Line_Comments never
         --  combine.

         declare
            Tok, Tok_EOL, Tok_2 : Token;
         --  Tok is a token. If it is a Fillable_Comment, it might be the
         --  first of a paragraph, and Tok_EOL is the End_Of_Line token that
         --  follows the comment, and Tok_2 is the token after that, which
         --  might need to be combined with Tok.
         begin
            Get_Token (Tok, Allow_Short_Fillable => False);
            Error_Sloc := To_Langkit (Sloc (Tok));
            if Tok.Kind in Comment_Kind then
               Append_Tok (Tok);
               if Tok.Kind /= Fillable_Comment then
                  Tok.Text := Intern (Name_Buffer);
                  Append_To_Result (Tok);
               else
                  --  Loop, repeatedly getting Tok_2, and combining Tok_2 into
                  --  Tok until something ends the paragraph.

                  loop
                     Get_Token (Tok_EOL, Allow_Short_Fillable => False);
                     pragma Assert (Tok_EOL.Kind = End_Of_Line);
                     Get_Token (Tok_2, Allow_Short_Fillable => True);

                     if Tok_2.Kind in Whole_Line_Comment then

                        --  Unfillable comment breaks the paragraph. Finish
                        --  constructing Tok, send it to Result, followed by
                        --  Tok_2.

                        if Tok_2.Kind /= Fillable_Comment then
                           Tok.Text := Intern (Name_Buffer);
                           Append_To_Result (Tok);
                           Name_Len := 0;
                           Finish_Token (Tok_EOL);
                           Append_To_Result (Tok_EOL);
                           Name_Len := 0;
                           Append_Tok (Tok_2);
                           Tok_2.Text := Intern (Name_Buffer);
                           Append_To_Result (Tok_2);
                           exit;

                        --  Tok_2 can be combined

                        elsif Tok.Sloc.Col = Tok_2.Sloc.Col
                          and then Tok.Leading_Blanks = Tok_2.Leading_Blanks
                        then
                           Append_Tok (Tok_2);
                           Tok.Width := Natural'Max (Tok.Width, Tok_2.Width);
                           Tok.Sloc.Last := Tok_2.Sloc.Last;
                           Tok.Sloc.Lastx := Tok_2.Sloc.Lastx;
                        --  Go around the loop again, in case the next Tok_2
                        --  can be combined.

                        --  Tok_2 cannot be combined, but it might start a
                        --  new paragraph. Finish constructing Tok, send it
                        --  to Result, and use Tok_2 as the new Tok.

                        else
                           Tok.Text := Intern (Name_Buffer);
                           Append_To_Result (Tok);
                           Name_Len := 0;
                           Finish_Token (Tok_EOL);
                           Append_To_Result (Tok_EOL);
                           Name_Len := 0;
                           Append_Tok (Tok_2);
                           Tok := Tok_2;
                           --  Go around the loop again, with the new initial
                           --  line of the paragraph in Tok.
                        end if;

                     --  Not a comment, so cannot be combined. Finish
                     --  constructing Tok, send it to Result, likewise
                     --  for Tok_2, then exit.

                     else
                        pragma Assert (Tok_2.Kind not in Comment_Kind);
                        Tok.Text := Intern (Name_Buffer);
                        Append_To_Result (Tok);
                        Name_Len := 0;
                        Finish_Token (Tok_EOL);
                        Append_To_Result (Tok_EOL);
                        Name_Len := 0;
                        Finish_Token (Tok_2);
                        Append_To_Result (Tok_2);
                        if Tok_2.Kind = End_Of_Input then
                           exit Outer_Loop;
                        else
                           exit;
                        end if;
                     end if;
                  end loop;
               end if;

            --  The simple case: Tok is not a comment, so just finish
            --  constructing it and send it to Result.

            else
               pragma Assert (Tok.Kind not in Comment_Kind);
               Finish_Token (Tok);
               Append_To_Result (Tok);
            end if;

            exit Outer_Loop when Tok.Kind = End_Of_Input;
         end;
      end loop Outer_Loop;

      pragma Debug (Assert);
      Reset (Input);
   end Get_Tokens;

   function Next_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token
   is
      X : Token_Index := Index + 1;

   begin
      while Tokens (X).Kind in Blank_Line | Comment_Kind loop
         X := X + 1;
      end loop;

      return Tokens (X);
   end Next_Lexeme;

   function Prev_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token
   is
      X : Token_Index := Index - 1;

   begin
      while Tokens (X).Kind in Blank_Line | Comment_Kind loop
         X := X - 1;
      end loop;

      return Tokens (X);
   end Prev_Lexeme;

   function Get_Token
     (Input : W_Str; Ada_Version : Ada_Version_Type)
     return Token is
      pragma Assert (Assert_Enabled); -- This is called only for debugging
      Tokens : Token_Vector;
      Buf    : Buffer := String_To_Buffer (Input);
   begin
      Get_Tokens
        (Buf, Tokens, Ada_Version,
         Pp_Off_On_Delimiters => (others => <>),
         Ignore_Single_Line_Breaks => True,
         Max_Tokens                => 1);
      pragma Assert (Tokens (1).Kind = Start_Of_Input);
      pragma Assert (Last_Index (Tokens) = 2);
      return Tokens (2);
   end Get_Token;

   function In_Gen_Regions
     (Line : Positive; Gen_Regions : Token_Vector) return Boolean
   is
   begin
      --  Assert that they're are in increasing order

      if Assert_Enabled then
         for X in 2 .. Last_Index (Gen_Regions) loop
            declare
               R1 : constant Token := Gen_Regions (X - 1);
               R1_Line : constant Positive := R1.Sloc.Line;
               R2 : constant Token := Gen_Regions (X);
               R2_Line : constant Positive := R2.Sloc.Line;
            begin
               pragma Assert (R1_Line < R2_Line);
            end;
         end loop;
      end if;

      --  We could do a binary search here, but there probably won't be very
      --  many regions, so we use a linear search.

      for X in 1 .. Last_Index (Gen_Regions) loop
         declare
            R : constant Token := Gen_Regions (X);
            R_Line : constant Positive := R.Sloc.Line;
         begin
            pragma Assert (Line /= R_Line);
            --  The comment is on a line by itself, so it can't be on the same
            --  line as the Tree, so we don't need to worry about column
            --  numbers.

            if Line < R_Line then
               return X mod 2 = 0;
            end if;
         end;
      end loop;
      return False;
   end In_Gen_Regions;

   procedure Put_Token (Tok : Token; Index : Token_Index := 1) is
   begin
      if Tok.Kind in Comment_Kind then
         Text_IO.Put
           (Text_IO.Standard_Output,
            "--" & (1 .. Tok.Leading_Blanks => ' '));
      end if;
      for C of Str (Tok.Text).S loop
         if Tok.Kind in Comment_Kind and then C = ASCII.LF then
            Text_IO.Put (Text_IO.Standard_Output, "$");
         else
            Text_IO.Put (Text_IO.Standard_Output, C);
         end if;
      end loop;
      Text_IO.Put_Line
        (Text_IO.Standard_Output,
         " -- #" &
         Image (Integer (Index)) &
         "  " &
         Capitalize (Tok.Kind'Img) &
         " at " &
         Image (Tok.Sloc) &
         " width = " &
         Image (Tok.Width));
   end Put_Token;

   procedure Put_Tokens
     (Tokens    : Token_Array;
      Highlight : Token_Index'Base := 0)
   is
   begin
      for Index in Tokens'Range loop
         if Index = Highlight then
            Text_IO.Put_Line (Text_IO.Standard_Output, "----------------");
         end if;

         Put_Token (Tokens (Index), Index);
      end loop;
   end Put_Tokens;

   procedure Put_Tokens
     (Tokens    : Token_Vector;
      First     : Token_Index'Base := 1;
      Last      : Token_Index'Base := Token_Index'Last;
      Highlight : Token_Index'Base := 0)
   is
      A : Scanner.Token_Array renames
        Elems (Tokens)
         (Token_Index'Max (First, 1) ..
          Token_Index'Min (Last, Last_Index (Tokens)));
   begin
      Put_Tokens (A, Highlight);
   end Put_Tokens;

   procedure Dump_Token (Tok : Token) is
   begin
      Put_Token (Tok);
   end Dump_Token;

   procedure Dump_Tokens (Tokens : Token_Array) is
   begin
      Put_Tokens (Tokens);
   end Dump_Tokens;

   procedure Check_Same_Tokens (X, Y : Token_Vector) is
      Xj, Yj : Token_Index := 1;

   begin
      loop
         declare
            X_Tok  : Token            := X (Xj);
            Y_Tok  : constant Token   := Y (Yj);
            X_Done : constant Boolean := X_Tok.Kind = End_Of_Input;
            Y_Done : constant Boolean := Y_Tok.Kind = End_Of_Input;

         begin
            if Xj = 1 then
               pragma Assert (X_Tok.Kind = Start_Of_Input);
               pragma Assert (Y_Tok.Kind = Start_Of_Input);
            end if;

            if X_Tok.Kind in End_Of_Line | Blank_Line then
               Xj := Xj + 1;
               goto Continue;
            end if;
            if Y_Tok.Kind in End_Of_Line | Blank_Line then
               Yj := Yj + 1;
               goto Continue;
            end if;

            X_Tok.Sloc := Y_Tok.Sloc;
            --  Ignore Sloc in comparison below
            pragma Assert (X_Tok = Y_Tok);

            pragma Assert (X_Done = (Xj = Last_Index (X)));
            pragma Assert (Y_Done = (Yj = Last_Index (Y)));
            pragma Assert (X_Done = Y_Done);
            exit when X_Done;

            Xj := Xj + 1;
            Yj := Yj + 1;
            <<Continue>>

         end;
      end loop;
   end Check_Same_Tokens;

end Pp.Scanner.Seqs;
