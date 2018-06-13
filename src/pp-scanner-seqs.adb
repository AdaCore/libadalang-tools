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

package body Pp.Scanner.Seqs is

   procedure Get_Tokens
     (Input                     : in out Buffer;
      Result                    : out Token_Vector;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Max_Tokens                : Token_Index       := Token_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null;
      Gen_Regions               : Token_Vector_Ptr  := null)
   is
      pragma Unreferenced (Gen_Regions); -- ????
      Tokns : aliased Tokn_Vec;
      Cur : Tokn_Cursor := First (Tokns'Access);
   begin
      Clear (Result);
      Get_Tokns
        (Input, Tokns, Ada_Version, Pp_Off_On_Delimiters,
         Tokn_Index (Max_Tokens), Line_Ends);

      while not After_Last (Cur) loop
         declare
            Tok : Token := (Kind (Cur), Sloc => Sloc (Cur), others => <>);
         begin
            if Kind (Cur) in Stored_Text_Kind then
               Tok.Text := Text (Cur);

               if Kind (Cur) in Comment_Kind then
                  Tok.Leading_Blanks := Leading_Blanks (Cur);

                  if Kind (Cur) in Whole_Line_Comment then
                     Tok.Width := Width (Cur);
                  end if;
               end if;

            elsif Assert_Enabled then
               Tok.Text := Intern ("invalid token text");
            end if;

            Append (Result, Tok);
         end;

         Next (Cur);
      end loop;
   end Get_Tokens;

   function Next_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token
   is
      X : Token_Index := Index + 1;

   begin
      while Tokens (X).Kind in End_Of_Line | Blank_Line | Comment_Kind loop
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
      while Tokens (X).Kind in End_Of_Line | Blank_Line | Comment_Kind loop
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
      for C of Str (Text (Tok)).S loop
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
