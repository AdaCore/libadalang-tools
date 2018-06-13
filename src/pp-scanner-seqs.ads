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

with Utils.Vectors;

package Pp.Scanner.Seqs is

   --  This package exists only because Ada won't let us instantiate Vectors in
   --  the parent package, given that Token is a private type.

   package Token_Vectors is new Utils.Vectors
     (Token_Index,
      Opt_Token,
      Token_Array);
   subtype Token_Vector is Token_Vectors.Vector;
   type Token_Vector_Ptr is access all Token_Vector;
   use Token_Vectors;
   --  use all type Token_Vector;

   procedure Get_Tokens
     (Input                     : in out Buffer;
      Result                    : out Token_Vector;
      Ada_Version               : Ada_Version_Type;
      Pp_Off_On_Delimiters      : Pp_Off_On_Delimiters_Rec;
      Max_Tokens                : Token_Index       := Token_Index'Last;
      Line_Ends                 : Marker_Vector_Ptr := null;
      Gen_Regions               : Token_Vector_Ptr  := null);
   --  Same as Scanner.Get_Tokens, except for the result type.
   --  ????

   function Next_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token;
   --  Returns the next token after Index that is not End_Of_Line, Blank_Line,
   --  or Comment_Kind.

   function Prev_Lexeme
     (Tokens : Token_Vector;
      Index  : Token_Index)
      return   Token;
   --  Returns the previous token before Index that is (as above)

   function Get_Token
     (Input : W_Str; Ada_Version : Ada_Version_Type)
     return Token;
   --  Get just one token, ignoring single line breaks

   procedure Check_Same_Tokens (X, Y : Token_Vector) with Pre => False;
   --  Checks that X and Y are the same except for Slocs and line breaks; raise
   --  an exception if not.????Not used.

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

end Pp.Scanner.Seqs;
