-- C35503C.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--     CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULTS WHEN
--     THE PREFIX IS AN INTEGER TYPE.
--     SUBTESTS ARE :
--         PART (A). TESTS FOR 'IMAGE'.
--         PART (B). TESTS FOR 'VALUE'.

-- HISTORY:
--     RJW  03/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER, ADDED A CHECK THAT
--                    CONSTRAINT_ERROR IS RAISED FOR THE ATTRIBUTE
--                    'VALUE' IF THE FINAL SHARP OR COLON IS MISSING
--                    FROM A BASED LITERAL.

with Report; use Report;
procedure C35503c is
   type Newint is new Integer;
   type Int is range -1_000 .. 1_000;

   function Ident (X : Int) return Int is
   begin
      if Equal (Int'Pos (X), Int'Pos (X)) then
         return X;
      end if;
      return Int'First;
   end Ident;

begin
   Test
     ("C35503C",
      "THE ATTIBUTES 'IMAGE' AND 'VALUE' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS AN " & "INTEGER TYPE");
-- PART (A).

   begin
      if Integer'Image (-500) /= "-500" then
         Failed ("INCORRECT 'IMAGE' OF '-500'");
      end if;
      if Integer'Image (-500)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-500'");
      end if;

      if Newint'Image (2**6) /= " 64" then
         Failed ("INCORRECT 'IMAGE' OF '2 ** 6'");
      end if;
      if Newint'Image (2**6)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '2 ** 6'");
      end if;

      if Natural'Image (-1E2) /= "-100" then
         Failed ("INCORRECT 'IMAGE' OF '-1E2'");
      end if;
      if Natural'Image (-1E2)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-1E2'");
      end if;

      if Newint'Image (3_45) /= " 345" then
         Failed ("INCORRECT 'IMAGE' OF '3_45'");
      end if;
      if Newint'Image (3_45)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '3_45'");
      end if;

      if Integer'Image (-2#1111_1111#) /= "-255" then
         Failed ("INCORRECT 'IMAGE' OF '-2#1111_1111#'");
      end if;
      if Integer'Image (-2#1111_1111#)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-2#1111_1111#'");
      end if;

      if Newint'Image (16#FF#) /= " 255" then
         Failed ("INCORRECT 'IMAGE' OF '16#FF#'");
      end if;
      if Newint'Image (16#FF#)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '16#FF#'");
      end if;

      if Integer'Image (-016#0FF#) /= "-255" then
         Failed ("INCORRECT 'IMAGE' OF '-016#0FF#'");
      end if;
      if Integer'Image (-016#0FF#)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-016#0FF#'");
      end if;

      if Newint'Image (2#1110_0000#) /= " 224" then
         Failed ("INCORRECT 'IMAGE' OF '2#1110_0000#'");
      end if;
      if Newint'Image (2#1110_0000#)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '2#1110_0000#'");
      end if;

      if Positive'Image (-16#E#E1) /= "-224" then
         Failed ("INCORRECT 'IMAGE' OF '-16#E#E1'");
      end if;
      if Positive'Image (-16#E#E1)'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-16#E#E1'");
      end if;

      if Int'Image (Ident (-1_000)) /= "-1000" then
         Failed ("INCORRECT 'IMAGE' OF '-1000'");
      end if;
      if Int'Image (Ident (-1_000))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-1000'");
      end if;

      if Int'Image (Ident (-999)) /= "-999" then
         Failed ("INCORRECT 'IMAGE' OF '-999'");
      end if;
      if Int'Image (Ident (-999))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-999'");
      end if;

      if Int'Image (Ident (-10)) /= "-10" then
         Failed ("INCORRECT 'IMAGE' OF '-1000'");
      end if;
      if Int'Image (Ident (-10))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-10'");
      end if;

      if Int'Image (Ident (-9)) /= "-9" then
         Failed ("INCORRECT 'IMAGE' OF '-9'");
      end if;
      if Int'Image (Ident (-9))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-9'");
      end if;

      if Int'Image (Ident (-1)) /= "-1" then
         Failed ("INCORRECT 'IMAGE' OF '-1'");
      end if;
      if Int'Image (Ident (-1))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '-1'");
      end if;

      if Int'Image (Ident (0)) /= " 0" then
         Failed ("INCORRECT 'IMAGE' OF '0'");
      end if;
      if Int'Image (Ident (0))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '0'");
      end if;

      if Int'Image (Ident (1)) /= " 1" then
         Failed ("INCORRECT 'IMAGE' OF '1'");
      end if;
      if Int'Image (Ident (1))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '1'");
      end if;

      if Int'Image (Ident (9)) /= " 9" then
         Failed ("INCORRECT 'IMAGE' OF '9'");
      end if;
      if Int'Image (Ident (9))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '9'");
      end if;

      if Int'Image (Ident (10)) /= " 10" then
         Failed ("INCORRECT 'IMAGE' OF '10'");
      end if;
      if Int'Image (Ident (10))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '10'");
      end if;

      if Int'Image (Ident (999)) /= " 999" then
         Failed ("INCORRECT 'IMAGE' OF '999'");
      end if;
      if Int'Image (Ident (999))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '999'");
      end if;

      if Int'Image (Ident (1_000)) /= " 1000" then
         Failed ("INCORRECT 'IMAGE' OF '1000'");
      end if;
      if Int'Image (Ident (1_000))'First /= 1 then
         Failed ("INCORRECT LOWER BOUND FOR '1000'");
      end if;

   end;

-----------------------------------------------------------------------

-- PART (B).

   begin
      if Positive'Value (Ident_Str ("-500")) /= -500 then
         Failed ("INCORRECT 'VALUE' OF ""-500""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF ""-500""");
   end;

   begin
      if Newint'Value (" -001E2") /= -100 then
         Failed ("INCORRECT 'VALUE' OF "" -001E2""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF "" -001E2""");
   end;

   begin
      if Integer'Value ("03_45") /= 345 then
         Failed ("INCORRECT 'VALUE' OF ""03_45""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF ""03_45""");
   end;

   begin
      if Newint'Value ("-2#1111_1111#") /= -255 then
         Failed ("INCORRECT 'VALUE' OF ""-2#1111_1111#""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF " & """-2#1111_1111#""");
   end;

   begin
      if Integer'Value (Ident_Str ("16#FF#")) /= 255 then
         Failed ("INCORRECT 'VALUE' OF ""16#FF#""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF ""16#FF#""");
   end;

   begin
      if Natural'Value (Ident_Str ("-016#0FF#")) /= -255 then
         Failed ("INCORRECT 'VALUE' OF ""-016#0FF#""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF " & """-016#0FF#""");
   end;

   begin
      if Integer'Value ("2#1110_0000#     ") /= 224 then
         Failed ("INCORRECT 'VALUE' OF " & """2#1110_0000#     """);
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF " & """2#1110_0000#     """);
   end;

   begin
      if Newint'Value ("  -16#E#E1") /= -224 then
         Failed ("INCORRECT 'VALUE' OF ""  -16#E#E1""");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - 'VALUE' OF " & """  -16#E#E1""");
   end;

   begin
      if Integer'Value ("5/0") = 0 then
         Failed ("NO EXCEPTION RAISED - ""5/0"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""5/0"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""5/0""");
   end;

   declare
      subtype Subint is Integer range 0 .. 10;
   begin
      if Subint'Value (Ident_Str ("-500")) /= -500 then
         Failed ("INCORRECT VALUE WITH ""-500"" AND SUBINT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - SUBINT");
   end;

   begin
      if Integer'Value (Ident_Str ("1.0")) = 1 then
         Failed ("NO EXCEPTION RAISED - "" 1.0"" - 1");
      else
         Failed ("NO EXCEPTION RAISED - ""1.0"" - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ""1.0"" ");
   end;

   begin
      if Integer'Value (Ident_Char (Ascii.Ht) & "244") /= 244 then
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - LEADING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LEADING 'HT'");
   end;

   begin
      if Integer'Value ("244" & (Ident_Char (Ascii.Ht))) /= 244 then
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 1");
      else
         Failed ("NO EXCEPTION RAISED - TRAILING 'HT' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - TRAILING 'HT'");
   end;

   begin
      if Integer'Value (Ident_Str ("2__44")) /= 244 then
         Failed ("NO EXCEPTION RAISED - CONSECUTIVE '_' - 1");
      else
         Failed ("NO EXCEPTION RAISED - CONSECUTIVE '_' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "WITH CONSECUTIVE '_'");
   end;

   begin
      if Integer'Value (Ident_Str ("_244")) /= 244 then
         Failed ("NO EXCEPTION RAISED - LEADING '_' - 1");
      else
         Failed ("NO EXCEPTION RAISED - LEADING '_' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - LEADING '_'");
   end;

   begin
      if Integer'Value (Ident_Str ("244_")) /= 244 then
         Failed ("NO EXCEPTION RAISED - TRAILING '_' - 1");
      else
         Failed ("NO EXCEPTION RAISED - TRAILING '_' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - TRAILING '_'");
   end;

   begin
      if Integer'Value (Ident_Str ("244_E1")) /= 2_440 then
         Failed ("NO EXCEPTION RAISED - '_' BEFORE 'E' - 1");
      else
         Failed ("NO EXCEPTION RAISED - '_' BEFORE 'E' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - '_' BEFORE 'E'");
   end;

   begin
      if Integer'Value (Ident_Str ("244E_1")) /= 2_440 then
         Failed ("NO EXCEPTION RAISED - '_' " & "FOLLOWING 'E' - 1");
      else
         Failed ("NO EXCEPTION RAISED - '_' FOLLOWING 'E' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "- '_' FOLLOWING 'E'");
   end;

   begin
      if Integer'Value (Ident_Str ("244_e1")) /= 2_440 then
         Failed ("NO EXCEPTION RAISED - '_' BEFORE 'e' - 1");
      else
         Failed ("NO EXCEPTION RAISED - '_' BEFORE 'e' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - '_' BEFORE 'e'");
   end;

   begin
      if Integer'Value (Ident_Str ("16#_FF#")) /= 255 then
         Failed
           ("NO EXCEPTION RAISED - LEADING '_' IN BASED " & "LITERAL - 1");
      else
         Failed
           ("NO EXCEPTION RAISED - LEADING '_' IN BASED " & "LITERAL - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "- LEADING '_' IN BASED LITERAL");
   end;

   begin
      if Integer'Value (Ident_Str ("1E-0")) /= 1 then
         Failed ("NO EXCEPTION RAISED - NEGATIVE " & "EXPONENT - 1");
      else
         Failed ("NO EXCEPTION RAISED - NEGATIVE EXPONENT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "- NEGATIVE EXPONENT");
   end;

   begin
      if Integer'Value (Ident_Str ("244.")) /= 244 then
         Failed ("NO EXCEPTION RAISED - TRAILING '.' - 1");
      else
         Failed ("NO EXCEPTION RAISED - TRAILING '.' - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - TRAILING '.'");
   end;

   begin
      if Integer'Value (Ident_Str ("8#811#")) /= 0 then
         Failed ("NO EXCEPTION RAISED - " & "DIGITS NOT IN CORRECT RANGE - 1");
      else
         Failed ("NO EXCEPTION RAISED - " & "DIGITS NOT IN CORRECT RANGE - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - " & "DIGITS NOT IN CORRECT RANGE");
   end;

   begin
      if Integer'Value (Ident_Str ("1#000#")) /= 0 then
         Failed ("NO EXCEPTION RAISED - BASE LESS THAN 2 - 1");
      else
         Failed ("NO EXCEPTION RAISED - BASE LESS THAN 2 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "- BASE LESS THAN 2");
   end;

   begin
      if Integer'Value (Ident_Str ("17#0#")) /= 0 then
         Failed ("NO EXCEPTION RAISED " & "- BASE GREATER THAN 16 - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "- BASE GREATER THAN 16 - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "- BASE GREATER THAN 16");
   end;

   begin
      if Integer'Value (Ident_Str ("8#666")) /= 438 then
         Failed ("NO EXCEPTION RAISED - MISSING FINAL SHARP - 1");
      else
         Failed ("NO EXCEPTION RAISED - MISSING FINAL SHARP - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - MISSING FINAL SHARP");
   end;

   begin
      if Integer'Value (Ident_Str ("16:FF")) /= 255 then
         Failed ("NO EXCEPTION RAISED - MISSING FINAL COLON - 1");
      else
         Failed ("NO EXCEPTION RAISED - MISSING FINAL COLON - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - MISSING FINAL COLON");
   end;

   Result;
end C35503c;
