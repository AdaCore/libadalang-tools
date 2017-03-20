-- C35503E.ADA

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
--     THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL
--     PARAMETER IS AN INTEGER TYPE.
--     SUBTESTS ARE :
--         PART (A). TESTS FOR 'IMAGE'.
--         PART (B). TESTS FOR 'VALUE'.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C35503e is

begin
   Test
     ("C35503E",
      "CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE " &
      "CORRECT RESULTS  WHEN THE PREFIX IS A " &
      "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
      "PARAMETER IS AN INTEGER TYPE");
-- PART (A).

   declare
      type Newint is new Integer range -2_000 .. 2_000;

      generic
         type Int is (<>);
      procedure P (I1 : Int; Str : String);

      procedure P (I1 : Int; Str : String) is
         subtype Subint is
           Int range
             Int'Val (Ident_Int (-1_000)) ..
               Int'Val (Ident_Int (1_000));
      begin

         if Int'Image (I1) /= Str then
            Failed ("INCORRECT INT'IMAGE OF " & Str);
         end if;
         if Int'Image (I1)'First /= 1 then
            Failed ("INCORRECT LOWER BOUND FOR INT'IMAGE OF " & Str);
         end if;

         if Subint'Image (I1) /= Str then
            Failed ("INCORRECT SUBINT'IMAGE OF " & Str);
         end if;
         if Subint'Image (I1)'First /= 1 then
            Failed ("INCORRECT LOWER BOUND FOR SUBINT'IMAGE " & "OF " & Str);
         end if;

      end P;

      procedure Proc1 is new P (Integer);
      procedure Proc2 is new P (Newint);

   begin
      Proc1 (-500, "-500");
      Proc2 (0, " 0");
      Proc2 (99, " 99");
   end;

-----------------------------------------------------------------------

-- PART (B).

   declare
      type Newint is new Integer;

      generic
         type Int is (<>);
      procedure P (Str : String; I1 : Int);

      procedure P (Str : String; I1 : Int) is
         subtype Subint is
           Int range Int'Val (Ident_Int (0)) .. Int'Val (Ident_Int (10));

      begin
         begin
            if Int'Value (Str) /= I1 then
               Failed ("INCORRECT INT'VALUE OF """ & Str & """");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED INT'VALUE OF """ & Str & """");
         end;
         begin
            if Subint'Value (Str) /= I1 then
               Failed ("INCORRECT SUBINT'VALUE OF """ & Str & """");
            end if;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED SUBINT'VALUE " & "OF """ & Str & """");
         end;
      end P;

      procedure Proc1 is new P (Integer);
      procedure Proc2 is new P (Newint);

   begin
      Proc1 ("-500", -500);
      Proc2 (" -001E2 ", -100);
      Proc1 ("3_45", 345);
      Proc2 ("-2#1111_1111#", -255);
      Proc1 ("16#FF#", 255);
      Proc2 ("-016#0FF#", -255);
      Proc1 ("2#1110_0000#     ", 224);
      Proc2 ("-16#E#E1", -224);

   end;

   declare
      type Newint is new Integer;

      generic
         type Int is (<>);
      procedure P (Str1 : String; I1 : Int; Str2 : String);

      procedure P (Str1 : String; I1 : Int; Str2 : String) is
         subtype Subint is
           Int range Int'Val (Ident_Int (0)) .. Int'Val (Ident_Int (10));

      begin
         begin
            if Int'Value (Str1) = I1 then
               Failed
                 ("NO EXCEPTION RAISED - INT'VALUE " &
                  "WITH " &
                  Str2 &
                  " - EQUAL");
            else
               Failed
                 ("NO EXCEPTION RAISED " &
                  "- INT'VALUE WITH " &
                  Str2 &
                  " - NOT EQUAL");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - " & "INT'VALUE WITH " & Str2);
         end;
         begin
            if Subint'Value (Str1) = I1 then
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "SUBINT'VALUE WITH " &
                  Str2 &
                  " - EQUAL");
            else
               Failed
                 ("NO EXCEPTION RAISED - " &
                  "SUBINT'VALUE WITH " &
                  Str2 &
                  " - NOT EQUAL");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED  - " & "SUBINT'VALUE WITH " & Str2);
         end;
      end P;

      procedure Proc1 is new P (Integer);
      procedure Proc2 is new P (Newint);

   begin
      Proc1 ("1.0", 1, "DECIMAL POINT");
      Proc1 (Ascii.Ht & "244", 244, "LEADING 'HT'");
      Proc2 ("244" & Ascii.Ht, 244, "TRAILING 'HT'");
      Proc1 ("2__44", 244, "CONSECUTIVE '_'");
      Proc2 ("_244", 244, "LEADING '_'");
      Proc1 ("244_", 244, "TRAILING '_'");
      Proc2 ("244_E1", 2_440, "'_' BEFORE 'E'");
      Proc1 ("244E_1", 2_440, "'_' FOLLOWING 'E'");
      Proc2 ("244_e1", 2_440, "'_' BEFORE 'e'");
      Proc1 ("16#_FF#", 255, "'_' IN BASED LITERAL");
      Proc2 ("1E-0", 0, "NEGATIVE EXPONENT");
      Proc1 ("244.", 244, "TRAILING '.'");
      Proc2 ("8#811#", 0, "DIGITS OUTSIDE OF RANGE");
      Proc1 ("1#000#", 0, "BASE LESS THAN 2");
      Proc2 ("17#0#", 0, "BASE GREATER THAN 16");
   end;

   Result;
end C35503e;
