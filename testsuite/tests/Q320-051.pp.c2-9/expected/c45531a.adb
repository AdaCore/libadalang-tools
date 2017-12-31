-- C45531A.ADA

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
--     CHECK THAT THE OPERATOR "*" PRODUCES CORRECT RESULTS
--     FOR MIXED FIXED POINT AND INTEGER TYPES USING 4 SUBTESTS.
--       THIS TEST REQUIRES MIN_WORD_LENGTH = 12.
--       THIS TEST USES VALUES OF DELTA WHICH ARE LESS THAN 0.5.
--
--     TEST CASES ARE:
--       A) INTEGER * FIXED WHEN ALL VALUES ARE MODEL NUMBERS.
--       B) FIXED * INTEGER WHEN ALL VALUES ARE MODEL NUMBERS.
--       C) INTEGER * FIXED FOR NON-MODEL NUMBERS.
--       D) FIXED * INTEGER FOR NON-MODEL NUMBERS.
--
--     REPEAT FOR MINIMUM REQUIRED WORD LENGTHS OF 12, 16, 32 AND 48,
--     WITH RANGE <, =, AND > THAN 1.0 AND
--     WITH DELTA <, =, AND > THAN 1.0.

-- HISTORY:
--     NTW 09/08/86 CREATED ORIGINAL TEST.
--     RJW 11/05/86 REVISED COMMENTS.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.
--     BCB 04/27/90 REVISED APPLICABILITY CRITERIA.
--     BCB 10/03/90 REMOVED APPLICABILITY CRITERIA AND N/A => ERROR
--                  LINE.  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report;
procedure C45531a is

   use Report;

   Min_Word_Length : constant := 12;
   Full_Scale      : constant := 2**(Min_Word_Length - 1);
   Forth           : constant := Full_Scale / 4;
   Del1            : constant := 0.5 / Full_Scale;
   Del4            : constant := 4.0 * Del1;
   type Fx_0p5 is delta Del1 * 1 range -0.5 .. 0.5 - Del1 * 1;
   type Fx_1 is delta Del1 * 2 range -1.0 .. 1.0 - Del1 * 2;
   type Fx_2 is delta Del1 * 4 range -2.0 .. 2.0 - Del1 * 4;

begin
   Test
     ("C45531A",
      "MIXED FIXED POINT AND INTEGER ""*"" " & "FOR RANGE <, =, > 1.0");

   --------------------------------------------------

   -- CASE A) INTEGER * FIXED WHEN ALL VALUES ARE MODEL NUMBERS.

   A :
   declare
      A                        : Integer := 0;
      B                        : Fx_0p5  := 0.0;
      Result_Value             : Fx_0p5  := 0.0;
      Lowest_Acceptable_Value  : Fx_0p5  := Fx_0p5 (0.375);
      Highest_Acceptable_Value : Fx_0p5  := Fx_0p5 (0.375);
   begin
      if Equal (3, 3) then
         A := 3;
         B := Fx_0p5 (0.125);               -- A MODEL NUMBER
      end if;

      Result_Value := A * B;

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""*"" OUTSIDE RESULT MODEL INTERVAL " &
            "FOR INTEGER * FIXED " & "WHEN ALL VALUES ARE MODEL NUMBERS");
      end if;
   end A;

   --------------------------------------------------

   -- CASE B) FIXED * INTEGER WHEN ALL VALUES ARE MODEL NUMBERS.

   B :
   declare
      A                        : Fx_1    := 0.0;
      B                        : Integer := 0;
      Result_Value             : Fx_1    := 0.0;
      Lowest_Acceptable_Value  : Fx_1    := Fx_1 (0.75);
      Highest_Acceptable_Value : Fx_1    := Fx_1 (0.75);
   begin
      if Equal (3, 3) then
         A := Fx_1 (0.125);                 -- A MODEL NUMBER
         B := 6;
      end if;

      Result_Value := A * B;

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""*"" OUTSIDE RESULT MODEL INTERVAL " &
            "FOR FIXED * INTEGER " & "WHEN ALL VALUES ARE MODEL NUMBERS");
      end if;
   end B;

   --------------------------------------------------

   -- CASE C) INTEGER * FIXED FOR NON-MODEL NUMBERS.

   C :
   declare
      A                        : Integer  := 0;
      B                        : Fx_2     := 0.0;
      Result_Value             : Fx_2     := 0.0;
      Low_Count                : constant := (3 * (Forth + 0));
      High_Count               : constant := (3 * (Forth + 1));
      Lowest_Acceptable_Value  : Fx_2     := Fx_2 (Del4 * Low_Count);
      Highest_Acceptable_Value : Fx_2     := Fx_2 (Del4 * High_Count);
   begin
      if Equal (3, 3) then        -- B NOT A MODEL NUMBER
         A := 3;
         B := Fx_2 (Del4 * Forth + Del1);
      end if;

      Result_Value := A * B;

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""*"" OUTSIDE RESULT MODEL INTERVAL " &
            "FOR INTEGER * FIXED FOR NON-MODEL NUMBERS");

      end if;
   end C;

   --------------------------------------------------

   -- CASE D) FIXED * INTEGER FOR NON-MODEL NUMBERS.

   D :
   declare
      A                        : Fx_2     := 0.0;
      B                        : Integer  := 0;
      Result_Value             : Fx_2     := 0.0;
      Low_Count                : constant := (3 * (Forth + 0));
      High_Count               : constant := (3 * (Forth + 1));
      Lowest_Acceptable_Value  : Fx_2     := Fx_2 (Del4 * Low_Count);
      Highest_Acceptable_Value : Fx_2     := Fx_2 (Del4 * High_Count);
   begin
      if Equal (3, 3) then        -- A NOT A MODEL NUMBER
         A := Fx_2 (Del4 * Forth + Del1);
         B := 3;
      end if;

      Result_Value := A * B;

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""*"" OUTSIDE RESULT MODEL INTERVAL " &
            "FOR FIXED * INTEGER FOR NON-MODEL NUMBERS");
      end if;
   end D;

   --------------------------------------------------

   Result;

end C45531a;
