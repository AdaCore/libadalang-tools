-- C45532D.ADA

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
--     CHECK THAT THE OPERATOR "/" PRODUCES CORRECT RESULTS
--     FOR FIXED POINT TYPES USING 3 SUBTESTS.
--       THIS TEST REQUIRES MIN_WORD_LENGTH = 12.
--       THIS TEST USES VALUES OF DELTA WHICH ARE GREATER THAN OR
--       EQUAL TO 0.5.
--
--     TEST CASES ARE:
--       A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.
--       B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.
--       C) THE OPERATOR /, USING NO MODEL NUMBERS.
--
--     REPEAT FOR MINIMUM REQUIRED WORD LENGTHS OF 12, 16, 32 AND 48,
--     WITH RANGE <, =, AND > THAN 1.0 AND
--     WITH DELTA <, =, AND > THAN 1.0.

-- HISTORY:
--     NTW 09/08/86 CREATED ORIGINAL TEST.
--     RJW 11/05/86 REVISED COMMENTS.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.
--     RDH 04/27/90 REVISED APPLICABILITY CRITERIA.
--     BCB 10/03/90 REMOVED APPLICABILITY CRITERIA AND N/A => ERROR
--                  LINE.  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report;
procedure C45532d is

   use Report;

   Min_Word_Length : constant := 12;
   Full_Scale      : constant := 2**(Min_Word_Length - 1);
   A_Third         : constant := Full_Scale / 3;
   Rng1            : constant := Full_Scale * 0.5;
   type Fx_0p5 is delta 0.5 range -Rng1 * 1 .. Rng1 * 1 - 0.5;
   type Fx_1 is delta 1.0 range -Rng1 * 2 .. Rng1 * 2 - 1.0;
   type Fx_Rng1 is
     delta Rng1 range -Rng1 * Full_Scale .. Rng1 * (Full_Scale - 1);

begin
   Test
     ("C45532D",
      "FIXED POINT OPERATOR ""/"" " & "FOR DELTA <, =, AND > 1.0");

   --------------------------------------------------

   -- CASE A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.

   A : declare
      A                        : Fx_Rng1 := 0.0;
      B                        : Fx_0p5  := 0.0;
      Result_Value             : Fx_0p5  := 0.0;
      Lowest_Acceptable_Value  : Fx_0p5  := Fx_0p5 (Rng1 / 2);
      Highest_Acceptable_Value : Fx_0p5  := Fx_0p5 (Rng1 / 2);
   begin
      if Equal (3, 3) then
         A := Fx_Rng1 (Rng1 * Rng1 / 4);    -- A MODEL NUMBER
         B := Fx_0p5 (Rng1 / 2);           -- A MODEL NUMBER
      end if;

      Result_Value := Fx_0p5 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value)
      then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN A, B, AND A / B ARE ALL MODEL NUMBERS");
      end if;
   end A;

   --------------------------------------------------

   -- CASE B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.

   B : declare
      A                        : Fx_Rng1 := 0.0;
      B                        : Fx_1    := 0.0;
      Result_Value             : Fx_0p5  := 0.0;
      Lowest_Acceptable_Value  : Fx_0p5  := Fx_0p5 (0.5 * A_Third);
      Highest_Acceptable_Value : Fx_0p5  := Fx_0p5 (0.5 * (A_Third + 1));
   begin
      if Equal (3, 3) then
         A := Fx_Rng1 (Rng1);               -- A MODEL NUMBER
         B := Fx_1 (3.0);                -- A MODEL NUMBER
      end if;

      Result_Value := Fx_0p5 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value)
      then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN A, B MODEL NUMBERS A / B NOT");
      end if;
   end B;

   --------------------------------------------------

   -- CASE C) THE OPERATOR /, USING NO MODEL NUMBERS

   C : declare
      A                        : Fx_Rng1 := 0.0;
      B                        : Fx_1    := 0.0;
      Result_Value             : Fx_1    := 0.0;
      Lowest_Acceptable_Value  : Fx_1    := Fx_1 (Rng1 - 3.0);
      Highest_Acceptable_Value : Fx_1    := Fx_1 (Rng1 + 4.0);
   begin
      if Equal (3, 3) then
         A := Fx_Rng1 (Rng1 * Rng1 / 3);    -- NOT A MODEL NUMBER
         B := Fx_1 (Rng1 / 3);           -- NOT A MODEL NUMBER
      end if;

      Result_Value := Fx_1 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value)
      then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN USING NO MODEL NUMBERS");
      end if;
   end C;

   --------------------------------------------------

   Result;

end C45532d;
