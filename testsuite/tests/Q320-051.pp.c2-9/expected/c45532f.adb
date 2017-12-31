-- C45532F.ADA

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
--       THIS TEST REQUIRES MIN_WORD_LENGTH = 16.
--       THIS TEST USES VALUES OF DELTA WHICH ARE LESS THAN 0.5.
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
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     RDH 04/27/90 REVISED APPLICABILITY CRITERIA.
--     BCB 10/03/90 REMOVED APPLICABILITY CRITERIA AND N/A => ERROR
--                  LINE.  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report;
procedure C45532f is

   use Report;

   Min_Word_Length : constant := 16;            -- MUST BE EVEN & >= 6
   Full_Scale      : constant := 2**(Min_Word_Length - 1);
   Forth           : constant := Full_Scale / 4;
   A_Third         : constant := Full_Scale / 3;
   Del1            : constant := 0.5 / Full_Scale;
   type Fx_0p5 is delta Del1 * 1 range -0.5 .. 0.5 - Del1 * 1;
   type Fx_1 is delta Del1 * 2 range -1.0 .. 1.0 - Del1 * 2;
   type Fx_2 is delta Del1 * 4 range -2.0 .. 2.0 - Del1 * 4;

begin
   Test ("C45532F", "FIXED POINT OPERATOR ""/""");

   --------------------------------------------------

   -- CASE A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.

   A :
   declare
      A                        : Fx_0p5 := 0.0;
      B                        : Fx_1   := 0.0;
      Result_Value             : Fx_2   := 0.0;
      Lowest_Acceptable_Value  : Fx_2   := Fx_2 (0.5);
      Highest_Acceptable_Value : Fx_2   := Fx_2 (0.5);
   begin
      if Equal (3, 3) then
         A := Fx_0p5 (0.125);               -- A MODEL NUMBER
         B := Fx_1 (0.25);                -- A MODEL NUMBER
      end if;

      Result_Value := Fx_2 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN A, B, AND A / B ARE ALL MODEL NUMBERS");
      end if;
   end A;

   --------------------------------------------------

   -- CASE B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.

   B :
   declare
      A                        : Fx_0p5 := 0.0;
      B                        : Fx_0p5 := 0.0;
      Result_Value             : Fx_1   := 0.0;
      Lowest_Acceptable_Value  : Fx_1   := Fx_1 (2 * Del1 * A_Third);
      Highest_Acceptable_Value : Fx_1   := Fx_1 (2 * Del1 * (A_Third + 1));
   begin
      if Equal (3, 3) then
         A := Fx_0p5 (Del1 * 1);            -- A MODEL NUMBER
         B := Fx_0p5 (Del1 * 3);            -- A MODEL NUMBER
      end if;

      Result_Value := Fx_1 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN A, B MODEL NUMBERS A / B NOT");
      end if;
   end B;

   --------------------------------------------------

   -- CASE C) THE OPERATOR /, USING NO MODEL NUMBERS

   C :
   declare
      A            : Fx_1     := 0.0;
      B            : Fx_1     := 0.0;
      Result_Value : Fx_0p5   := 0.0;
      Low_Count    : constant := 2 * A_Third;
      --   := (2 * FULL_SCALE * (2 * FORTH + 0))
      --                      / (6 * FORTH + 2);
      High_Count : constant := 2 * A_Third + 4;
      --   := (2 * FULL_SCALE * (2 * FORTH + 2))
      --                      / (6 * FORTH + 0);
      Lowest_Acceptable_Value  : Fx_0p5 := Fx_0p5 (Del1 * Low_Count);
      Highest_Acceptable_Value : Fx_0p5 := Fx_0p5 (Del1 * High_Count);
   begin
      if Equal (3, 3) then     -- A AND B NOT MODEL NUMBERS
         A := Fx_1 (Del1 * (2 * Forth + 1));
         B := Fx_1 (Del1 * (6 * Forth + 1));
      end if;

      Result_Value := Fx_0p5 (A / B);

      if (Result_Value < Lowest_Acceptable_Value) or
        (Result_Value > Highest_Acceptable_Value) then
         Failed
           ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL " &
            "WHEN USING NO MODEL NUMBERS");
      end if;
   end C;

   --------------------------------------------------

   Result;

end C45532f;
