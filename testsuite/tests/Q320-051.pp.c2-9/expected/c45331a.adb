-- C45331A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE OPERATORS "+" AND "-" PRODUCE CORRECT
-- RESULTS WHEN:
--      (A) A, B, A+B, AND A-B ARE ALL MODEL NUMBERS.
--      (B) A IS A MODEL NUMBER BUT B, A+B, AND A-B ARE NOT.
--      (C) A, B, A+B, AND A-B ARE ALL MODEL NUMBERS WITH DIFFERENT
--          SUBTYPES.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/27/86
-- KAS 11/14/95 REDUCE EXPECTATION FOR T'SMALL KAS 11/30/95 ONE MORE CHANGE...
-- PWN 02/28/96 CLEANED COMMENTS FOR RELEASE KAS 03/18/96 ELIDED TWO 'SMALL
-- CASES FOR 2.1

with Report; use Report;
procedure C45331a is

   type Like_Duration is delta 0.020 range -86_400.0 .. 86_400.0;
   -- 'MANTISSA = 23.
   subtype F is Like_Duration delta 0.25 range -1_000.0 .. 1_000.0;
   subtype St_F1 is Like_Duration delta 0.5 range -4.0 .. 3.0;
   subtype St_F2 is
     Like_Duration delta 1.0 / 16 range -13.0 / 16 .. 5.0 + 1.0 / 16;

begin

   Test
     ("C45331A",
      "CHECK THAT FOR FIXED POINT TYPES THE " &
      "OPERATORS ""+"" AND ""-"" PRODUCE CORRECT " &
      "RESULTS - BASIC TYPES");

   -------------------------------------------------------------------

   A : declare
      Small, Max, Min, Zero : F := 0.5;
      X                     : F := 0.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Small := F'Small;
         Max   := F'Last;  -- BECAUSE F'LAST < F'LARGE AND F'LAST
         -- IS A MODEL NUMBER.
         Min  := F'First; -- F'FIRST IS A MODEL NUMBER.
         Zero := 0.0;
      end if;

      -- CHECK SMALL + OR - ZERO = SMALL:
      if "+" (Left => Small, Right => Zero) /= Small or
        0.0 + Small /= Small
      then
         Failed ("F'SMALL + 0.0 /= F'SMALL");
      end if;
      if "-" (Left => Small, Right => Zero) /= Small or
        Small - 0.0 /= Small
      then
         Failed ("F'SMALL - 0.0 /= F'SMALL");
      end if;

      -- CHECK MAX + OR - ZERO = MAX:
      if Max + Zero /= Max or 0.0 + Max /= Max then
         Failed ("F'LAST + 0.0 /= F'LAST");
      end if;
      if Max - Zero /= Max or Max - 0.0 /= Max then
         Failed ("F'LAST - 0.0 /= F'LAST");
      end if;

      -- CHECK SMALL - SMALL = 0.0:
      if Equal (3, 3) then
         X := Small;
      end if;
      if Small - X /= 0.0 or
        Small - Small /= 0.0 or
        F'Small - F'Small /= 0.0
      then
         Failed ("F'SMALL - F'SMALL /= 0.0");
      end if;

      -- CHECK MAX - MAX = 0.0:
      if Equal (3, 3) then
         X := Max;
      end if;
      if Max - X /= 0.0 or Max - Max /= 0.0 or F'Last - F'Last /= 0.0 then
         Failed ("F'LAST - F'LAST /= 0.0");
      end if;

      -- CHECK ZERO - MAX = MIN, MIN - MIN = 0.0, AND MIN + MAX = 0.0:
      if Equal (3, 3) then
         X := Zero - Max;
      end if;
      if X /= Min then
         Failed ("0.0 - 1000.0 /= -1000.0");
      end if;
      if Equal (3, 3) then
         X := Min;
      end if;
      if Min - X /= 0.0 or Min - Min /= 0.0 or F'First - F'First /= 0.0 then
         Failed ("F'FIRST - F'FIRST /= 0.0");
      end if;
      if Min + Max /= 0.0 or Max + Min /= 0.0 or F'First + F'Last /= 0.0 then
         Failed ("-1000.0 + 1000.0 /= 0.0");
      end if;

      -- CHECK ADDITION AND SUBTRACTION FOR ARBITRARY MID-RANGE NUMBERS:
      if Equal (3, 3) then
         X := 100.75;
      end if;
      if (X + Small) /= (Small + X) or
        (X + Small) > (X + 0.25)
      then -- X + SMALL SB <= X + DELTA
         Failed ("X + SMALL DELIVERED BAD RESULT");
      end if;

      -- CHECK (MAX - SMALL) + SMALL = MAX:
      if Equal (3, 3) then
         X := Max - Small;
      end if;
      if X + Small /= Max then
         Failed ("(MAX - SMALL) + SMALL /= MAX");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED - A");
   end A;

   -------------------------------------------------------------------

   B : declare
      Non_Model_Const : constant := 2.0 / 3;
      Non_Model_Var   : F        := 0.0;

      Small, Max, Min, Zero : F := 0.5;
      X                     : F := 0.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Small := F'Small;
         Max   := F'Last;  -- BECAUSE F'LAST < F'LARGE AND
         -- F'LAST  IS A MODEL NUMBER.
         Min           := F'First; -- F'FIRST IS A MODEL NUMBER.
         Zero          := 0.0;
         Non_Model_Var := Non_Model_Const;
      end if;

      -- CHECK VALUE OF NON_MODEL_VAR:
      if Non_Model_Var not in 0.5 .. 0.75 then
         Failed ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE");
      end if;

      -- CHECK NON-MODEL VALUE + OR - ZERO:
      if Non_Model_Var + Zero not in 0.5 .. 0.75 or
        F'(0.0) + Non_Model_Const not in 0.5 .. 0.75
      then
         Failed ("(2.0 / 3) + 0.0 NOT IN 0.5 .. 0.75");
      end if;
      if Non_Model_Var - Zero not in 0.5 .. 0.75 or
        Non_Model_Const - F'(0.0) not in 0.5 .. 0.75
      then
         Failed ("(2.0 / 3) - 0.0 NOT IN 0.5 .. 0.75");
      end if;

      -- CHECK ZERO - NON-MODEL:
      if F'(0.0) - Non_Model_Const not in -0.75 .. -0.5 then
         Failed ("0.0 - (2.0 / 3) NOT IN -0.75 .. -0.5");
      end if;

      if F'(1.0) - Non_Model_Const not in 0.25 .. 0.5 then
         Failed ("1.0 - (2.0 / 3) NOT IN 0.25 .. 0.5");
      end if;

      -- CHECK ADDITION AND SUBTRACTION OF NON-MODEL NEAR MIN AND MAX:
      if Min + Non_Model_Var not in -999.5 .. -999.25 or
        Non_Model_Const + F'First not in -999.5 .. -999.25
      then
         Failed ("-1000.0 + (2.0 / 3) NOT IN -999.5 .. -999.25");
      end if;
      if Max - Non_Model_Var not in 999.25 .. 999.5 or
        F'Last - Non_Model_Const not in 999.25 .. 999.5
      then
         Failed ("1000.0 - (2.0 / 3) NOT IN 999.25 .. 999.5");
      end if;

      -- CHECK ADDITION AND SUBTRACTION FOR ARBITRARY MID-RANGE MODEL NUMBER
      -- WITH NON-MODEL:
      if Equal (3, 3) then
         X := -213.25;
      end if;
      if X + Non_Model_Const not in -212.75 .. -212.5 then
         Failed ("-213.25 + (2.0 / 3) NOT IN -212.75 .. -212.5");
      end if;
      if Non_Model_Var - X not in 213.75 .. 214.0 then
         Failed ("(2.0 / 3) - (-213.25) NOT IN 213.75 .. 214.0");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED - B");
   end B;

   -------------------------------------------------------------------

   C : declare
      A_Small, A_Max, A_Min : St_F1 := 0.0;
      B_Small, B_Max, B_Min : St_F2 := 0.0;
      X                     : F;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         A_Small := St_F1'Small;
         A_Max   := St_F1'Last;  -- BECAUSE 'LAST < 'LARGE AND
         -- 'LAST  IS A MODEL NUMBER.
         A_Min := St_F1'First; -- 'FIRST IS A MODEL NUMBER.

         B_Small := St_F2'Small;
         B_Max   := St_F2'Last;  -- BECAUSE 'LAST <= 'LARGE AND
         -- 'LAST  IS A MODEL NUMBER.
         B_Min := St_F2'First; -- 'FIRST IS A MODEL NUMBER.
      end if;

      if A_Min + B_Min /= -4.812_5 then
         Failed ("-4.0 + (-0.8125) /= -4.8125");
      end if;

      if A_Min - B_Min /= -3.187_5 then
         Failed ("-4.0 - (-0.8125) /= -3.1875");
      end if;

      if (A_Min + B_Small) not in A_Min .. -3.937_5 then
         Failed ("(A_MIN + B_SMALL) NOT IN A_MIN .. -3.9375");
      end if;

      if (A_Min - B_Small) not in -4.062_5 .. -4.0 then
         Failed ("(A_MIN - B_SMALL) NOT IN -4.0 .. -4.0625");
      end if;

      if A_Min + B_Max /= 1.062_5 then
         Failed ("-4.0 + 5.0625 /= 1.0625");
      end if;

      if A_Min - B_Max /= -9.062_5 then
         Failed ("-4.0 - 5.0625 /= -9.0625");
      end if;

      if (A_Small + B_Min) not in B_Min .. -0.312_5 then
         Failed ("(A_SMALL + B_MIN) NOT IN  B_MIN..-0.3125");
      end if;

      if (A_Small - B_Min) not in +0.812_5 .. 1.312_5 then
         Failed ("(A_SMALL - B_MIN) NOT IN -0.8125 .. 1.3125");
      end if;

      if (A_Small + B_Max) not in 5.062_5 .. 5.562_5 then
         Failed ("(A_SMALL + B_MAX) NOT IN 5.0625 .. 5.5625");
      end if;

      if (A_Small - B_Max) not in -5.062_5 .. -4.562_5 then
         Failed ("(A_SMALL - B_MAX) NOT IN -5.0625 .. -4.5625");
      end if;

      if A_Max + B_Min /= 2.187_5 then
         Failed ("3.0 + (-0.8125) /= 2.1875");
      end if;

      if A_Max - B_Min /= 3.812_5 then
         Failed ("3.0 - (-0.8125) /= 3.8125");
      end if;

      if (A_Max + B_Small) not in 3.0 .. 3.062_5 then
         Failed ("(A_MAX + B_SMALL) NOT IN 3.0 .. 3.0625");
      end if;

      if (A_Max - B_Small) not in 2.937_5 .. 3.0 then
         Failed ("(A_MAX - B_SMALL) NOT IN 2.9375..3.0");
      end if;

      if A_Max + B_Max /= 8.062_5 then
         Failed ("3.0 + 5.0625 /= 8.0625");
      end if;

      if A_Max - B_Max /= -2.062_5 then
         Failed ("3.0 - 5.0625 /= -2.0625");
      end if;

      X := B_Min - A_Min;
      if X not in 3.0 .. 3.25 then
         Failed ("-0.8125 - (-4.0) NOT IN RANGE");
      end if;

      X := B_Min - A_Small;
      if X not in -1.312_5 .. -0.812_5 then
         Failed ("B_MIN - A_SMALL NOT IN RANGE");
      end if;

      X := B_Min - A_Max;
      if X not in -4.0 .. -3.75 then
         Failed ("-0.8125 - 3.0 NOT IN RANGE");
      end if;

      X := B_Small - A_Min;
      if X not in 4.0 .. 4.062_5 then
         Failed ("B_SMALL - A_MIN NOT IN RANGE");
      end if;

      X := B_Small - A_Max;
      if X not in -3.0 .. -2.75 then
         Failed ("B_SMALL - A_MAX NOT IN RANGE");
      end if;

      X := B_Max - A_Min;
      if X not in 9.0 .. 9.25 then
         Failed ("5.0625 - (-4.0) NOT IN RANGE");
      end if;

      X := B_Max - A_Small;
      if X not in 4.56 .. 5.062_5 then
         Failed ("5.0625 - 0.5 NOT IN RANGE");
      end if;

      X := B_Max - A_Max;
      if X not in 2.0 .. 2.25 then
         Failed ("5.0625 - 3.0 NOT IN RANGE");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED - C");
   end C;

   -------------------------------------------------------------------

   Result;

end C45331a;
