-- C45651A.ADA

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
--     FOR FIXED POINT TYPES, CHECK:
--          (A) FOR MODEL NUMBERS A >= 0.0, THAT ABS A = A.
--          (B) FOR MODEL NUMBERS A <= 0.0. THAT ABS A = -A.
--          (C) FOR NON-MODEL NUMBERS A > 0.0, THAT ABS A VALUES ARE
--              WITHIN THE APPROPRIATE MODEL INTERVAL.
--          (D) FOR NON-MODEL NUMBERS A < 0.0, THAT ABS A VALUES ARE
--              WITHIN THE APPROPRIATE MODEL INTERVAL.

--     CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF
--     DURATION'BASE.

-- HISTORY:
--     WRG 9/11/86
--     PWB 3/31/88  CHANGED RANGE FOR MEMBERSHIP TEST INVOLVING
--                  ABS (DECIMAL_M4'FIRST + DECIMAL_M4'SMALL / 2).
--     RJW 8/21/89  REMOVED CHECKS INVOLVING HARD-CODED FIXED-POINT
--                  UPPER BOUNDS WHICH WERE INCORRECT FOR SOME
--                  IMPLEMENTATIONS.  REVISED HEADER.
--     PWN 02/02/95 REMOVED INCONSISTENCIES WITH ADA 9X.
--     KAS 11/14/95 REMOVED CASES THAT DEPEND ON SPECIFIC VALUE FOR 'SMALL
--     TMB 11/19/94 REMOVED CASES RELATING TO 3.5.9(8) RULES - SMALL
--                  MAY BE LESS THAN OR EQUAL TO DELTA FOR FIXED POINT.

with Report; use Report;
procedure C45651a is

-- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S 'MANTISSA VALUE.

begin

   Test
     ("C45651A",
      "CHECK THAT, FOR FIXED POINT TYPES, THE ABS " &
      "OPERATOR PRODUCES CORRECT RESULTS - BASIC " &
      "TYPES");

   -------------------------------------------------------------------

   A :
   declare
      type Like_Duration_M23 is delta 0.020 range -86_400.0 .. 86_400.0;

      Non_Model_Const : constant          := 2.0 / 3;
      Non_Model_Var   : Like_Duration_M23 := 0.0;

      Small, Max, Min, Zero : Like_Duration_M23 := 0.5;
      X                     : Like_Duration_M23 := 1.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Small         := Like_Duration_M23'Small;
         Max           := Like_Duration_M23'Last;
         Min           := Like_Duration_M23'First;
         Zero          := 0.0;
         Non_Model_Var := Non_Model_Const;
      end if;

      -- (A)
      if Equal (3, 3) then
         X := Small;
      end if;
      if abs X /= Small or X /= abs Like_Duration_M23'Small then
         Failed ("ABS (1.0 / 64) /= (1.0 / 64)");
      end if;
      if Equal (3, 3) then
         X := Max;
      end if;
      if abs X /= Max or X /= abs Like_Duration_M23'Last then
         Failed ("ABS 86_400.0 /= 86_400.0");
      end if;

      -- (B)
      if Equal (3, 3) then
         X := -Small;
      end if;
      if abs X /= Small or abs (-Like_Duration_M23'Small) /= Small then
         Failed ("ABS -(1.0 / 64) /= (1.0 / 64)");
      end if;
      if Equal (3, 3) then
         X := Min;
      end if;
      if abs X /= Max or abs Like_Duration_M23'First /= Max then
         Failed ("ABS -86_400.0 /= 86_400.0");
      end if;

      -- (A) AND (B)
      if Equal (3, 3) then
         X := 0.0;
      end if;
      if "ABS" (Right => X) /= Zero or X /= abs 0.0 then
         Failed ("ABS 0.0 /= 0.0 -- (LIKE_DURATION_M23)");
      end if;

      -- CHECK THAT VALUE OF NON_MODEL_VAR IS IN THE RANGE 42 * 'SMALL .. 43 *
      -- 'SMALL:
      if Non_Model_Var not in 0.656_25 .. 0.671_875 then
         Failed ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE " & "- A");
      end if;

      -- (C)
      if abs Non_Model_Var not in 0.656_25 .. 0.671_875 or
        abs Like_Duration_M23'(Non_Model_Const) not in 0.656_25 .. 0.671_875
      then
         Failed ("ABS (2.0 / 3) NOT IN CORRECT RANGE - A");
      end if;
      if Equal (3, 3) then
         X := 86_399.992_187_5;  -- LIKE_DURATION_M23'LAST -
         -- 1.0 / 128.
      end if;
      if abs X not in 86_399.984_375 .. 86_400.0 or
        abs (Like_Duration_M23'Last - Like_Duration_M23'Small / 2) not in
          86_399.984_375 .. 86_400.0
      then
         Failed
           ("ABS (LIKE_DURATION_M23'LAST - " &
            "LIKE_DURATION_M23'SMALL / 2) NOT IN CORRECT " &
            "RANGE");
      end if;

      -- (D)
      if Equal (3, 3) then
         X := -Non_Model_Const;
      end if;
      if abs X not in 0.656_25 .. 0.671_875 or
        abs (-Like_Duration_M23'(Non_Model_Const)) not in 0.656_25 .. 0.671_875
      then
         Failed ("ABS (-2.0 / 3) NOT IN CORRECT RANGE - A");
      end if;
      if Equal (3, 3) then
         X := -86_399.992_187_5;  -- LIKE_DURATION_M23'FIRST +
         -- 1.0 / 128.
      end if;
      if abs X not in 86_399.984_375 .. 86_400.0 or
        abs (Like_Duration_M23'First + Like_Duration_M23'Small / 2) not in
          86_399.984_375 .. 86_400.0
      then
         Failed
           ("ABS (LIKE_DURATION_M23'FIRST +" &
            "LIKE_DURATION_M23'SMALL / 2) NOT IN CORRECT " &
            "RANGE");
      end if;
   end A;

   -------------------------------------------------------------------

   B :
   declare
      type Decimal_M4 is delta 100.0 range -1_000.0 .. 1_000.0;

      Non_Model_Const : constant   := 2.0 / 3;
      Non_Model_Var   : Decimal_M4 := 0.0;

      Small, Max, Min, Zero : Decimal_M4 := 128.0;
      X                     : Decimal_M4 := 0.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Small         := Decimal_M4'Small;
         Zero          := 0.0;
         Non_Model_Var := Non_Model_Const;
      end if;

      -- (A)
      if Equal (3, 3) then
         X := Small;
      end if;
      if abs X /= Small or X /= abs Decimal_M4'Small then
         Failed ("ABS 64.0 /= 64.0");
      end if;

      -- (B)
      if Equal (3, 3) then
         X := -Small;
      end if;
      if abs X /= Small or abs (-Decimal_M4'Small) /= Small then
         Failed ("ABS -64.0 /= 64.0");
      end if;

      -- (A) AND (B)
      if Equal (3, 3) then
         X := 0.0;
      end if;
      if abs X /= Zero or X /= abs 0.0 then
         Failed ("ABS 0.0 /= 0.0 -- (DECIMAL_M4)");
      end if;

      -- CHECK THE VALUE OF NON_MODEL_VAR:
      if Non_Model_Var not in 0.0 .. 64.0 then
         Failed ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE " & "- B");
      end if;

      -- (C)
      if abs Non_Model_Var not in 0.0 .. 64.0 or
        abs Decimal_M4'(Non_Model_Const) not in 0.0 .. 64.0
      then
         Failed ("ABS (2.0 / 3) NOT IN CORRECT RANGE - B");
      end if;
      if Equal (3, 3) then
         X := 37.0;  -- INTERVAL IS 0.0 .. 64.0.
      end if;
      if Equal (3, 3) then
         X := 928.0;
      end if;

      -- (D)
      if Equal (3, 3) then
         X := -Non_Model_Const;
      end if;
      if abs X not in 0.0 .. 64.0 or
        abs (-Decimal_M4'(Non_Model_Const)) not in 0.0 .. 64.0
      then
         Failed ("ABS -(2.0 / 3) NOT IN CORRECT RANGE - B");
      end if;
      if Equal (3, 3) then
         X := -37.0;  -- INTERVAL IS -SMALL .. 0.0.
      end if;
      if Equal (3, 3) then
         X := -928.0;
      end if;
   end B;

   -------------------------------------------------------------------

   Result;

end C45651a;
