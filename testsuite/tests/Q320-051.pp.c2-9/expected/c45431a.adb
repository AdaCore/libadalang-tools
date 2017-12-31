-- C45431A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES +A = A AND THAT, FOR MODEL NUMBERS, -(-A) =
-- A.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/28/86
-- PWN 02/02/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure C45431a is

begin

   Test
     ("C45431A",
      "CHECK THAT FOR FIXED POINT TYPES +A = A AND " &
      "THAT, FOR MODEL NUMBERS, -(-A) = A " & "-- BASIC TYPES");

   -------------------------------------------------------------------

   A :
   declare
      type Like_Duration is delta 0.020 range -86_400.0 .. 86_400.0;

      Non_Model_Const : constant      := 2.0 / 3;
      Non_Model_Var   : Like_Duration := 0.0;

      Small, Max, Min, Zero : Like_Duration := 0.5;
      X                     : Like_Duration := 0.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Non_Model_Var := Non_Model_Const;
         Small         := Like_Duration'Small;
         Max           := Like_Duration'Last;
         Min           := Like_Duration'First;
         Zero          := 0.0;
      end if;

      -- CHECK + OR - ZERO = ZERO:
      if "+" (Right => Zero) /= 0.0 or +Like_Duration'(0.0) /= Zero then
         Failed ("+0.0 /= 0.0");
      end if;
      if "-" (Right => Zero) /= 0.0 or -Like_Duration'(0.0) /= Zero then
         Failed ("-0.0 /= 0.0");
      end if;
      if -(-Zero) /= 0.0 then
         Failed ("-(-0.0) /= 0.0");
      end if;

      -- CHECK + AND - MAX:
      if Equal (3, 3) then
         X := Max;
      end if;
      if +X /= Max or +Like_Duration'Last /= Max then
         Failed ("+LIKE_DURATION'LAST /= LIKE_DURATION'LAST");
      end if;
      if -(-X) /= Max or -(-Like_Duration'Last) /= Max then
         Failed ("-(-LIKE_DURATION'LAST) /= LIKE_DURATION'LAST");
      end if;

      -- CHECK + AND - MIN:
      if Equal (3, 3) then
         X := Min;
      end if;
      if +X /= Min or +Like_Duration'First /= Min then
         Failed ("+LIKE_DURATION'FIRST /= LIKE_DURATION'FIRST");
      end if;
      if -(-X) /= Min or -(-Like_Duration'First) /= Min then
         Failed ("-(-LIKE_DURATION'FIRST) /= LIKE_DURATION'FIRST");
      end if;

      -- CHECK + AND - SMALL:
      if Equal (3, 3) then
         X := Small;
      end if;
      if +X /= Small or +Like_Duration'Small /= Small then
         Failed ("+LIKE_DURATION'SMALL /= LIKE_DURATION'SMALL");
      end if;
      if -(-X) /= Small or -(-Like_Duration'Small) /= Small then
         Failed ("-(-LIKE_DURATION'SMALL) /= LIKE_DURATION'SMALL");
      end if;

      -- CHECK ARBITRARY MID-RANGE NUMBERS:
      if Equal (3, 3) then
         X := 1000.984_375;
      end if;
      if +X /= 1000.984_375 or +1000.984_375 /= X then
         Failed ("+1000.984_375 /= 1000.984_375");
      end if;
      if -(-X) /= 1000.984_375 or -(-1000.984_375) /= X then
         Failed ("-(-1000.984_375) /= 1000.984_375");
      end if;

      -- CHECK "+" AND "-" FOR NON-MODEL NUMBER:
      if +Like_Duration'(Non_Model_Const) not in 0.656_25 .. 0.671_875 or
        +Non_Model_Var not in 0.656_25 .. 0.671_875 then
         Failed ("+LIKE_DURATION'(2.0 / 3) NOT IN 0.656_25 .. " & "0.671_875");
      end if;
      if -Like_Duration'(Non_Model_Const) not in -0.671_875 .. -0.656_25 or
        -Non_Model_Var not in -0.671_875 .. -0.656_25 then
         Failed
           ("-LIKE_DURATION'(2.0 / 3) NOT IN -0.671_875 " & ".. -0.656_25");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED -- A");
   end A;

   -------------------------------------------------------------------

   B :
   declare
      type Decimal_M4 is delta 100.0 range -1_000.0 .. 1_000.0;

      Non_Model_Const : constant   := 2.0 / 3;
      Non_Model_Var   : Decimal_M4 := 0.0;

      Small, Max, Min, Zero : Decimal_M4 := -128.0;
      X                     : Decimal_M4 := 0.0;
   begin
      -- INITIALIZE "CONSTANTS":
      if Equal (3, 3) then
         Non_Model_Var := Non_Model_Const;
         Small         := Decimal_M4'Small;
         Zero          := 0.0;
      end if;

      -- CHECK + OR - ZERO = ZERO:
      if +Zero /= 0.0 or +Decimal_M4'(0.0) /= Zero then
         Failed ("+0.0 /= 0.0");
      end if;
      if -Zero /= 0.0 or -Decimal_M4'(0.0) /= Zero then
         Failed ("-0.0 /= 0.0");
      end if;
      if -(-Zero) /= 0.0 then
         Failed ("-(-0.0) /= 0.0");
      end if;

      -- CHECK + AND - MAX:
      if Equal (3, 3) then
         X := Max;
      end if;
      -- CHECK + AND - SMALL:
      if Equal (3, 3) then
         X := Small;
      end if;
      if +X /= Small or +Decimal_M4'Small /= Small then
         Failed ("+DECIMAL_M4'SMALL /= DECIMAL_M4'SMALL");
      end if;
      if -(-X) /= Small or -(-Decimal_M4'Small) /= Small then
         Failed ("-(-DECIMAL_M4'SMALL) /= DECIMAL_M4'SMALL");
      end if;

      -- CHECK ARBITRARY MID-RANGE NUMBERS:
      if Equal (3, 3) then
         X := 256.0;
      end if;
      if +X /= 256.0 or +256.0 /= X then
         Failed ("+256.0 /= 256.0");
      end if;
      if -(-X) /= 256.0 or -(-256.0) /= X then
         Failed ("-(-256.0) /= 256.0");
      end if;

      -- CHECK "+" AND "-" FOR NON-MODEL NUMBER:
      if +Decimal_M4'(Non_Model_Const) not in 0.0 .. 64.0 or
        +Non_Model_Var not in 0.0 .. 64.0 then
         Failed ("+DECIMAL_M4'(2.0 / 3) NOT IN 0.0 .. 64.0");
      end if;
      if -Decimal_M4'(Non_Model_Const) not in -64.0 .. 0.0 or
        -Non_Model_Var not in -64.0 .. 0.0 then
         Failed ("-DECIMAL_M4'(2.0 / 3) NOT IN -64.0 .. 0.0");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED -- B");
   end B;

   -------------------------------------------------------------------

   Result;

end C45431a;
