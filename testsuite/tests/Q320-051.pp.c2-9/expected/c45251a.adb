-- C45251A.ADA

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
-- CHECK THAT FOR RELATIONAL OPERATIONS ON FIXED POINT TYPES THE FOLLOWING
-- HOLD:
--       (A) A /= B IS THE SAME AS NOT (A = B).
--       (B) A < B IS THE SAME AS NOT (A >= B).
--       (C) A > B IS THE SAME AS NOT (A <= B).
--       (D) ADJACENT MODEL NUMBERS GIVE CORRECT RESULTS.
--       (E) NON-MODEL NUMBERS WITH DISTINCT MODEL INTERVALS GIVE
--           CORRECT RESULTS.
--       (F) CASE WHERE MODEL INTERVALS INTERSECT IN A SINGLE MODEL
--           NUMBER GIVES CORRECT RESULT.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/26/86

with Report; use Report;
procedure C45251a is

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S 'MANTISSA VALUE.

   type Like_Duration_M23 is delta 0.020 range -86_400.0 .. 86_400.0;
   type Decimal_M4 is delta 100.0 range -1_000.0 .. 1_000.0;

begin

   Test
     ("C45251A",
      "CHECK RELATIONAL OPERATIONS FOR FIXED POINT " & "TYPES - BASIC TYPES");

   -------------------------------------------------------------------

   declare
      A, B : Like_Duration_M23 := 0.0;
      C, D : Decimal_M4        := 0.0;
   begin
      if Equal (3, 3) then
         A := 2#0.0000_0011#; -- JUST BELOW LIKE_DURATION'SMALL.
         B := 2#0.0000_0101#; -- JUST ABOVE LIKE_DURATION'SMALL.
      end if;

      -- (A)
      if A /= B xor not (A = B) then
         Failed ("A /= B IS NOT THE SAME AS NOT (A = B)");
      end if;

      -- (B)
      if A < B xor not (A >= B) then
         Failed ("A < B IS NOT THE SAME AS NOT (A >= B)");
      end if;

      -- (C)
      if A > B xor not (A <= B) then
         Failed ("A > B IS NOT THE SAME AS NOT (A <= B)");
      end if;

      -- (D)
      if Equal (3, 3) then
         A := -(16#1_5180.00#); -- (-86_400.0)
         B := -(16#1_517F.FC#); -- (-86_400.0 + 1.0/64)

         C := 64.0; -- DECIMAL_M4'SMALL.
         D := 128.0; -- 2 * DECIMAL_M4'SMALL.
      end if;
      if "=" (Left => A, Right => B) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (A = B)");
      end if;
      if not "/=" (Left => C, Right => D) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (C /= D)");
      end if;
      if "<" (Left => B, Right => A) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (B < A)");
      end if;
      if ">" (Left => C, Right => D) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (C > D)");
      end if;
      if ">=" (Left => A, Right => B) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (A >= B)");
      end if;
      if "<=" (Left => D, Right => C) then
         Failed
           ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " & "- (D <= C)");
      end if;

      -- (E)
      if Equal (3, 3) then
         A := 0.02;  -- INTERVAL IS  1.0/64 ..  2.0/64.
         B := -0.02;  -- INTERVAL IS -2.0/64 .. -1.0/64.

         C := 800.0; -- INTERVAL IS 768.0 .. 832.0.
         D := 900.0; -- INTERVAL IS 896.0 .. 960.0.
      end if;
      if A = B then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (A = B)");
      end if;
      if not (C /= D) then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (C /= D)");
      end if;
      if A < B then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (A < B)");
      end if;
      if C > D then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (C > D)");
      end if;
      if B >= A then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (B >= A)");
      end if;
      if D <= C then
         Failed
           ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
            "INTERVALS GIVE INCORRECT RESULT - (D <= C)");
      end if;

      -- (F)
      if Equal (3, 3) then
         B := 0.035;  -- INTERVAL IS 2.0/64 .. 3.0/64.

         C := 850.0;  -- INTERVAL IS 832.0 .. 896.0.
      end if;
      if not (A <= B) then
         Failed
           ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
            "COMMON MODEL INTERVAL END-POINT GIVES " &
            "INCORRECT RESULT - (A <= B)");
      end if;
      if A > B then
         Failed
           ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
            "COMMON MODEL INTERVAL END-POINT GIVES " &
            "INCORRECT RESULT - (A > B)");
      end if;
      if not (D >= C) then
         Failed
           ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
            "COMMON MODEL INTERVAL END-POINT GIVES " &
            "INCORRECT RESULT - (D >= C)");
      end if;
      if D < C then
         Failed
           ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
            "COMMON MODEL INTERVAL END-POINT GIVES " &
            "INCORRECT RESULT - (D < C)");
      end if;
   end;

   -------------------------------------------------------------------

   Result;

end C45251a;
