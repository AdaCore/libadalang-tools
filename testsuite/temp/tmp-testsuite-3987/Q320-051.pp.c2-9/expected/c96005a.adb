-- C96005A.ADA

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
-- CHECK THE CORRECTNESS OF THE ADDITION AND SUBTRACTION FUNCTIONS IN
-- THE PREDEFINED PACKAGE CALENDAR, AND APPROPRIATE EXCEPTION HANDLING.
-- SPECIFICALLY,
--   (A) CHECK THAT ADDITION AND SUBTRACTION OPERATORS WORK CORRECTLY ON
--       VALUES OF TYPE TIME.

-- CPP 8/16/84

with Calendar; use Calendar;
with Report;   use Report;
-- WITH TEXT_IO;  USE TEXT_IO;
procedure C96005a is

-- PACKAGE DURATION_IO IS NEW FIXED_IO (DURATION);
-- USE DURATION_IO;

begin
   Test
     ("C96005A",
      "CHECK THAT THE ADDITION AND SUBTRACTION " &
      "FUNCTIONS FOR VALUES OF TYPE TIME WORK CORRECTLY");

   -----------------------------------------------

   begin     -- (A)

      -- ADDITION TESTS FOLLOW.
      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 13, 0.0);
         New_Time := Now + Increment;
         if New_Time /= Time_Of (1_984, 8, 13, 1.0) then
            Failed ("SUM OF TIMES IS INCORRECT - (A)1");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 13, 0.0);
         New_Time := Increment + Now;
         if New_Time /= Time_Of (1_984, 8, 13, 1.0) then
            Failed ("SUM OF TIMES IS INCORRECT - (A)2");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 13, 0.0);
         New_Time := "+" (Increment, Now);
         if New_Time /= Time_Of (1_984, 8, 13, 1.0) then
            Failed ("SUM OF TIMES IS INCORRECT - (A)3");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 13, 0.0);
         New_Time := "+" (Left => Now, Right => Increment);
         if New_Time /= Time_Of (1_984, 8, 13, 1.0) then
            Failed ("SUM OF TIMES IS INCORRECT - (A)4");
         end if;
      end;

      -- SUBTRACTION TESTS FOLLOW.
      declare
         Now, Once  : Time;
         Difference : Duration;
      begin
         Now        := Time_Of (1_984, 8, 13, 45_000.0);
         Once       := Time_Of (1_984, 8, 12, 45_000.0);
         Difference := Now - Once;
         if Difference /= 86_400.0 then
            Failed ("DIFFERENCE OF TIMES IS INCORRECT - (A)1");
            -- COMMENT ("DIFFERENCE YIELDS: ");
            -- PUT (DIFFERENCE);
         end if;
      end;

      declare
         -- TIMES IN DIFFERENT MONTHS.
         Now, Once  : Time;
         Difference : Duration;
      begin
         Now        := Time_Of (1_984, 8, Ident_Int (1), 60.0);
         Once       := Time_Of (1_984, 7, 31, 86_399.0);
         Difference := "-" (Now, Once);
         if Difference /= 61.0 then
            Failed ("DIFFERENCE OF TIMES IS INCORRECT - (A)2");
            -- COMMENT ("DIFFERENCE YIELDS: ");
            -- PUT (DIFFERENCE);
         end if;
      end;

      declare
         -- TIMES IN DIFFERENT YEARS.
         Now, After : Time;
         Difference : Duration;
      begin
         Now        := Time_Of (Ident_Int (1_999), 12, 31, 86_399.0);
         After      := Time_Of (2_000, 1, 1, 1.0);
         Difference := "-" (Left => After, Right => Now);
         if Difference /= 2.0 then
            Failed ("DIFFERENCE OF TIMES IS INCORRECT - (A)3");
            -- COMMENT ("DIFFERENCE YIELDS: ");
            -- PUT (DIFFERENCE);
         end if;
      end;

      declare
         -- TIMES IN A LEAP YEAR.
         Now, Leap  : Time;
         Difference : Duration;
      begin
         Now        := Time_Of (1_984, 3, 1);
         Leap       := Time_Of (1_984, 2, 29, 86_399.0);
         Difference := Now - Leap;
         if Difference /= 1.0 then
            Failed ("DIFFERENCE OF TIMES IS INCORRECT - (A)4");
            -- COMMENT ("DIFFERENCE YIELDS: ");
            -- PUT (DIFFERENCE);
         end if;
      end;

      declare
         -- TIMES IN A NON-LEAP YEAR.
         Now, Non_Leap : Time;
         Difference    : Duration;
      begin
         Now        := Time_Of (1_983, 3, 1);
         Non_Leap   := Time_Of (1_983, 2, 28, 86_399.0);
         Difference := Now - Non_Leap;
         if Difference /= 1.0 then
            Failed ("DIFFERENCE OF TIMES IS INCORRECT - (A)5");
            -- COMMENT ("DIFFERENCE YIELDS: ");
            -- PUT (DIFFERENCE);
         end if;
      end;

      -- SUBTRACTION TESTS FOLLOW: TIME - DURATION.
      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 13, 0.0);
         New_Time := Now - Increment;
         if New_Time /= Time_Of (1_984, 8, 12, 86_399.0) then
            Failed
              ("DIFFERENCE OF TIME AND DURATION IS " & "INCORRECT - (A)6");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 1, 0.0);
         New_Time := Now - Increment;
         if New_Time /= Time_Of (1_984, 7, 31, 86_399.0) then
            Failed
              ("DIFFERENCE OF TIME AND DURATION IS " & "INCORRECT - (A)7");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 1, 0.0);
         New_Time := "-" (Left => Now, Right => Increment);
         if New_Time /= Time_Of (1_984, 7, 31, 86_399.0) then
            Failed
              ("DIFFERENCE OF TIME AND DURATION IS " & "INCORRECT - (A)8");
         end if;
      end;

      declare
         Now, New_Time : Time;
         Increment     : Duration := 1.0;
      begin
         Now      := Time_Of (1_984, 8, 1, 0.0);
         New_Time := "-" (Now, Increment);
         if New_Time /= Time_Of (1_984, 7, 31, 86_399.0) then
            Failed
              ("DIFFERENCE OF TIME AND DURATION IS " & "INCORRECT - (A)7");
         end if;
      end;

   end; -- (A)

   -----------------------------------------------

   Result;
end C96005a;
