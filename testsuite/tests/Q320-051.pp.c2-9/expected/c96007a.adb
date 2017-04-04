-- C96007A.ADA

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
-- CHECK THAT APPROPRIATE EXCEPTIONS ARE RAISED FOR THE TIME_OF() FUNCTION IN
-- THE PACKAGE CALENDAR. PARTICULARLY,
--   (A) TIME_ERROR IS RAISED ON INVALID DATES.
--   (B) CONSTRAINT_ERROR IS RAISED FOR OUT-OF-RANGE PARAMETERS.

-- CPP  8/16/84
-- RLB 12/18/06 - Changed so that the test will work for Ada 2005
--                implementations.

with Calendar; use Calendar;
with Report;   use Report;
procedure C96007a is

begin
   Test
     ("C96007A",
      "CHECK THAT APPROPRIATE EXCEPTIONS ARE RAISED " &
      "FOR THE TIME_OF FUNCTION IN THE PACKAGE CALENDAR");

   --------------------------------------------

   declare   -- (A)

      Bad_Time : Time;

   begin     -- (A)

      begin
         Bad_Time := Time_Of (1_984, 2, 30);
         Failed ("EXCEPTION NOT RAISED - 2/30 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2/30 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 2, 31);
         Failed ("EXCEPTION NOT RAISED - 2/31 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2/31 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 4, 31);
         Failed ("EXCEPTION NOT RAISED - 4/31 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 4/31 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 6, 31);
         Failed ("EXCEPTION NOT RAISED - 6/31 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 6/31 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 9, 31);
         Failed ("EXCEPTION NOT RAISED - 9/31 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 9/31 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 11, 31);
         Failed ("EXCEPTION NOT RAISED - 11/31 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 11/31 (A)");
      end;

      begin
         Bad_Time := Time_Of (1_983, 2, 29);
         Failed ("EXCEPTION NOT RAISED - 2/29 (A)");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2/29 (A)");
      end;

   end; -- (A)

   --------------------------------------------

   declare   -- (B)

      Bad_Time : Time;

   begin     -- (B)

      begin
         Bad_Time := Time_Of (1_900, 8, 13);
         Failed ("EXCEPTION NOT RAISED - 1900 (B)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1900 (B)");
      end;

      begin
         Bad_Time := Time_Of (Year_Number'Last + 1, 8, 13);
         Failed ("EXCEPTION NOT RAISED - YEAR_NUM'LAST+1 (B)");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - YEAR_NUM'LAST+1 (B)");
      end;

      begin
         Bad_Time := Time_Of (1_984, 0, 13);
         Failed ("EXCEPTION NOT RAISED - MONTH (B)1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - MONTH (B)1");
      end;

      begin
         Bad_Time := Time_Of (1_984, 13, 13);
         Failed ("EXCEPTION NOT RAISED - MONTH (B)2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - MONTH (B)2");
      end;

      begin
         Bad_Time := Time_Of (1_984, 8, 0);
         Failed ("EXCEPTION NOT RAISED - DAY (B)1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - DAY (B)1");
      end;

      begin
         Bad_Time := Time_Of (19_784, 8, 32);
         Failed ("EXCEPTION NOT RAISED - DAY (B)2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - DAY (B)2");
      end;

      begin
         Bad_Time := Time_Of (1_984, 8, 13, -0.5);
         Failed ("EXCEPTION NOT RAISED - SECONDS (B)1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - SECONDS (B)1");
      end;

   end; -- (B)

   --------------------------------------------

   Result;
end C96007a;
