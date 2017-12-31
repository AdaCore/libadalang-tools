-- C96008A.ADA

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
-- MISCELLANEOUS CHECKS ON THE PRE-DEFINED FUNCTIONS IN THE PACKAGE CALENDAR.
-- SUBTESTS ARE:
--   (A) TIME_OF() AND SPLIT() ARE INVERSE FUNCTIONS.
--   (B) FORMAL PARAMETERS OF TIME_OF() AND SPLIT() ARE NAMED CORRECTLY.
--   (C) TIME_OF() GIVES THE PARAMETER SECONDS A DEFAULT VALUE OF 0.0.
--   (D) THE FUNCTIONS YEAR(), MONTH(), DAY(), AND SECONDS() RETURN
--       CORRECT VALUES USING NAMED NOTATION.
--   (E) A VALUE RETURNED FROM CLOCK() CAN BE PROCESSED BY SPLIT().
--   (F) DURATION'SMALL MEETS REQUIRED LIMIT.

-- CPP 8/16/84

with System;
with Calendar; use Calendar;
with Report;   use Report;
procedure C96008a is

begin
   Test
     ("C96008A", "CHECK MISCELLANEOUS FUNCTIONS IN THE " & "PACKAGE CALENDAR");

   ---------------------------------------------

   declare   -- (A)
      Now : Time;
      Yr  : Year_Number;
      Mo  : Month_Number;
      Dy  : Day_Number;
      Sec : Day_Duration;
   begin     -- (A)
      begin
         Now := Time_Of (1_984, 8, 13, Duration (1.0 / 3.0));
         Split (Now, Yr, Mo, Dy, Sec);
         if Now /= Time_Of (Yr, Mo, Dy, Sec) then
            Comment
              ("TIME_OF AND SPLIT ARE NOT INVERSES " &
               "WHEN SECONDS IS A NON-MODEL NUMBER " & "- (A)");
         end if;
      exception
         when others =>
            Failed ("TIME_OF(SPLIT) RAISED EXCEPTION - (A)");
      end;

      begin
         -- RESET VALUES.
         Yr  := 1_984;
         Mo  := 8;
         Dy  := 13;
         Sec := 1.0;

         Split (Time_Of (Yr, Mo, Dy, Sec), Yr, Mo, Dy, Sec);

         if Yr /= 1_984 then
            Failed ("SPLIT(TIME_OF) CHANGED VALUE OF YR - (A)");
         end if;

         if Mo /= 8 then
            Failed ("SPLIT(TIME_OF) CHANGED VALUE OF MO - (A)");
         end if;

         if Dy /= 13 then
            Failed ("SPLIT(TIME_OF) CHANGED VALUE OF DY - (A)");
         end if;

         if Sec /= 1.0 then
            Failed ("SPLIT(TIME_OF) CHANGED VALUE OF " & "SEC - (A)");
         end if;
      exception
         when others =>
            Failed ("SPLIT(TIME_OF) PROCESSING RAISED " & "EXCEPTION - (A)");
      end;
   end; -- (A)

   ---------------------------------------------

   begin     -- (B)
      declare
         Now : Time;
      begin
         Now :=
           Time_Of (Year => 1_984, Month => 8, Day => 13, Seconds => 60.0);
      exception
         when others =>
            Failed
              ("NAMED ASSOCIATION ON TIME_OF() RAISED " & "EXCEPTION - (B)");
      end;

      declare
         Now : Time         := Clock;
         Yr  : Year_Number  := 1_984;
         Mo  : Month_Number := 8;
         Dy  : Day_Number   := 13;
         Sec : Day_Duration := 0.0;
      begin
         Split
           (Date => Now, Year => Yr, Month => Mo, Day => Dy, Seconds => Sec);
      exception
         when others =>
            Failed
              ("NAMED ASSOCIATION ON SPLIT() RAISED " & "EXCEPTION - (B)2");
      end;
   end; -- (B)

   ---------------------------------------------

   declare   -- (C)
      Now : Time;
   begin     -- (C)
      Now := Time_Of (1_984, 8, 13);
      if Seconds (Now) /= 0.0 then
         Failed ("TIME_OF() DID NOT ZERO SECONDS - (C)");
      end if;
   end; -- (C)

   ---------------------------------------------

   declare   -- (D)
      -- ASSUMES TIME_OF() WORKS CORRECTLY.
      Holiday : Time;
   begin     -- (D)
      Holiday := Time_Of (1_958, 9, 9, 1.0);

      if Year (Date => Holiday) /= 1_958 then
         Failed ("YEAR() DID NOT RETURN CORRECT VALUE - (D)");
      end if;

      if Month (Date => Holiday) /= 9 then
         Failed ("MONTH() DID NOT RETURN CORRECT VALUE - (D)");
      end if;

      if Day (Date => Holiday) /= 9 then
         Failed ("DAY() DID NOT RETURN CORRECT VALUE - (D)");
      end if;

      if Seconds (Holiday) /= 1.0 then
         Failed ("SECONDS() DID NOT RETURN CORRECT VALUE - (D)");
      end if;
   end; -- (D)

   ---------------------------------------------

   declare   -- (E)
      Yr  : Year_Number;
      Mo  : Month_Number;
      Dy  : Day_Number;
      Sec : Day_Duration;
   begin     -- (E)
      Split (Clock, Yr, Mo, Dy, Sec);
      delay System.Tick;

      if Time_Of (Yr, Mo, Dy, Sec) > Clock then
         Failed ("SPLIT() ON CLOCK INCORRECT - (E)");
      end if;
   exception
      when others =>
         Failed ("SPLIT() ON CLOCK RAISED EXCEPTION - (E)");
   end; -- (E)

   ---------------------------------------------

   begin     -- (F)
      if Duration'Small > 0.020 then
         Failed ("DURATION'SMALL LARGER THAN SPECIFIED - (F)");
      end if;
   end; -- (F)

   ---------------------------------------------

   Result;
end C96008a;
