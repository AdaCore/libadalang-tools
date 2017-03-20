-- C96001A.ADA

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
--      CHECK THAT A DELAY STATEMENT DELAYS EXECUTION FOR AT LEAST THE
--      SPECIFIED TIME. SPECIFICALLY,
--        (A) POSITIVE DELAY ARGUMENT.
--        (B) NEGATIVE DELAY ARGUMENT.
--        (C) ZERO DELAY ARGUMENT.
--        (D) DURATION'SMALL DELAY ARGUMENT.
--        (E) EXPRESSION OF TYPE DURATION AS DELAY ARGUMENT.

-- HISTORY:
--     CPP  8/14/84  CREATED ORIGINAL TEST.
--     RJW 11/13/87  ADDED CODE WHICH ALLOWS TEST TO REPORT "PASSED"
--                   IF TICK > DURATION'SMALL.

with Calendar; use Calendar;
with System;   use System;
with Report;   use Report;
procedure C96001a is

   subtype Int is Integer range 0 .. 20_000;

begin
   Test
     ("C96001A",
      "CHECK THAT DELAY STATEMENT DELAYS " &
      "EXECUTION FOR AT LEAST THE SPECIFIED TIME");

   ---------------------------------------------

   declare   -- (A)
      X        : Duration := 5.0;
      Old_Time : Time;
      Lapse    : Duration;
   begin     -- (A)
      loop
         Old_Time := Clock;
         delay X;
         Lapse := Clock - Old_Time;
         exit;
      end loop;
      if Lapse < X then
         Failed ("DELAY DID NOT LAPSE AT LEAST 5.0 " & "SECONDS - (A)");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - (A)");
   end;

   ---------------------------------------------

   declare   -- (B)
      Old_Time : Time;
      Lapse    : Duration;
   begin     -- (B)
      loop
         Old_Time := Clock;
         delay -5.0;
         Lapse := Clock - Old_Time;
         exit;
      end loop;
      Comment
        ("(B) - NEGATIVE DELAY LAPSED FOR " &
         Int'Image (Int (Lapse * 1_000)) &
         " MILLISECONDS");
   exception
      when others =>
         Failed ("EXCEPTION RAISED - (B)");
   end;

   ---------------------------------------------

   declare   -- (C)
      X        : Duration := 0.0;
      Old_Time : Time;
      Lapse    : Duration;
   begin     -- (C)
      loop
         Old_Time := Clock;
         delay X;
         Lapse := Clock - Old_Time;
         exit;
      end loop;
      Comment
        ("(C) - ZERO DELAY LAPSED FOR " &
         Int'Image (Int (Lapse * 1_000)) &
         " MILLISECONDS");
   exception
      when others =>
         Failed ("EXCEPTION RAISED - (C)");
   end;

   ---------------------------------------------

   declare   -- (D)
      X        : Duration := Duration'Small;
      Old_Time : Time;
      Lapse    : Duration;
   begin     -- (D)
      loop
         Old_Time := Clock;
         delay X;
         Lapse := Clock - Old_Time;
         exit;
      end loop;
      if Lapse < X then
         if Tick < Duration'Small then
            Failed
              ("DELAY DID NOT LAPSE AT LEAST " &
               "DURATION'SMALL SECONDS - (D)");
         else
            Comment
              ("TICK > DURATION'SMALL SO DELAY IN " &
               "'(D)' IS NOT MEASURABLE");
         end if;
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - (D)");
   end;

   ---------------------------------------------

   declare   -- (E)
      Inc1     : Duration := 2.0;
      Inc2     : Duration := 3.0;
      Old_Time : Time;
      Lapse    : Duration;
   begin     -- (E)
      loop
         Old_Time := Clock;
         delay Inc1 + Inc2;
         Lapse := Clock - Old_Time;
         exit;
      end loop;
      if Lapse < (Inc1 + Inc2) then
         Failed
           ("DELAY DID NOT LAPSE AT LEAST " & "INC1 + INC2 SECONDS - (E)");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED - (E)");
   end;

   Result;
end C96001a;
