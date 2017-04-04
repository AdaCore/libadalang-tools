-- C96008B.ADA

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
-- MISCELLANEOUS CHECKS ON THE PRE-DEFINED FUNCTIONS IN THE PACKAGE
-- CALENDAR.  SUBTESTS ARE:
--   (A) THE FUNCTION TIME_OF() MUST ADVANCE DAY WHEN CALLED WITH THE
--       SECONDS ARGUMENT HAVING THE VALUE 86_400.

-- CPP 8/16/84
-- JRK 12/4/84

with Calendar; use Calendar;
with Report;   use Report;
procedure C96008b is

   Now1, Now2 : Time;
   Yr         : Year_Number;
   Mo         : Month_Number;
   Dy         : Day_Number;
   Sec        : Day_Duration;

begin

   Test ("C96008B", "CHECK THAT TIME_OF() ADVANCES DAY");

   Now1 := Time_Of (1_984, 8, 13, 86_400.0);
   Now2 := Time_Of (1_984, 8, 14, 0.0);

   if Now1 /= Now2 then
      Failed ("TIME_OF DID NOT CONVERT 86_400 SECONDS TO A DAY");
   end if;

   Split (Now2, Yr, Mo, Dy, Sec);

   if Dy /= 14 then
      Failed ("DAY OF NOW2 INCORRECT");
   end if;
   if Sec /= 0.0 then
      Failed ("SECONDS OF NOW2 INCORRECT");
   end if;

   Split (Now1, Yr, Mo, Dy, Sec);

   if Dy /= 14 or Sec /= 0.0 or Day (Now1) /= 14 or Seconds (Now1) /= 0.0 then
      Failed ("TIME_OF DID NOT ADVANCE DAY");
   end if;

   Result;
end C96008b;
