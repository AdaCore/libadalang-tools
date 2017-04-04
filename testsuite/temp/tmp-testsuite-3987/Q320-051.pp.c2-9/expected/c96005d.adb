-- C96005D.ADA

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
--   (D) THE EXCEPTION TIME_ERROR IS RAISED WHEN THE FUNCTION "-"
--       RETURNS A VALUE NOT IN THE SUBTYPE RANGE DURATION.

-- CPP 8/16/84

with Calendar; use Calendar;
with Report;   use Report;
procedure C96005d is

begin
   Test
     ("C96005D",
      "CHECK THAT THE SUBTRACTION OPERATOR RAISES " &
      "TIME_ERROR APPROPRIATELY");

   ---------------------------------------------

   begin     -- (D)

      declare
         Now, Later : Time;
         Wait       : Duration;
      begin
         Now   := Time_Of (1_984, 8, 13, 0.0);
         Later := (Now + Duration'Last) + 1.0;
         Wait  := Later - Now;
         Failed ("EXCEPTION NOT RAISED - (D)1");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (D)1");
      end;

      declare
         Now, Later : Time;
         Wait       : Duration;
      begin
         Now   := Time_Of (1_984, 8, 13, 0.0);
         Later := (Now + Duration'First) - 1.0;
         Wait  := Now - Later;
         Failed ("EXCEPTION NOT RAISED - (D)2");
      exception
         when Time_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - (D)2");
      end;

   end; -- (D)

   ---------------------------------------------

   Result;
end C96005d;
