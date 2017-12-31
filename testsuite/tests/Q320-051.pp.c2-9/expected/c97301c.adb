-- C97301C.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED AMOUNT OF
-- TIME IF A RENDEVOUS IS NOT POSSIBLE.

-- CASE C: AN ACCEPT STATEMENT FOR THE CALLED ENTRY HAS NOT BEEN
--           REACHED.

-- RJW 3/31/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97301c is

   Or_Branch_Taken : Boolean := False;

begin

   Test
     ("C97301C",
      "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN AN " &
      "ACCEPT STATEMENT FOR THE CALLED ENTRY HAS " & "NOT BEEN REACHED");

   declare
      Start_Time : Time;
      Stop_Time  : Time;
      Wait_Time  : Duration := 3.0;

      task T is
         entry No_Spin;
         entry Do_It_Now_Or_Wait;
      end T;

      task body T is
      begin
         accept No_Spin;
         accept Do_It_Now_Or_Wait;
      end T;

   begin
      Start_Time := Clock;
      select
         T.Do_It_Now_Or_Wait;
         Failed ("RENDEZVOUS OCCURRED");
         abort T;
      or
         -- THIS BRANCH MUST BE CHOSEN.
         delay Wait_Time;
         Stop_Time := Clock;
         if Stop_Time >= (Wait_Time + Start_Time) then
            null;
         else
            Failed ("INSUFFICIENT DELAY");
         end if;
         T.No_Spin;
         Or_Branch_Taken := True;
         Comment ("OR_BRANCH TAKEN");
         T.Do_It_Now_Or_Wait;
      end select;
   exception
      when Tasking_Error =>
         Failed ("TASKING ERROR");
   end;
   -- END OF BLOCK CONTAINING TIMED ENTRY CALL.

   -- BY NOW, TASK T IS TERMINATED (AND THE NONLOCALS UPDATED).

   if Or_Branch_Taken then
      null;
   else
      Failed ("RENDEZVOUS ATTEMPTED");
   end if;

   Result;

end C97301c;
