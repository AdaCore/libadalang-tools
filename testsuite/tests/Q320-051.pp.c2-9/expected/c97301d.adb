-- C97301D.ADA

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

-- CASE D: THE BODY OF THE TASK CONTAINING THE CALLED ENTRY
--           DOES NOT CONTAIN AN ACCEPT_STATEMENT FOR THAT ENTRY.

-- RJW 3/31/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97301d is

   Or_Branch_Taken : Boolean := False;

begin

   Test
     ("C97301D",
      "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN THE " &
      "BODY OF THE TASK CONTAINING THE CALLED ENTRY " &
      "DOES NOT CONTAIN AN ACCEPT_STATEMENT FOR " &
      "THAT ENTRY");

   declare
      Start_Time : Time;
      Wait_Time  : constant Duration := 10.0;

      task T is
         entry Do_It_Now_Or_Wait;
         entry Keep_Alive;
      end T;

      task body T is
      begin

         -- NO ACCEPT_STATEMENT FOR THE ENTRY_CALL BEING TESTED.

         accept Keep_Alive;  -- TO PREVENT THIS SERVER TASK FROM
         --     TERMINATING IF
         --     UPON ACTIVATION
         --     IT GETS TO RUN
         --     AHEAD OF THE CALLER (WHICH
         --     WOULD LEAD TO A SUBSEQUENT
         --     TASKING_ERROR AT THE TIME
         --     OF THE NO-WAIT CALL).

      end T;

   begin
      Start_Time := Clock;
      select
         T.Do_It_Now_Or_Wait;
      or
         --  THIS BRANCH MUST BE CHOSEN.
         delay Wait_Time;
         if Clock >= (Wait_Time + Start_Time) then
            null;
         else
            Failed ("INSUFFICIENT WAITING TIME");
         end if;
         Or_Branch_Taken := True;
         Comment ("OR_BRANCH  TAKEN");
      end select;

      T.Keep_Alive;
   exception
      when Tasking_Error =>
         Failed ("TASKING ERROR RAISED");

   end;   -- END OF BLOCK CONTAINING THE ENTRY CALL.

   -- BY NOW, THE TASK IS TERMINATED.

   if Or_Branch_Taken then
      null;
   else
      Failed ("RENDEZVOUS ATTEMPTED?");
   end if;

   Result;

end C97301d;
