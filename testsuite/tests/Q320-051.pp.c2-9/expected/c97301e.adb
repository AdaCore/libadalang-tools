-- C97301E.ADA

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
-- TIME IF A RENDEZVOUS IS NOT POSSIBLE.

-- CASE E: THE BODY OF THE TASK CONTAINING THE CALLED ENTRY
--           DOES NOT CONTAIN AN ACCEPT_STATEMENT FOR THAT ENTRY  -
--           (THE ENTRY BELONGS TO AN ENTRY FAMILY; SOME FAMILY MEMBERS
--           ARE "ACCEPTABLE", BUT NOT THE CALLED ONE.)

-- RJW 3/31/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97301e is

   Or_Branch_Taken : Boolean := False;

begin

   Test
     ("C97301E",
      "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
      "LEAST THE SPECIFIED AMOUNT OF TIME " &
      "IN THE ABSENCE OF A CORRESPONDING " &
      "ACCEPT_STATEMENT ");

   declare

      Wait_Time : Duration := 3.0;

      Start_Time : Time;

      Stop_Time : Time;

      subtype Short is Integer range 10 .. 20;

      Keep_Alive : Integer := 15;

      task T is
         entry Do_It_Now_Or_Wait (Short);
      end T;

      task body T is
      begin

         -- NO ACCEPT_STATEMENT FOR THE ENTRY_CALL BEING TESTED.
         accept Do_It_Now_Or_Wait (Ident_Int (15));

         -- THIS ALSO PREVENTS THIS SERVER
         --     TASK FROM TERMINATING IF
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
         T.Do_It_Now_Or_Wait (10);  -- ACCEPT_STATEMENT HAS 15.
      or
         --   THEREFORE THIS BRANCH MUST BE CHOSEN.
         delay Wait_Time;
         Stop_Time := Clock;
         if Stop_Time >= (Wait_Time + Start_Time) then
            null;
         else
            Failed ("INSUFFICIENT DELAY");
         end if;
         Or_Branch_Taken := True;
         Comment ("OR_BRANCH TAKEN");
      end select;

      T.Do_It_Now_Or_Wait (Keep_Alive);

   exception
      when Tasking_Error =>
         Failed ("TASKING ERROR");

   end;   -- END OF BLOCK CONTAINING THE TIMED ENTRY CALL.

   -- BY NOW, TASK T IS TERMINATED.

   if Or_Branch_Taken then
      null;
   else
      Failed ("RENDEZVOUS ATTEMPTED");
   end if;

   Result;

end C97301e;
