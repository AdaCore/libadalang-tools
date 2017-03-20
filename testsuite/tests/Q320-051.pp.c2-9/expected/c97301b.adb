-- C97301B.ADA

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
--     CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED
--     AMOUNT OF TIME IF A RENDEZVOUS IS NOT POSSIBLE.

--     CASE  B:  THE QUEUE FOR THE CALLED ENTRY ALREADY CONTAINS
--           ANOTHER  TASK WHOSE RENDEZVOUS CANNOT BE COMPLETED WITHIN
--           THE SPECIFIED DELAY.

--HISTORY:
--     RJW 03/31/86 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report;   use Report;
with Calendar; use Calendar;
procedure C97301b is

   Or_Branch_Taken : Boolean := False;

begin

   Test
     ("C97301B",
      "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN THE " &
      "QUEUE FOR THE CALLED ENTRY ALREADY CONTAINS " &
      "ANOTHER TASK WHOSE RENDEZVOUS CANNOT BE " &
      "COMPLETED WITHIN THE SPECIFIED DELAY");

   declare
      Wait_Time : Duration := 3.0;

      task T1;

      task T2 is
         entry Awaken_T2;
      end T2;

      task T3 is
         entry Awaken_T3;
         entry Release_T;
      end T3;

      task T is
         entry Do_It_Now_Or_Wait (X : Integer);
      end T;

      task body T is
      begin
         accept Do_It_Now_Or_Wait (X : Integer) do
            if X = 1 then
               T2.Awaken_T2;
               while Do_It_Now_Or_Wait'Count = 0 loop
                  delay 1.0;
               end loop;
               T3.Awaken_T3;
               T3.Release_T;
            else
               Failed ("WRONG TASK IN RENDEZVOUS - 1");
            end if;
         end Do_It_Now_Or_Wait;
         accept Do_It_Now_Or_Wait (X : Integer) do
            if X /= 2 then
               Failed ("WRONG TASK IN RENDEZVOUS - 2");
            end if;
         end Do_It_Now_Or_Wait;
      end T;

      task body T1 is
      begin
         T.Do_It_Now_Or_Wait (1);
      end T1;

      task body T2 is
      begin
         accept Awaken_T2;
         T.Do_It_Now_Or_Wait (2);
      end T2;

      task body T3 is
         Start_Time : Time;
         Stop_Time  : Time;
      begin
         begin
            accept Awaken_T3;
            Start_Time := Clock;
            select
               T.Do_It_Now_Or_Wait (3);
            or
               -- THIS BRANCH MUST BE CHOSEN.
               delay Wait_Time;
               Stop_Time := Clock;
               if Stop_Time >= (Wait_Time + Start_Time) then
                  null;
               else
                  Failed ("INSUFFICIENT DELAY");
               end if;
               Or_Branch_Taken := True;
               Comment ("OR_BRANCH TAKEN");
               accept Release_T;
            end select;
         exception
            when Tasking_Error =>
               Failed ("TASKING ERROR");
         end;
         -- END OF BLOCK CONTAINING TIMED
         -- ENTRY CALL.

         -- BY NOW, THE TASK T IS EFFECTIVELY
         -- TERMINATED (AND THE NONLOCALS UPDATED).

         if Or_Branch_Taken then
            null;
         else
            Failed ("RENDEZVOUS ATTEMPTED");
         end if;
      end T3;
   begin
      null;
   end;

   Result;

end C97301b;
