-- C97301A.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED
-- AMOUNT OF TIME IF A RENDEVOUS IS NOT POSSIBLE.

-- CASE  A:  THE TASK TO BE CALLED HAS NOT YET BEEN ACTIVATED AS OF THE
--           MOMENT OF CALL.

-- RJW 3/31/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97301a is

   Wait_Time       : constant Duration := 10.0;
   Or_Branch_Taken : Integer           := 3;

begin

   Test
     ("C97301A",
      "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN THE " &
      "CALLED TASK IS NOT ACTIVE");

   ------------------------------------------------------------------

   declare

      task T is
         entry Do_It_Now_Or_Wait (Authorized : in Boolean);
      end T;

      task body T is

         package Second_Attempt is
         end Second_Attempt;
         package body Second_Attempt is
            Start_Time : Time;
         begin
            Start_Time := Clock;
            select
               Do_It_Now_Or_Wait (False); --CALLING OWN ENTRY.
            or
               -- THEREFORE THIS BRANCH
               --    MUST BE CHOSEN.
               delay Wait_Time;
               if Clock >= (Wait_Time + Start_Time) then
                  null;
               else
                  Failed ("INSUFFICIENT DELAY (#2)");
               end if;
               Or_Branch_Taken := 2 * Or_Branch_Taken;
               Comment ("OR_BRANCH  TAKEN  (#2)");
            end select;
         end Second_Attempt;

      begin

         accept Do_It_Now_Or_Wait (Authorized : in Boolean) do

            if Authorized then
               Comment ("AUTHORIZED ENTRY_CALL");
            else
               Failed ("UNAUTHORIZED ENTRY_CALL");
            end if;

         end Do_It_Now_Or_Wait;

      end T;

      package First_Attempt is
      end First_Attempt;
      package body First_Attempt is
         Start_Time : Time;
      begin
         Start_Time := Clock;
         select
            T.Do_It_Now_Or_Wait (False);
         or
            -- THIS BRANCH MUST BE CHOSEN.
            delay Wait_Time;
            if Clock >= (Wait_Time + Start_Time) then
               null;
            else
               Failed ("INSUFFICIENT DELAY (#1)");
            end if;
            Or_Branch_Taken := 1 + Or_Branch_Taken;
            Comment ("OR_BRANCH  TAKEN  (#1)");
         end select;

      end First_Attempt;

   begin

      T.Do_It_Now_Or_Wait (True);   -- TO SATISFY THE SERVER'S
      --     WAIT FOR SUCH A CALL.

   exception

      when Tasking_Error =>
         Failed ("TASKING ERROR");

   end;

   ------------------------------------------------------------------

   -- BY NOW, THE TASK IS TERMINATED  (AND THE NONLOCALS UPDATED).

   case Or_Branch_Taken is

      when 3 =>
         Failed ("NO 'OR'; BOTH (?) RENDEZVOUS ATTEMPTED?");

      when 4 =>
         Failed ("'OR' #1 ONLY; RENDEZVOUS (#2) ATTEMPTED?");

      when 6 =>
         Failed ("'OR' #2 ONLY; RENDEZVOUS (#1) ATTEMPTED?");

      when 7 =>
         Failed ("WRONG ORDER FOR 'OR':  #2,#1");

      when 8 =>
         null;

      when others =>
         Failed ("WRONG CASE_VALUE");

   end case;

   Result;

end C97301a;
