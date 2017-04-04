-- C97303C.ADA

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
-- CHECK THAT A TIMED ENTRY CALL CAN APPEAR IN PLACES WHERE A SELECTIVE WAIT IS
-- NOT ALLOWED.

-- PART 3: TASK BODY NESTED WITHIN A TASK.

-- WRG 7/15/86

with Report; use Report;
procedure C97303c is

begin

   Test
     ("C97303C",
      "CHECK THAT A TIMED ENTRY CALL CAN " &
      "APPEAR IN PLACES WHERE A SELECTIVE WAIT " &
      "IS NOT ALLOWED; CASE: TASK BODY NESTED " &
      "WITHIN A TASK");

   declare

      task T is
         entry E;
         entry Synch;
      end T;

      task body T is
      begin
         accept Synch;
         accept Synch;
         accept Synch;
         accept E;
      end T;

      task Outer is
         entry E;
         entry Synch;
      end Outer;

      task body Outer is

         task type Inner;

         Inner1 : Inner;

         task body Inner is
         begin
            select
               T.E;
               Failed ("TIMED ENTRY CALL ACCEPTED - " & "INNER (1)");
            or
               delay 1.0;
               T.Synch;
            end select;

            select
               Outer.E;
               Failed ("TIMED ENTRY CALL ACCEPTED - " & "INNER (2)");
            or
               delay 1.0;
               Outer.Synch;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED - INNER");
         end Inner;

         package Dummy is
            type Acc_Inner is access Inner;
            Inner2 : Acc_Inner := new Inner;
         end Dummy;

      begin

         select
            T.E;
            Failed ("TIMED ENTRY CALL ACCEPTED - OUTER");
         or
            delay 1.0;
            T.Synch;
         end select;

         accept Synch;
         accept Synch;
         accept E;

      exception

         when others =>
            Failed ("EXCEPTION RAISED - OUTER");

      end Outer;

   begin

      T.E;
      Outer.E;

   end;

   Result;

end C97303c;
