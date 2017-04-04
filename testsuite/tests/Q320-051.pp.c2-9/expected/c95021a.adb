-- C95021A.ADA

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
-- CHECK THAT CALLS TO AN ENTRY ARE PLACED IN A FIFO QUEUE.

-- JBG 2/22/84
-- DAS 10/8/90 ADDED PRAGMA PRIORITY TO ENSURE THAT THE FIFO
--              DISCIPLINE MUST BE FOLLOWED (OTHERWISE THE
--              IMPLEMENTATION MIGHT PROHIBIT QUEUES FROM
--              FORMING SO THAT E'COUNT IS ALWAYS ZERO FOR
--              AN ENTRY E).
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

-- THE TASK QUEUE IS THE TASK THAT CHECKS THE QUEUEING DISCIPLINE.
--
-- THIS TEST PLACES TWO CALLS ON AN ENTRY, WAITS UNTIL ONE OF THE CALLS IS
-- ACCEPTED, AND THEN PLACES A THIRD CALL ON THE ENTRY. THE TEST CHECKS THAT
-- THE SECOND CALL IS HANDLED BEFORE THE THIRD. (IT IS NONDETERMINISTIC WHICH
-- CALL WILL BE THE FIRST ONE ON THE QUEUE, SO THIS MORE COMPLICATED APPROACH
-- IS NECESSARY.)
--
-- THE TASK DISPATCH FIRES UP THE TWO TASKS THAT MAKE THE FIRST TWO CALLS AND
-- THEN WAITS UNTIL QUEUE SAYS IT IS READY FOR THE THIRD CALL.
--
-- THE TASK TYPE CALLERS IS USED TO CREATE TASKS THAT WILL CALL THE ENTRY IN
-- THE TASK QUEUE.

with Report; use Report;
with System;
procedure C95021a is
begin

   Test ("C95021A", "CHECK THAT ENTRY CALLS ARE PUT IN FIFO QUEUES");

-- DO THIS TEST 3 TIMES TO ALLOW FOR RANDOM VARIATIONS IN TIMING.
   for I in 1 .. 3 loop
      Comment ("ITERATION" & Integer'Image (I));

      declare

         task type Callers is
            entry Name (N : Natural);
         end Callers;

         task Queue is
            entry Go;
            entry E1 (Name : Natural);
         end Queue;

         task Dispatch is
            entry Ready;
         end Dispatch;

         task body Callers is
            My_Name : Natural;
         begin

-- GET NAME OF THIS TASK OBJECT
            accept Name (N : Natural) do
               My_Name := N;
            end Name;

-- PUT THIS TASK ON QUEUE FOR QUEUE.E1
            Queue.E1 (My_Name);
         end Callers;

         task body Dispatch is
            type Acc_Callers is access Callers;
            Obj : Acc_Callers;
         begin

-- FIRE UP TWO CALLERS FOR QUEUE.E1
            Obj := new Callers;
            Obj.Name (1);
            Obj := new Callers;
            Obj.Name (2);

-- ALLOW THESE CALLS TO BE PROCESSED (ONLY ONE WILL BE ACCEPTED).
            Queue.Go;

-- WAIT TILL ONE CALL HAS BEEN PROCESSED.
            accept Ready;       -- CALLED FROM QUEUE

-- FIRE UP THIRD CALLER
            Obj := new Callers;
            Obj.Name (3);

         end Dispatch;

         task body Queue is
            Next : Natural;     -- NUMBER OF SECOND CALLER IN QUEUE.
         begin

-- WAIT UNTIL TWO TASKS CALLING E1 HAVE BEEN ACTIVATED.
            accept Go;

-- WAIT FOR TWO CALLS TO BE AVAILABLE. THIS WAIT ASSUMES THAT THE CALLER TASKS
-- WILL PROCEED IF THIS TASK IS EXECUTING A DELAY STATEMENT, ALTHOUGH THIS IS
-- NOT STRICTLY REQUIRED BY THE STANDARD.
            for I in 1 .. 6       -- WILL WAIT FOR ONE MINUTE
            loop
               exit when E1'Count = 2;
               delay 10.0;    -- WAIT FOR CALLS TO ARRIVE
            end loop;

            if E1'Count /= 2 then
               Failed ("CALLER TASKS NOT QUEUED AFTER ONE " & "MINUTE - 1");
            end if;

-- ASSUMING NO FAILURE, PROCESS ONE OF THE QUEUED CALLS.
            accept E1 (Name : Natural) do

-- GET NAME OF NEXT CALLER
               case Name is
                  when 1 =>
                     Next := 2;
                  when 2 =>
                     Next := 1;
                  when others =>
                     Failed ("UNEXPECTED ERROR");
               end case;
            end E1;

-- TELL DISPATCH TO FIRE UP NEXT CALLER (ONE IS STILL IN QUEUE).
            Dispatch.Ready;

-- WAIT FOR CALL TO ARRIVE.
            for I in 1 .. 6       -- WILL WAIT FOR ONE MINUTE
            loop
               exit when E1'Count = 2;
               delay 10.0;    -- WAIT FOR CALLS TO ARRIVE
            end loop;

            if E1'Count /= 2 then
               Failed ("CALLER TASKS NOT QUEUED AFTER ONE " & "MINUTE - 2");
            end if;

-- ASSUMING NO FAILURE, ACCEPT SECOND CALL AND CHECK THAT IT IS FROM THE
-- CORRECT TASK.
            accept E1 (Name : Natural) do
               if Name /= Next then
                  Failed ("FIFO DISCIPLINE NOT OBEYED");
               end if;
            end E1;

-- ACCEPT THE LAST CALLER
            accept E1 (Name : Natural);

         end Queue;

      begin
         null;
      end;           -- ALL TASKS NOW TERMINATED.
   end loop;

   Result;

end C95021a;
