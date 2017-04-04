-- C9A010A.ADA

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
-- TEST ABORT DURING RENDEZVOUS

-- ABORTING AN ABNORMAL (NOT YET TERMINATED) TASK.

-- JEAN-PIERRE ROSEN 09 MARCH 1984 JBG 6/1/84 JWC 6/28/85 RENAMED FROM
-- C9A009E-B.ADA PWN 01/31/95 REMOVED PRAGMA PRIORITY FOR ADA 9X.

with System; use System;
with Report; use Report;
procedure C9a010a is

begin

   Test ("C9A010A", "ABORTING AN ABNORMAL TASK");

   declare
      -- T1 CALLS T2. WHILE IN RENDEVOUS, T2 ABORTS T1 AND WAITS FOR A CALL
      -- FROM THE MAIN PROGRAM. WHEN THE CALL IS ACCEPTED, THE MAIN PROGRAM
      -- AGAIN ABORTS T1, WHICH IS NOW ABNORMAL, SINCE T1 HAS NOT YET COMPLETED
      -- ITS RENDEVOUS WITH T2.

      task T1 is
      end T1;

      task T2 is
         entry E1;
         entry E2;
      end T2;

      task body T1 is
      begin
         T2.E1;
         Failed ("T1 NOT ABORTED");
      exception
         when Tasking_Error =>
            Failed ("TASKING_ERROR IN T1");
         when others =>
            Failed ("OTHER EXCEPTION IN T1");
      end T1;

      task body T2 is
      begin
         accept E1 do
            abort T1;
            accept E2;     -- NOTE CALLER REMAINS IN RENDEVOUS
            accept E2;     -- UNTIL TWO ENTRY CALLS ACCEPTED
         end E1;
      end T2;
   begin
      T2.E2;    -- ONLY ACCEPTED AFTER T1 HAS BEEN ABORTED.
      abort T1; -- T1 IS ABNORMAL BECAUSE IT IS STILL IN RENDEVOUS.
      if T1'Callable then
         Failed ("T1 CALLABLE AFTER BEING ABORTED");
      end if;
      if T1'Terminated then
         Failed ("T1 TERMINATED ALTHOUGH IN RENDEVOUS");
      end if;
      T2.E2; -- T1'S RENDEVOUS CAN NOW COMPLETE; T1 CAN TERMINATE.
   end;

   Result;

end C9a010a;
