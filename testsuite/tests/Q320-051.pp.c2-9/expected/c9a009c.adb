-- C9A009C.ADA

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

-- THE CALLING TASK IN THE RENDEVOUS IS DEPENDENT ON THE ABORTED TASK,
-- SO THE DEPENDENT TASK IS INDIRECTLY ABORTED WHILE IN A RENDEVOUS;
-- NEITHER THE CALLING TASK NOR ITS MASTER CAN BE TERMINATED WHILE THE
-- RENDEVOUS CONTINUES.

-- JEAN-PIERRE ROSEN 09 MARCH 1984
-- JBG 6/1/84

with System; use System;
with Report; use Report;
procedure C9a009c is

begin

   Test ("C9A009C", "DEPENDENT TASK IN RENDEVOUS WHEN MASTER IS " & "ABORTED");

   declare
      -- T2 CONTAINS DEPENDENT TASK T3 WHICH CALLS T1.
      -- T1 ABORTS T2 WHILE IN RENDEVOUS WITH T3.

      task T1 is
         entry E1;
      end T1;

      task body T1 is

         task T2;

         task body T2 is
            task T3;
            task body T3 is
            begin
               T1.E1;
               Failed ("T3 NOT ABORTED");
            exception
               when Tasking_Error =>
                  Failed ("TASKING_ERROR IN T3");
               when others =>
                  Failed ("OTHER EXCEPTION IN T3");
            end T3;
         begin     -- T3 ACTIVATED NOW
            null;
         end T2;

      begin     -- T1
         accept E1 do
            abort T2;
            abort T2;
            abort T2; -- WHY NOT?
            if T2'Terminated then
               Failed ("T2 TERMINATED PREMATURELY");
            end if;
         end E1;
      exception
         when Tasking_Error =>
            Failed
              ("TASKING_ERROR IN T1 BECAUSE CALLING TASK " & "WAS ABORTED");
         when others =>
            Failed ("OTHER EXCEPTION - T1");
      end T1;

   begin
      null;
   end;

   Result;

end C9a009c;
