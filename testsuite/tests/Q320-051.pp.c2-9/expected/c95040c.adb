-- C95040C.ADA

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
-- CHECKS THAT A TASK COMPLETED, BUT NOT TERMINATED (I.E. WAITING FOR
-- TERMINATION OF A DEPENDENT TASK) IS NEITHER 'TERMINATED NOR 'CALLABLE.
-- CALLS TO ENTRIES BELONGING TO SUCH A TASK RAISE TASKING_ERROR.

-- J.P. ROSEN, ADA PROJECT, NYU JBG 6/1/84 JWC 6/28/85 RENAMED FROM
-- C9A009A-B.ADA PWN 9/11/94 REMOVED PRAGMA PRIORITY FOR ADA 9X

with Report; use Report;
with System; use System;
procedure C95040c is
begin

   Test
     ("C95040C",
      "TASKING_ERROR RAISED WHEN CALLING COMPLETED " &
      "BUT UNTERMINATED TASK");

   declare

      task T1 is
         entry E;
      end T1;

      task body T1 is

         task T2 is
         end T2;

         task body T2 is
         begin
            Comment ("BEGIN T2");
            T1.E;          -- T1 WILL COMPLETE BEFORE THIS CALL
            -- OR WHILE WAITING FOR THIS CALL TO BE ACCEPTED. WILL DEADLOCK IF
            -- TASKING_ERROR IS NOT RAISED.
            Failed ("NO TASKING_ERROR RAISED");
         exception
            when Tasking_Error =>
               if T1'Callable then
                  Failed ("T1 STILL CALLABLE");
               end if;

               if T1'Terminated then    -- T1 CAN'T TERMINATE
                  -- UNTIL T2 HAS
                  -- TERMINATED.
                  Failed ("T1 TERMINATED");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION");
         end T2;
      begin
         null;
      end T1;

   begin
      null;
   end;

   Result;

end C95040c;
