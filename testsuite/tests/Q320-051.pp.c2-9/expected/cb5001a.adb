-- CB5001A.ADA

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
-- CHECK THAT AN EXCEPTION RAISED IN A RENDEVOUS IS PROPAGATED BOTH TO THE
-- CALLER AND TO THE CALLED TASK.

-- THIS VERSION CHECKS THAT THE EXCEPTION IS PROPAGATED THROUGH ONE LEVEL OF
-- RENDEVOUS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- JEAN-PIERRE ROSEN 09 MARCH 1984 JBG 6/1/84 MRM 03/30/93 REMOVED
-- NUMERIC_ERROR FOR 9X COMPATIBILITY

with System; use System;
with Report; use Report;
procedure Cb5001a is

begin

   Test
     ("CB5001A",
      "CHECK THAT AN EXCEPTION IN A RENDEVOUS IS " &
      "PROPAGATED TO CALLER AND CALLED TASKS -- ONE " &
      "LEVEL");

   declare
      task T2 is
         entry E2;
      end T2;

      task body T2 is
         My_Exception : exception;
      begin
         accept E2 do
            if Equal (1, 1) then
               raise My_Exception;
            end if;
         end E2;
         Failed ("T2: EXCEPTION NOT RAISED");
      exception
         when My_Exception =>
            null;
         when Tasking_Error =>
            Failed ("TASKING_ERROR RAISED IN T2");
         when others =>
            Failed ("T2 RECEIVED ABNORMAL EXCEPTION");
      end T2;

   begin
      T2.E2;
      Failed ("MAIN: EXCEPTION NOT RAISED");
   exception
      when Constraint_Error | Program_Error | Storage_Error =>
         Failed ("PREDEFINED ERROR RAISED IN MAIN");
      when Tasking_Error =>
         Failed ("TASKING_ERROR RAISED IN MAIN");
      when others =>
         null;
   end;

   Result;

end Cb5001a;
