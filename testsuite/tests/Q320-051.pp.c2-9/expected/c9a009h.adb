-- C9A009H.ADA

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
-- CHECK THAT A TASK ABORTED DURING A RENDEVOUS IS NEITHER CALLABLE NOR
-- TERMINATED BEFORE THE END OF THE RENDEVOUS.

-- J.P ROSEN, ADA PROJECT, NYU JBG 6/1/84

with Report; use Report;
procedure C9a009h is
begin
   Test
     ("C9A009H",
      "TASK ABORTED IN RENDEVOUS IS NOT CALLABLE OR " & "TERMINATED");

   declare

      task T1 is
         entry E1;
      end T1;

      task T2 is
      end T2;

      task body T2 is
      begin
         T1.E1;
         Failed ("T2 NOT ABORTED");
      exception
         when Tasking_Error =>
            Failed ("TASKING_ERROR RAISED IN ABORTED TASK");
         when others =>
            Failed ("OTHER EXCEPTION RAISED");
      end T2;

      task body T1 is
      begin
         accept E1 do
            abort T2;
            if T2'Callable then
               Failed ("T2 STILL CALLABLE");
            end if;

            if T2'Terminated then
               Failed ("T2 TERMINATED");
            end if;
         end E1;
      end T1;

   begin
      null;
   end;

   Result;

end C9a009h;
