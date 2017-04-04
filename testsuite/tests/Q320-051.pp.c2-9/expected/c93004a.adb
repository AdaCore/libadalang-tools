-- C93004A.ADA

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
-- CHECK THAT A TASK BECOMES COMPLETED WHEN AN EXCEPTION OCCURS DURING ITS
-- ACTIVATION.

-- WEI  3/ 4/82

with Report; use Report;
procedure C93004a is
begin

   Test ("C93004A", "TASK COMPLETION CAUSED BY EXCEPTION");

   Block : declare
      type I0 is range 0 .. 1;

      task T1 is
         entry Bye;
      end T1;

      task body T1 is
         subtype I1 is I0 range 0 .. 2;     -- CONSTRAINT ERROR.
      begin
         accept Bye;
      end T1;
   begin
      Failed ("NO EXCEPTION RAISED");
      if not T1'Terminated then
         Failed ("TASK NOT TERMINATED");
         T1.Bye;
      end if;
   exception
      when Tasking_Error =>
         null;
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED");
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end Block;

   Result;

end C93004a;
