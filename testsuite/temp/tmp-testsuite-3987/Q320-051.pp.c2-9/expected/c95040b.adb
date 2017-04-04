-- C95040B.ADA

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
-- CHECK THAT THE EXCEPTION TASKING_ERROR IS RAISED BY A TASK IF THE
-- TASK BECOMES COMPLETED OR ABNORMAL BEFORE ACCEPTING THE CALL.

-- WEI  3/ 4/82
-- TLB 10/30/87  RENAMED FROM C950CHC.ADA.

with Report; use Report;
procedure C95040b is

   task T1 is
      entry E1;
   end T1;

   task body T1 is
   begin
      delay 1.0;
      if Equal (1, 1) then
         abort T1;
      end if;
      accept E1;
   end T1;

begin

   Test ("C95040B", "TASK COMPLETION BEFORE ACCEPTING AN ENTRY CALL");

   T1.E1;

   Failed ("NO EXCEPTION TASKING_ERROR RAISED");

   Result;

exception
   when Tasking_Error =>
      Result;

end C95040b;
