-- C94002D.ADA

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
-- CHECK THAT A TASK DOES N O T DEPEND ON A UNIT IF IT IS DESIGNATED BY A LOCAL
-- ACCESS VARIABLE (OF THIS UNIT) WHOSE TYPE IS DECLARED OUTSIDE THIS UNIT.

-- WEI  3/ 4/82
-- JBG 2/20/84
-- TBN 11/25/85 RENAMED FROM C940ACB-B.ADA.

with Report; use Report;
procedure C94002d is

   task type Tt1 is
      entry E1;
      entry E2;
   end Tt1;

   type Att1 is access Tt1;
   Outer_Tt1 : Att1;

   task body Tt1 is
   begin
      accept E1;
      accept E2;
   end Tt1;

begin
   Test
     ("C94002D",
      "DEPENDENCY IS INDEPENDENT OF WHERE ACCESS " & "VARIABLE IS DECLARED");

   Block1 : declare
      Pointer_Tt1 : Att1 := new Tt1;
   begin
      Outer_Tt1 := Pointer_Tt1;
      Pointer_Tt1.all.E1;
   end Block1;         -- MAY DEADLOCK HERE IF INCORRECT DEPENDENCY
   -- RULE IS IMPLEMENTED.

   if Outer_Tt1.all'Terminated then
      Failed
        ("NON-DEPENDENT TASK IS TERMINATED " &
         "IMMEDIATELY AFTER ENCLOSING UNIT HAS " &
         "BEEN COMPLETED");
   end if;

   Outer_Tt1.E2;       -- RELEASE TASK

   Result;

end C94002d;
