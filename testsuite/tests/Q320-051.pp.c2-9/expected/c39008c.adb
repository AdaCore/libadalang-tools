-- C39008C.ADA

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
-- OBJECTIVE:
--     CHECK THAT PROGRAM_ERROR IS RAISED WHEN AN ATTEMPT IS MADE TO
--     ACTIVATE A TASK BEFORE ITS BODY HAS BEEN ELABORATED.  CHECK THE
--     CASE IN WHICH SEVERAL TASKS ARE TO BE ACTIVATED, AND ONLY SOME
--     HAVE UNELABORATED BODIES; NO TASKS SHOULD BE ACTIVATED.

-- HISTORY:
--     BCB 07/08/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C39008c is

begin
   Test
     ("C39008C",
      "CHECK THAT PROGRAM_ERROR IS RAISED WHEN AN " &
      "ATTEMPT IS MADE TO ACTIVATE A TASK BEFORE ITS " &
      "BODY HAS BEEN ELABORATED.  CHECK THE CASE IN " &
      "WHICH SEVERAL TASKS ARE TO BE ACTIVATED, AND " &
      "ONLY SOME HAVE UNELABORATED BODIES; NO TASKS " & "SHOULD BE ACTIVATED");

   begin
      declare
         task type A;

         task type B;

         task type C;

         task type D;

         package P is
            W : A;
            X : B;
            Y : C;
            Z : D;
         end P;

         task body A is
         begin
            Failed ("TASK A ACTIVATED");
         end A;

         task body D is
         begin
            Failed ("TASK D ACTIVATED");
         end D;

         package body P is
         end P;

         task body B is
         begin
            Failed ("TASK B ACTIVATED");
         end B;

         task body C is
         begin
            Failed ("TASK C ACTIVATED");
         end C;
      begin
         Failed ("PROGRAM_ERROR WAS NOT RAISED");
      end;
   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("AN EXCEPTION OTHER THAN PROGRAM_ERROR WAS " & "RAISED");
   end;

   Result;
end C39008c;
