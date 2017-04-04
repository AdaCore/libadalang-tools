-- C9A011A.ADA

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
--     CHECK THAT IF A CALLED TASK IS ABORTED WHILE IN RENDEZVOUS, THEN
--     "TASKING_ERROR" IS RAISED IN THE CALLING TASK.

-- HISTORY:
--     DHH 03/28/88 CREATED ORIGINAL TEST.

with System; use System;
with Report; use Report;
procedure C9a011a is

   task type Choice is
      entry E1;
   end Choice;

   T : Choice;

   task body Choice is
      X : Integer;
   begin
      accept E1 do
         X := Ident_Int (3);
         if Equal (X, X) then
            abort Choice;
         end if;
      end E1;
   end Choice;

begin

   Test
     ("C9A011A",
      "CHECK THAT IF A CALLED TASK IS ABORTED WHILE " &
      "IN RENDEZVOUS, THEN ""TASKING_ERROR"" IS " &
      "RAISED IN THE CALLING TASK");

   T.E1;
   Failed ("EXCEPTION NOT RAISED ON ABORT");

   Result;

exception
   when Tasking_Error =>
      Result;

   when others =>
      Failed ("UNEXPECTED EXCEPTION RAISED ON ABORT");
      Result;
end C9a011a;
