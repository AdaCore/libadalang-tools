-- C97304B.ADA

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
-- CHECK THAT TASKING_ERROR IS RAISED IF THE CALLED TASK IS ABORTED BEFORE THE
-- TIMED ENTRY CALL IS EXECUTED.

-- WRG 7/13/86

with Report; use Report;
procedure C97304b is

begin

   Test
     ("C97304B",
      "CHECK THAT TASKING_ERROR IS RAISED IF THE " &
      "CALLED TASK IS ABORTED BEFORE THE TIMED " &
      "ENTRY CALL IS EXECUTED");

   declare

      task T is
         entry E (I : Integer);
      end T;

      task body T is
      begin
         accept E (I : Integer);
         Failed ("ENTRY CALL ACCEPTED");
      exception
         when others =>
            Failed ("EXCEPTION RAISED");
      end T;

      function F return Integer is
      begin
         abort T;
         return 1;
      end F;

   begin

      select
         T.E (F);
         Failed ("TIMED ENTRY CALL MADE");
      or
         delay 1.0;
         Failed ("DELAY ALTERNATIVE TAKEN");
      end select;

      Failed ("EXCEPTION NOT RAISED");

   exception

      when Tasking_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");

   end;

   Result;

end C97304b;
