-- C9A011B.ADA

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
--     CHECK THAT "TASKING_ERROR" IS RAISED BY A TIMED ENTRY CALL IF
--     THE CALLED TASK IS ABORTED BEFORE THE DELAY EXPIRES BUT NOT
--     WHEN THE CALL IS FIRST EXECUTED.

-- HISTORY:
--     DHH 06/14/88 CREATED ORIGINAL TEST.

with System; use System;
with Report; use Report;
procedure C9a011b is

   task Timed_Entry is
      entry Wait_Around;
   end Timed_Entry;

   task Owner is
      entry Start;
      entry Self_Abort;
   end Owner;

   task body Timed_Entry is
   begin
      select
         Owner.Self_Abort;
      or
         delay 60.0;
      end select;
      Failed ("NO EXCEPTION RAISED");

      accept Wait_Around;
   exception
      when Tasking_Error =>
         accept Wait_Around;
      when others =>
         Failed ("WRONG EXCEPTION RAISED");
         accept Wait_Around;
   end Timed_Entry;

   task body Owner is
   begin
      accept Start do
         while Self_Abort'Count = 0 loop
            delay 1.0;
         end loop;
      end Start;

      abort Owner;

      accept Self_Abort;

   end Owner;

begin

   Test
     ("C9A011B",
      "CHECK THAT ""TASKING_ERROR"" IS RAISED BY A " &
      "TIMED ENTRY CALL IF THE CALLED TASK IS " &
      "ABORTED BEFORE THE DELAY EXPIRES BUT NOT " &
      "WHEN THE CALL IS FIRST EXECUTED");

   Owner.Start;
   delay 5.0;

   if Timed_Entry'Callable then
      Timed_Entry.Wait_Around;
   else
      Failed ("TASK ABORTED WHEN TASKING ERROR IS RAISED");
   end if;

   Result;

exception
   when others =>
      Failed ("EXCEPTION RAISED OUTSIDE OF TASK");
      Result;

end C9a011b;
