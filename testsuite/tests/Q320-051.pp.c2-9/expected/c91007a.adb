-- C91007A.ADA

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
--     IF THE ELABORATION OF AN ENTRY DECLARATION RAISES
--     "CONSTRAINT_ERROR", THEN NO TASKS ARE ACTIVATED, AND
--     "TASKING_ERROR" IS NOT RAISED.

-- HISTORY:
--     LDC 06/17/88  CREATED ORGINAL TEST

with Report; use Report;

procedure C91007a is

   type Enum is
     (Teresa, Brian, Phil, Joleen, Lynn, Doug, Jodie, Vince, Tom, Dave, John,
      Rosa);
   subtype Enum_Sub is Enum range Brian .. Lynn;

begin
   Test
     ("C91007A",
      "IF THE ELABORATION OF AN ENTRY DECLARATION " &
      "RAISES 'CONSTRAINT_ERROR', THEN NO TASKS ARE " &
      "ACTIVATED, AND 'TASKING_ERROR' IS NOT RAISED");

   begin
      declare
         task type Tsk1;
         T1 : Tsk1;
         task body Tsk1 is
         begin
            Failed ("TSK1 WAS ACTIVATED");
         end Tsk1;

         task Tsk2 is
            entry Ent (Enum_Sub range Teresa .. Lynn);
         end Tsk2;

         task body Tsk2 is
         begin
            Failed ("TASK BODY WAS ACTIVATED");
         end Tsk2;

         task Tsk3;
         task body Tsk3 is
         begin
            Failed ("TSK3 WAS ACTIVATED");
         end Tsk3;

      begin
         null;
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR WAS RAISED IN THE " & "BEGIN BLOCK");
         when Tasking_Error =>
            Failed
              ("TASKING_ERROR WAS RAISED INSTEAD OF " &
               "CONSTRAINT_ERROR IN THE BEGIN BLOCK");
         when others =>
            Failed ("OTHER EXCEPTION WAS RAISED IN " & "THE BEGIN BLOCK");
      end;
   exception
      when Constraint_Error =>
         null;
      when Tasking_Error =>
         Failed ("TASKING_ERROR WAS RAISED INSTEAD OF " & "CONSTRAINT_ERROR");
      when others =>
         Failed ("WRONG EXCEPTION WAS RAISED");
   end;

   Result;

end C91007a;
