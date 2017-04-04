-- C91004B.ADA

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
--     CHECK THAT A TASK (TYPE) IDENTIFIER, WHEN USED WITHIN ITS OWN
--     BODY, REFERS TO THE EXECUTING TASK.

--      TEST USING IDENTIFIER IN ABORT STATEMENT, AS AN EXPRESSION IN
--      A MEMBERSHIP TEST, AND THE PREFIX OF 'CALLABLE AND
--      'TERMINATED.

-- HISTORY:
--     WEI  3/ 4/82  CREATED ORIGINAL TEST.
--     RJW 11/13/87  RENAMED TEST FROM C910BDA.ADA.  ADDED CHECKS FOR
--                   MEMBERSHIP TEST, AND 'CALLABLE AND 'TERMINATED
--                   ATTRIBUTES.

with Report; use Report;
procedure C91004b is

   type I0 is range 0 .. 1;
   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   task type Tt1 is
      entry E1 (P1 : in I0; P2 : Arg);
      entry Bye;
   end Tt1;

   subtype Sub_Tt1 is Tt1;

   Obj_Tt1 : array (Natural range 1 .. 2) of Tt1;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

   task body Tt1 is
   begin
      if Tt1 not in Sub_Tt1 then
         Failed ("INCORRECT RESULTS FOR MEMBERSHIP TEST");
      end if;

      if not Tt1'Callable then
         Failed ("INCORRECT RESULTS FOR 'CALLABLE");
      end if;

      if Tt1'Terminated then
         Failed ("INCORRECT RESULTS FOR 'TERMINATED");
      end if;

      accept E1 (P1 : in I0; P2 : Arg) do
         if P1 = 1 then
            abort Tt1;
            accept Bye;    -- WILL DEADLOCK IF NOT ABORTED.
         end if;
         Pspy_Numb (Arg (P2));
      end E1;

   end Tt1;

begin

   Test ("C91004B", "TASK IDENTIFIER IN OWN BODY");

   begin
      Obj_Tt1 (1).E1 (1, 1);
      Failed ("NO TASKING_ERROR RAISED");
-- ABORT DURING RENDEVOUS RAISES TASKING ERROR
   exception
      when Tasking_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end;

   Obj_Tt1 (2).E1 (0, 2);

   if Spynumb /= 2 then
      Failed ("WRONG TASK OBJECT REFERENCED");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   Result;

end C91004b;
