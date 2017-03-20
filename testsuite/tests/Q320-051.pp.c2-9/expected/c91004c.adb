-- C91004C.ADA

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
-- CHECK THAT A TASK (TYPE) IDENTIFIER, WHEN USED WITHIN ITS OWN BODY
-- REFERS TO THE EXECUTING TASK.
--
-- TEST USING CONDITIONAL ENTRY CALL.

-- WEI  3/ 4/82
-- TLB 10/30/87  RENAMED FROM C910BDB.ADA.

with Report; use Report;
procedure C91004c is

   task type Tt1 is
      entry E1;
      entry Bye;
   end Tt1;

   Obj_Tt1 : array (Natural range 1 .. 2) of Tt1;

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

   task body Tt1 is
   begin
      accept E1 do
         Pspy_Numb (1);
      end E1;

      select
         Tt1.E1;
      else
         Pspy_Numb (2);
      end select;

      accept Bye;
   end Tt1;

begin

   Test ("C91004C", "TASK IDENTIFIER IN OWN BODY");
   Obj_Tt1 (1).E1;
   Obj_Tt1 (1).Bye;

   if Spynumb /= 12 then
      Failed ("WRONG TASK OBJECT REFERENCED");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   abort Obj_Tt1 (2);

   Result;

end C91004c;
