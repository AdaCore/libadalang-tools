-- C94001F.ADA

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
-- CHECK THAT A TASK IS ALSO COMPLETED IF AN EXCEPTION IS RAISED BY
-- THE EXECUTION OF ITS SEQUENCE OF STATEMENTS.
-- THIS MUST HOLD FOR BOTH CASES WHERE A HANDLER IS PRESENT OR NOT.
--   VERSION WITHOUT EXCEPTION HANDLER.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C940AGB-B.ADA

with Report; use Report;
procedure C94001f is

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

begin

   Test ("C94001F", "TASK COMPLETION BY EXCEPTION -- NO HANDLER");

   Block : declare

      task T1;

      task body T1 is
         type I1 is range 0 .. 1;
         Obj_I1 : I1;
      begin
         Obj_I1 := I1 (Ident_Int (2));  -- CONSTRAINT_ERROR.
         Pspy_Numb (1);
      end T1;

   begin
      null;          -- WAIT FOR TERMINATION.
   exception
      when Constraint_Error =>
         Failed ("PROPAGATED CONSTRAINT_ERROR OUT OF TASK");
      when Tasking_Error =>
         Failed ("RAISED TASKING_ERROR");
      when others =>
         Failed ("RAISED OTHER EXCEPTION");
   end Block;

   if Spynumb /= 0 then
      Failed
        ("TASK T1 NOT COMPLETED AFTER EXCEPTION IN SEQUENCE " &
         "OF STATEMENTS");
   end if;

   Result;

end C94001f;
