-- C38102D.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE REDECLARED AS A TASK TYPE.

-- AH    8/14/86

with Report; use Report;
procedure C38102d is
   Global : Integer := 0;
begin
   Test ("C38102D", "INCOMPLETE TYPES CAN BE TASKS");
   declare
      type T1;
      task type T1 is
         entry E (Local : in out Integer);
      end T1;
      T1_Obj : T1;
      task body T1 is
      begin
         accept E (Local : in out Integer) do
            Local := Ident_Int (2);
         end E;
      end T1;
   begin
      T1_Obj.E (Global);
   end;

   if Global /= Ident_Int (2) then
      Failed ("TASK NOT EXECUTED");
   end if;
   Result;
end C38102d;
