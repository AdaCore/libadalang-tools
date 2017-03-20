-- C55C02B.ADA

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
-- CHECK THAT THE WHILE CONDITION IS EVALUATED EACH TIME.

-- DAT 1/29/81
-- SPS 3/2/83

with Report;
procedure C55c02b is

   use Report;

   I : Integer := 0;

   Ft : array (False .. True) of Boolean :=
     (Ident_Bool (False), Ident_Bool (True));

begin
   Test ("C55C02B", "WHILE CONDITION IS EVALUATED EACH TIME THROUGH");

   while I /= 10 loop
      I := I + 1;
   end loop;
   if I /= 10 then
      Failed ("BAD LOOP FLOW - OPTIMIZABLE CONDITION");
   end if;

   I := 10;
   while Ft (Ident_Bool (I /= 14)) loop
      I := I + 1;
   end loop;
   if I /= 14 then
      Failed ("BAD LOOP FLOW - DYNAMIC CONDITION");
   end if;

   Result;
end C55c02b;
