-- C83E03A.ADA

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
-- CHECK THAT A FORMAL PARAMETER IN A NAMED PARAMETER ASSOCIATION
--    IS NOT CONFUSED WITH AN ACTUAL PARAMETER IDENTIFIER  HAVING THE
--    SAME SPELLING.

--    RM    23 JULY 1980

with Report;
procedure C83e03a is

   use Report;

   P          : Integer range 1 .. 23 := 17;
   Flow_Index : Integer               := 0;

begin

   Test
     ("C83E03A",
      "CHECK THAT A FORMAL PARAMETER IN A NAMED" &
      " PARAMETER ASSOCIATION  IS NOT CONFUSED" &
      " WITH AN ACTUAL PARAMETER HAVING THE" & " SAME SPELLING");

   declare

      procedure Bump is
      begin
         Flow_Index := Flow_Index + 1;
      end Bump;

      procedure P1 (P : Integer) is
      begin
         if P = 17 then
            Bump;
         end if;
      end P1;

      function F1 (P : Integer) return Integer is
      begin
         return P;
      end F1;

   begin

      P1 (P);
      P1 (P => P);

      if F1 (P + 1) = 17 + 1 then
         Bump;
      end if;
      if F1 (P => P + 1) = 17 + 1 then
         Bump;
      end if;

   end;

   if Flow_Index /= 4 then
      Failed ("INCORRECT ACCESSING OR INCORRECT FLOW");
   end if;

   Result;

end C83e03a;
