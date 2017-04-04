-- C43214D.ADA

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
-- CHECK THAT THE LOWER BOUND FOR THE STRING LITERAL IS DETERMINED BY
-- THE APPLICABLE INDEX CONSTRAINT, WHEN ONE EXISTS.

-- EG  02/10/84

with Report;

procedure C43214d is

   use Report;

begin

   Test ("C43214D", "CONSTRAINED FUNCTION RESULT TYPE");

   begin

      Case_C : declare

         type Tc is
           array (Integer range -1 .. 0, Ident_Int (7) .. 9) of Character;

         function Fun1 (A : Integer) return Tc is
         begin
            return ("ABC", "DEF");
         end Fun1;

      begin

         if Fun1 (5)'First (1) /= -1 then
            Failed ("LOWER BOUND INCORRECT " & "FOR 'FIRST(1)");
         elsif Fun1 (5)'First (2) /= 7 then
            Failed ("LOWER BOUND INCORRECT " & "FOR 'FIRST(2)");
         elsif Fun1 (5)'Last (1) /= 0 then
            Failed ("UPPER BOUND INCORRECT " & "FOR 'LAST(1)");
         elsif Fun1 (5)'Last (2) /= 9 then
            Failed ("UPPER BOUND INCORRECT " & "FOR 'LAST(2)");
         elsif Fun1 (5) /= ("ABC", "DEF") then
            Failed ("FUNCTION DOES NOT " & "RETURN THE CORRECT VALUES");
         end if;

      end Case_C;

   end;

   Result;

end C43214d;
