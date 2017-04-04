-- C43214C.ADA

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

procedure C43214c is

   use Report;

begin

   Test ("C43214C", "CONSTRAINED ARRAY FORMAL GENERIC " & "PARAMETER");

   begin

      Case_B : declare

         subtype Stb is String (5 .. 8);

         generic
            B1 : Stb;
         procedure Proc1;

         procedure Proc1 is
         begin
            if B1'First /= 5 then
               Failed ("LOWER BOUND INCORRECT");
            elsif B1'Last /= 8 then
               Failed ("UPPER BOUND INCORRECT");
            elsif B1 /= "ABCD" then
               Failed ("ARRAY DOES NOT " & "CONTAIN THE CORRECT VALUES");
            end if;
         end Proc1;

         procedure Proc2 is new Proc1 ("ABCD");

      begin

         Proc2;

      end Case_B;

   end;

   Result;

end C43214c;
