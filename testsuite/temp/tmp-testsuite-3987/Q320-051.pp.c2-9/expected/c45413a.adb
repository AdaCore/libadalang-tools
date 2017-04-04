-- C45413A.ADA

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
-- CHECK THAT UNARY MINUS YIELDS AND ACCEPTS RESULTS BELONGING TO
-- THE BASE TYPE.

-- JBG 2/24/84
-- JRL 10/13/96 Removed static expressions which contained values outside
--              the base range.

with Report; use Report;
procedure C45413a is

   type Int is range 1 .. 10;

   X : Int := Int (Ident_Int (9));

begin

   Test ("C45413A", "CHECK SUBTYPE OF UNARY PLUS/MINUS");

   begin

      if -X /= Int'Val (-9) then
         Failed ("INCORRECT RESULT - UNARY MINUS");
      end if;

   exception

      when Constraint_Error =>
         Failed
           ("UNARY MINUS DOES NOT YIELD RESULT " &
            "BELONGING TO THE BASE TYPE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 1");
   end;

   begin

      if -(Int'Val (-9)) /= 9 then
         Failed ("WRONG RESULT - UNARY MINUS");
      end if;

   exception

      when Constraint_Error =>
         Failed ("UNARY MINUS ARGUMENT NOT IN BASE TYPE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED - 2");
   end;

   Result;

end C45413a;
