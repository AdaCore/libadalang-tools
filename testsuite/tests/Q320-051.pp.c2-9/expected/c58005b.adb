-- C58005B.ADA

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
-- CHECK THAT WHEN A GENERIC FUNCTION IS READY TO RETURN CONTROL TO ITS
--    INVOKER THE CONSTRAINTS ON THE RETURN VALUES ARE CHECKED, AND THAT
--    CONSTRAINT ERROR  IS THEN RAISED IF AND ONLY IF THE CONSTRAINTS
--    ARE NOT SATISFIED.

-- THIS TEST CHECKS THAT THE EXCEPTION IS RAISED UNDER THE APPROPRIATE
--    CONDITIONS; IT ALSO CHECKS THE IDENTITY OF THE EXCEPTION.  THE
--    PRECISE MOMENT AND PLACE THE EXCEPTION IS RAISED IS TESTED
--    ELSEWHERE.

-- SPS 3/10/83
-- JBG 9/13/83
-- AH 8/29/86 ADDED CALLS TO "FAILED" AFTER "IF" STATEMENTS.

with Report;
procedure C58005b is

   use Report;

begin

   Test
     ("C58005B",
      "CHECK THAT EXCEPTIONS ARE RAISED BY A RETURN" &
      " STATEMENT IF AND ONLY IF THE CONSTRAINTS ARE" & " VIOLATED");

   declare
      subtype I1 is Integer range -10 .. 90;
      subtype I2 is Integer range 1 .. 10;

      generic
      function Fn1 (X : I1) return I2;

      function Fn1 (X : I1) return I2 is
      begin
         return X;
      end Fn1;

      function F1 is new Fn1;

   begin

      begin
         if F1 (Ident_Int (0)) in I2 then
            Failed ("EXCEPTION NOT RAISED  -  1A");
         else
            Failed ("EXCEPTION NOT RAISED  -  1B");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED  -  1");
      end;

      begin
         if F1 (Ident_Int (11)) in I2 then
            Failed ("EXCEPTION NOT RAISED  -  2A");
         else
            Failed ("EXCEPTION NOT RAISED  -  2B");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED  -  2");
      end;

   end;

   Result;

end C58005b;
