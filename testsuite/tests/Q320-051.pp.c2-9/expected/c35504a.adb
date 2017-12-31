-- C35504A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED WHEN THE USER-DEFINED ENUMERATION
-- ARGUMENT TO 'SUCC, 'PRED, 'POS, 'VAL, 'IMAGE, AND 'VALUE IS NOT IN THE
-- ATTRIBUTED SUBTYPE'S RANGE CONSTRAINT.

-- DAT 3/18/81
-- SPS 01/13/83

with Report; use Report;

procedure C35504a is

   type E is (A, 'A', B, 'B', C, 'C', D, 'D', Xyz);

   subtype S is E range B .. C;

begin
   Test
     ("C35504A",
      "CONSTRAINT_ERROR IS NOT RAISED IN T'SUCC(X)," &
      " T'PRED(X), T'POS(X), T'VAL(X), T'IMAGE(X), AND" &
      " T'VALUE(X) WHEN THE VALUES ARE NOT WITHIN T'S" &
      " RANGE CONSTRAINT, FOR USER-DEFINED ENUMERATION TYPES");

   begin
      for X in E loop
         if (X /= A and then S'Succ (S'Pred (X)) /= X) or
           (X /= Xyz and then S'Pred (S'Succ (X)) /= X) or
           S'Val (S'Pos (X)) /= X or S'Value (S'Image (X)) /= X then
            Failed ("WRONG ATTRIBUTE VALUE");
         end if;
      end loop;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED" & " WHEN IT SHOULDN'T HAVE BEEN");
      when others =>
         Failed ("INCORRECT EXCEPTION RAISED");
   end;

   Result;
end C35504a;
