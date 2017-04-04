-- C45304A.ADA

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
-- OBJECTIVE:
--     CHECK THAT CONSTRAINT_ERROR IS RAISED BY
--     "+" AND "-" FOR PREDEFINED INTEGER WHEN THE RESULT IS OUTSIDE
--     THE RANGE OF THE BASE TYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- HISTORY:
--     TBN 10/06/86  CREATED ORIGINAL TEST.
--     JET 12/29/87  FURTHER DEFEATED OPTIMIZATION.
--     JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

with Report; use Report;
procedure C45304a is

begin
   Test
     ("C45304A",
      "CHECK THAT CONSTRAINT_ERROR " &
      "IS RAISED BY ""+"" AND ""-"" FOR PREDEFINED " &
      "INTEGER WHEN THE RESULT IS OUTSIDE THE RANGE " &
      "OF THE BASE TYPE");

   declare
      B : Integer := Integer'Last;
   begin
      if Equal (Ident_Int (B) + 1, 0) then
         Failed ("NO EXCEPTION FOR ADDITION -- ZERO RESULT");
      else
         Failed ("NO EXCEPTION FOR ADDITION -- NONZERO RESULT");
      end if;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR ADDITION");
   end;

   declare
      B : Integer := Integer'First;
   begin
      if Equal (Ident_Int (B) - 1, 0) then
         Failed ("NO EXCEPTION FOR SUBTRACTION -- ZERO RESULT");
      else
         Failed ("NO EXCEPTION FOR SUBTRACTION -- " & "NONZERO RESULT");
      end if;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR SUBTRACTION");
   end;

   Result;
end C45304a;
