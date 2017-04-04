-- C83031E.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR IS
--     HIDDEN BY A GENERIC FORMAL SUBPROGRAM DECLARATION WHICH DECLARES
--     A HOMOGRAPH OF THE OPERATOR.

-- HISTORY:
--     BCB 09/19/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C83031e is

begin
   Test
     ("C83031E",
      "CHECK THAT AN IMPLICIT DECLARATION OF A " &
      "PREDEFINED OPERATOR IS HIDDEN BY A GENERIC " &
      "FORMAL SUBPROGRAM DECLARATION WHICH DECLARES " &
      "A HOMOGRAPH OF THE OPERATOR");

   declare             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
      type Int is range -20 .. 20;

      generic
         with function "*" (Left, Right : Int) return Int;
      package P is
      end P;

      package body P is
      begin
         if 2 * Int (Ident_Int (2)) /= 1 then
            Failed
              ("INCORRECT VALUE RETURNED IN CALL TO " &
               "EXPLICIT '*' OPERATOR - 1");
         end if;
      end P;

      function Mult (X, Y : Int) return Int is
      begin
         return X / Y;
      end Mult;

      package New_P is new P (Mult);
   begin
      null;
   end;

   Result;
end C83031e;
