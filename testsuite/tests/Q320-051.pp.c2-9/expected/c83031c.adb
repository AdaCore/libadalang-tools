-- C83031C.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR OR
--     ENUMERATION LITERAL IS HIDDEN BY A GENERIC INSTANTIATION WHICH
--     DECLARES A HOMOGRAPH OF THE OPERATOR OR LITERAL.

-- HISTORY:
--     BCB 09/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83031c is

begin
   Test
     ("C83031C",
      "CHECK THAT AN IMPLICIT DECLARATION OF A " &
      "PREDEFINED OPERATOR OR ENUMERATION LITERAL IS " &
      "HIDDEN BY A GENERIC INSTANTIATION WHICH " &
      "DECLARES A HOMOGRAPH OF THE OPERATOR OR " & "LITERAL");

   declare             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
      package P is
         type Int is range -20 .. 20;

         generic
            type X is range <>;
         function Gen_Fun (Left, Right : X) return X;
      end P;
      use P;

      package body P is
         function Gen_Fun (Left, Right : X) return X is
         begin
            return Left / Right;
         end Gen_Fun;

         function "*" is new Gen_Fun (Int);
      begin
         if 2 * Int (Ident_Int (2)) /= 1 then
            Failed
              ("INCORRECT VALUE RETURNED IN CALL TO " &
               "EXPLICIT '*' OPERATOR - 1");
         end if;
      end P;
   begin
      null;
   end;

   declare   -- CHECK SUBPROGRAM DECLARATIONS OF ENUMERATION LITERALS.

      package P1 is
         type Enum1 is (E11, E12, E13);
         type Priv1 is private;

         generic
            type X is (<>);
         function Gen_Fun return X;
      private
         type Priv1 is new Enum1;
      end P1;
      use P1;

      package body P1 is
         function Gen_Fun return X is
         begin
            return X'Last;
         end Gen_Fun;

         function E11 is new Gen_Fun (Priv1);
      begin
         if Priv1'(E11) /= E13 then
            Failed ("INCORRECT VALUE FOR E11");
         end if;
      end P1;
   begin
      null;
   end;

   Result;
end C83031c;
