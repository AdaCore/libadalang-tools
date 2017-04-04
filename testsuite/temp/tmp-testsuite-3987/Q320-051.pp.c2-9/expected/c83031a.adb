-- C83031A.ADA

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
--     AN ENUMERATION LITERAL IS HIDDEN BY A SUBPROGRAM DECLARATION OR
--     A RENAMING DECLARATION WHICH DECLARES A HOMOGRAPH OF THE
--     OPERATOR OR LITERAL.

-- HISTORY:
--     VCL  08/10/88  CREATED ORIGINAL TEST.
--     JRL  03/20/92  ELIMINATED REDUNDANT TESTING.

with Report; use Report;
procedure C83031a is
begin
   Test
     ("C83031A",
      "AN IMPLICIT DECLARATION OF A PREDEFINED " &
      "OPERATOR OR AN ENUMERATION LITERAL IS HIDDEN " &
      "BY A SUBPROGRAM DECLARATION OR A RENAMING " &
      "DECLARATION WHICH DECLARES A HOMOGRAPH OF THE " &
      "OPERATOR OR LITERAL");

   declare             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
      package P is
         type Int is range -20 .. 20;

         M : Int := 3 * Int (Ident_Int (3));
         N : Int := 4 + Int (Ident_Int (4));

         function "*" (Left, Right : Int) return Int;
         type Int2 is private;
         function "+" (Left, Right : Int2) return Int2;
      private
         function "+" (Left, Right : Int) return Int renames "/";

         type Int2 is range -20 .. 20;
      end P;
      use P;

      package body P is
         function "*" (Left, Right : Int) return Int is
         begin
            return Left / Right;
         end "*";

         function "+" (Left, Right : Int2) return Int2 is
         begin
            return Left - Right;
         end "+";

      begin
         if 2 * Int (Ident_Int (2)) /= 1 then
            Failed
              ("INCORRECT VALUE RETURNED IN CALL TO " &
               "EXPLICIT '*' OPERATOR - 1");
         end if;

         if N /= 8 then
            Failed ("INCORRECT INITIAL VALUE FOR N - 1");
         end if;
         N := 2 + 2;
         if N /= Int (Ident_Int (1)) then
            Failed
              ("INCORRECT VALUE FOR N AFTER CALL TO " &
               "EXPLICIT '+' OPERATOR - 1");
         end if;

         declare
            Q : Int2 := 8 + 9;
         begin
            if Q /= -1 then
               Failed ("INCORRECT VALUE FOR Q");
            end if;
         end;
      end P;
   begin
      if M /= 9 then
         Failed ("INCORRECT INITIAL VALUE FOR M - 2");
      end if;
      if 2 * Int (Ident_Int (2)) /= 1 then
         Failed
           ("INCORRECT VALUE RETURNED IN CALL TO " &
            "EXPLICIT '*' OPERATOR - 2");
      end if;

      N := 2 + 2;
      if N /= Int (Ident_Int (4)) then
         Failed
           ("INCORRECT VALUE FOR N AFTER CALL TO " &
            "IMPLICIT '+' OPERATOR - 2");
      end if;

   end;

   declare   -- CHECK SUBPROGRAM DECLARATIONS OF ENUMERATION LITERALS.

      package P1 is
         type Enum1 is (E11, E12, E13);
         type Priv1 is private;
         function E11 return Priv1;
      private
         type Priv1 is new Enum1;
         function E12 return Priv1 renames E13;
      end P1;
      use P1;

      E13 : Integer := Ident_Int (5);

      function E12 return Enum1 renames E11;

      function Check (E : Enum1) return Integer is
      begin
         return Enum1'Pos (E);
      end Check;

      function Check (E : Integer) return Integer is
      begin
         return Integer'Pos (E);
      end Check;

      package body P1 is
         function E11 return Priv1 is
         begin
            return E13;
         end E11;
      begin
         if Priv1'(E11) /= E13 then
            Failed ("INCORRECT VALUE FOR E11");
         end if;

         if E12 /= Priv1'Last then
            Failed ("INCORRECT VALUE FOR E12 - 1");
         end if;
      end P1;
   begin
      if E12 /= Enum1'First then
         Failed ("INCORRECT VALUE FOR E12 - 2");
      end if;

      if Check (E13) /= 5 then
         Failed ("INCORRECT VALUE FOR E13");
      end if;
   end;
   Result;
end C83031a;
