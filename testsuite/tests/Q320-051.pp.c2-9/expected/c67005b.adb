-- C67005B.ADA

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
-- CHECK THAT IF EQUALITY IS REDEFINED FOR A SCALAR TYPE, CASE
-- STATEMENTS STILL USE THE PREDEFINED EQUALITY OPERATION.

-- JBG 9/28/83

with Report; use Report;
procedure C67005b is

   generic
      type Lp is limited private;
      with function Equal (L, R : Lp) return Boolean;
   package Equality_Operator is
      function "=" (L, R : Lp) return Boolean;
   end Equality_Operator;

   package body Equality_Operator is
      function "=" (L, R : Lp) return Boolean is
      begin
         return Equal (L, R);
      end "=";
   end Equality_Operator;

begin
   Test
     ("C67005B",
      "CHECK THAT REDEFINING EQUALITY FOR A " &
      "SCALAR TYPE DOES NOT AFFECT CASE STATEMENTS");

   declare
      type My is new Integer;
      Check : My;

      Var : Integer range 1 .. 3 := 3;

      package Integer_Equals is
         function Equal (L, R : Integer) return Boolean;
         package Integer_Equal is new Equality_Operator (Integer, Equal);
      end Integer_Equals;

      function "="
        (L, R : Integer) return Boolean renames
        Integer_Equals.Integer_Equal."=";

      package body Integer_Equals is
         function Equal (L, R : Integer) return Boolean is
         begin
            return False;
         end Equal;
      end Integer_Equals;

   begin

      if Var = 3 then
         Failed ("DID NOT USE REDEFINED '=' - 1");
      end if;

      if Var /= 3 then
         null;
      else
         Failed ("DID NOT USE REDEFINED '/=' - 1");
      end if;

      if Var = Ident_Int (3) then
         Failed ("DID NOT USE REDEFINED '=' - 2");
      end if;

      if Var /= Ident_Int (3) then
         null;
      else
         Failed ("DID NOT USE REDEFINED '/=' - 2");
      end if;

      Check := My (Ident_Int (0));
      if Check /= 0 then
         Failed ("USING WRONG EQUALITY FOR DERIVED TYPE");
      end if;

      case Var is
         when 1 .. 3 =>
            Check := My (Ident_Int (1));
         when others =>
            null;
      end case;

      if Check /= 1 then
         Failed ("DID NOT USE PREDEFINED EQUALS IN CASE - 1");
      end if;

      case Ident_Int (Var) is
         when 1 =>
            Check := 4;
         when 2 =>
            Check := 5;
         when 3 =>
            Check := 6;
         when others =>
            Check := 7;
      end case;

      if Check /= 6 then
         Failed ("DID NOT USE PREDEFINED EQUALS IN CASE - 2");
      end if;

   end;

   Result;

end C67005b;
