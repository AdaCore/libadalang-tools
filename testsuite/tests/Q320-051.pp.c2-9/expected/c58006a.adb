-- C58006A.ADA

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
-- CHECK THAT IF THE EVALUATION OF A RETURN STATEMENT'S EXPRESSION
-- RAISES AN EXCEPTION, THE EXCEPTION CAN BE HANDLED WITHIN THE BODY OF
-- THE FUNCTION.

-- RM 05/11/81
-- SPS 10/26/82
-- SPS 3/8/83
-- JBG 9/13/83

with Report;
procedure C58006a is

   use Report;

begin

   Test
     ("C58006A",
      "CHECK THAT EXCEPTION RAISED BY A RETURN" &
      " STATEMENT CAN BE HANDLED LOCALLY");

   declare
      subtype I1 is Integer range -10 .. 90;
      subtype I2 is Integer range 1 .. 10;

      function Fn1 (X : I1) return I2 is
      begin
         return 0;
      exception
         when Constraint_Error =>
            Comment ("EXCEPTION RAISED - F1");
            return 1;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FN1");
      end Fn1;

      function Fn2 (X : I1) return I2 is
      begin
         return X + Ident_Int (0);
      exception
         when Constraint_Error =>
            Comment ("EXCEPTION RAISED - F2");
            return 1;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FN2");
      end Fn2;

      function Fn3 (X : I1) return I2 is
         Hundred : Integer range -100 .. 100 := Ident_Int (100);
      begin
         return Hundred;
      exception
         when Constraint_Error =>
            Comment ("EXCEPTION RAISED - F3");
            return 1;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FN3");
      end Fn3;

   begin

      begin
         if Fn1 (0) /= Ident_Int (1) then
            Failed ("NO EXCEPTION RAISED - FN1( 0 )");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION PROPAGATED - FN1( 0 )");
      end;

      begin
         if Fn2 (0) /= Ident_Int (1) then
            Failed ("NO EXCEPTION RAISED - FN2( 0 )");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION PROPAGATED - FN2( 0 )");
      end;

      begin
         if Fn2 (11) /= Ident_Int (1) then
            Failed ("NO EXCEPTION RAISED - FN2(11 )");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION PROPAGATED - FN2(11 )");
      end;

      begin
         if Fn3 (0) /= Ident_Int (1) then
            Failed ("NO EXCEPTION RAISED - FN3( 0 )");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION PROPAGATED - FN3( 0 )");
      end;

   end;

   Result;

end C58006a;
