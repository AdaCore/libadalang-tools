-- C87B44A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE TYPE OF THE EXPRESSION IN A RETURN STATEMENT MUST MATCH THE
-- EXPLICIT TYPEMARK IN THE RETURN CLAUSE OF THE FUNCTION'S
-- SPECIFICATION.
--
-- THE FOUR KINDS OF EXPRESSIONS TESTED HERE ARE:
--
--    (A): A CALL TO AN OVERLOADED FUNCTION.
--    (B): AN OVERLOADED OPERATOR SYMBOL.
--    (C): AN OVERLOADED (INFIX) OPERATOR.
--    (D): AN OVERLOADED ENUMERATION LITERAL.

-- TRH  25 JUNE 82

with Report; use Report;

procedure C87b44a is

   type Whole is new Integer range 0 .. Integer'Last;
   type Citrus is (Lemon, Lime, Orange);
   type Hue is (Red, Orange, Yellow);

   function F1 (X, Y : Integer) return Integer is
   begin
      return -1;
   end F1;

   function "*" (X, Y : Whole) return Whole is
   begin
      return 0;
   end "*";

   function "*" (X, Y : Integer) return Hue is
   begin
      return Orange;
   end "*";

   function F1 (X, Y : Integer) return Citrus is
   begin
      return Orange;
   end F1;

begin
   Test ("C87B44A", "OVERLOADED EXPRESSIONS IN RETURN STATEMENTS");
   declare

      function F2 (X, Y : Integer) return Integer is
      begin
         return F1 (X, Y);
      end F2;

      function F2 (X, Y : Whole) return Whole is
      begin
         return "*" (X, Y);
      end F2;

      function F2 (X, Y : Integer) return Hue is
      begin
         return (X * Y);
      end F2;

      function F2 (X, Y : Integer) return Citrus is
      begin
         return Orange;
      end F2;

   begin
      if Integer'(F2 (0, 0)) /= -1 then
         Failed ("(A): RESOLUTION INCORRECT - FUNCTION CALL");
      end if;

      if Whole'(F2 (0, 0)) /= 0 then
         Failed ("(B): RESOLUTION INCORRECT - OPERATOR SYMBOL");
      end if;

      if Hue'Pos (F2 (0, 0)) /= 1 then
         Failed ("(C): RESOLUTION INCORRECT - INFIX OPERATOR");
      end if;

      if Citrus'Pos (F2 (0, 0)) /= 2 then
         Failed ("(D): RESOLUTION INCORRECT - ENUMERATION LITERAL");
      end if;
   end;

   Result;
end C87b44a;
