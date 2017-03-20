-- C45231A.ADA

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
-- CHECK THAT THE RELATIONAL AND MEMBERSHIP OPERATIONS YIELD CORRECT
-- RESULTS FOR PREDEFINED TYPE INTEGER (INCLUDING THE CASE IN WHICH THE
-- RELATIONAL OPERATORS ARE REDEFINED).

-- SUBTESTS ARE:
--     (A). TESTS FOR RELATIONAL OPERATORS.
--     (B). TESTS FOR MEMBERSHIP OPERATORS.
--     (C). TESTS FOR MEMBERSHIP OPERATORS IN THE CASE IN WHICH THE
--          RELATIONAL OPERATORS ARE REDEFINED.

-- RJW 2/4/86

with Report; use Report;

procedure C45231a is

begin

   Test
     ("C45231A",
      "CHECK THAT THE RELATIONAL AND " &
      "MEMBERSHIP OPERATIONS YIELD CORRECT " &
      "RESULTS FOR PREDEFINED TYPE INTEGER " &
      "(INCLUDING THE CASE IN WHICH THE " &
      "RELATIONAL OPERATORS ARE REDEFINED)");

   declare -- (A)

      I1a, I1b : Integer          := Ident_Int (1);
      I2       : Integer          := Ident_Int (2);
      Ci2      : constant Integer := 2;

   begin -- (A)

      if (I2 = Ci2) and (not (I2 /= Ci2)) then
         null;
      else
         Failed ("RELATIONAL TEST - 1");
      end if;

      if (I2 /= 4) and (not (I2 = 4)) then
         null;
      else
         Failed ("RELATIONAL TEST - 2");
      end if;

      if (I1a = I1b) and (not (I1a /= I1b)) then
         null;
      else
         Failed ("RELATIONAL TEST - 3");
      end if;

      if (I2 >= Ci2) and (not (I2 < Ci2)) then
         null;
      else
         Failed ("RELATIONAL TEST - 4");
      end if;

      if (I2 <= 4) and (not (I2 > 4)) then
         null;
      else
         Failed ("RELATIONAL TEST - 5");
      end if;

      if (I1a >= I1b) and (I1a <= I1b) then
         null;
      else
         Failed ("RELATIONAL TEST - 6");
      end if;

      if ">" (Left => Ci2, Right => I1a) then
         null;
      else
         Failed ("RELATIONAL TEST - 7");
      end if;

      if "<" (Left => I1a, Right => I2) then
         null;
      else
         Failed ("RELATIONAL TEST - 8");
      end if;

      if ">=" (Left => I1a, Right => I1a) then
         null;
      else
         Failed ("RELATIONAL TEST - 9 ");
      end if;

      if "<=" (Left => I1a, Right => Ci2) then
         null;
      else
         Failed ("RELATIONAL TEST - 10 ");
      end if;

      if "=" (Left => I1a, Right => I1b) then
         null;
      else
         Failed ("RELATIONAL TEST - 11 ");
      end if;

      if "/=" (Left => Ci2, Right => 4) then
         null;
      else
         Failed ("RELATIONAL TEST - 12 ");
      end if;

   end; -- (A)

   ----------------------------------------------------------------

   declare -- (B)

      subtype St is Integer range -10 .. 10;

      I1 : Integer := Ident_Int (1);
      I5 : Integer := Ident_Int (5);

      Ci2  : constant Integer := 2;
      Ci10 : constant Integer := 10;

   begin -- (B)

      if (I1 in St) and (I1 not in Ci2 .. Ci10) then
         null;
      else
         Failed ("MEMBERSHIP TEST - B.1");
      end if;

      if (Ident_Int (11) not in St) and (Ci2 in I1 .. I5) then
         null;
      else
         Failed ("MEMBERSHIP TEST - B.2");
      end if;

      if not (I5 not in Ci2 .. 10) and not (Ident_Int (-11) in St) then
         null;
      else
         Failed ("MEMBERSHIP TEST - B.3");
      end if;

      if not (I1 in Ci2 .. Ci10) and not (I5 not in St) then
         null;
      else
         Failed ("MEMBERSHIP TEST - B.4");
      end if;

      if (I1 not in I5 .. I1) and not (I5 in I5 .. I1) then
         null;
      else
         Failed ("MEMBERSHIP TEST - B.5");
      end if;

   end; -- (B)

   -------------------------------------------------------------

   declare -- (C)

      subtype St is Integer range -10 .. 10;

      I1 : Integer := Ident_Int (1);
      I5 : Integer := Ident_Int (5);

      Ci2  : constant Integer := 2;
      Ci10 : constant Integer := 10;

      function ">" (L, R : Integer) return Boolean is
      begin
         return Integer'Pos (L) <= Integer'Pos (R);
      end ">";

      function ">=" (L, R : Integer) return Boolean is
      begin
         return Integer'Pos (L) < Integer'Pos (R);
      end ">=";

      function "<" (L, R : Integer) return Boolean is
      begin
         return Integer'Pos (L) >= Integer'Pos (R);
      end "<";

      function "<=" (L, R : Integer) return Boolean is
      begin
         return Integer'Pos (L) > Integer'Pos (R);
      end "<=";

   begin -- (C)

      if (I1 in St) and (I1 not in Ci2 .. Ci10) then
         null;
      else
         Failed ("MEMBERSHIP TEST - C.1");
      end if;

      if (Ident_Int (11) not in St) and (Ci2 in I1 .. I5) then
         null;
      else
         Failed ("MEMBERSHIP TEST - C.2");
      end if;

      if not (I5 not in Ci2 .. 10) and not (Ident_Int (-11) in St) then
         null;
      else
         Failed ("MEMBERSHIP TEST - C.3");
      end if;

      if not (I1 in Ci2 .. Ci10) and not (I5 not in St) then
         null;
      else
         Failed ("MEMBERSHIP TEST - C.4");
      end if;

      if (I1 not in I5 .. I1) and not (I5 in I5 .. I1) then
         null;
      else
         Failed ("MEMBERSHIP TEST - C.5");
      end if;

   end; -- (C)

   Result;

end C45231a;
