-- C54A13A.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A DECLARED VARIABLE OR
--     CONSTANT, OR ONE OF THESE IN PARENTHESES, AND ITS SUBTYPE IS
--     NONSTATIC, THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE MAY
--     APPEAR AS A CHOICE.

-- HISTORY:
--     BCB 02/29/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C54a13a is

   subtype Int is Integer range Ident_Int (5) .. Ident_Int (10);

   A    : Int          := 8;
   B    : constant Int := 7;
   C, D : Integer;

   function Ident (X : Int) return Int is
   begin
      if Equal (3, 3) then
         return X;
      else
         return 0;
      end if;
   end Ident;

begin
   Test
     ("C54A13A",
      "CHECK THAT IF A CASE EXPRESSION IS A DECLARED " &
      "VARIABLE OR CONSTANT, OR ONE OF THESE IN " &
      "PARENTHESES, AND ITS SUBTYPE IS NONSTATIC, " &
      "THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE " &
      "MAY APPEAR AS A CHOICE");

   case A is
      when 0 =>
         C := Ident_Int (5);
      when 8 =>
         C := Ident_Int (10);
      when 30_000 =>
         C := Ident_Int (15);
      when -30_000 =>
         C := Ident_Int (20);
      when others =>
         C := Ident_Int (25);
   end case;

   if C /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR CASE EXPRESSION - 1");
   end if;

   case B is
      when 0 =>
         D := Ident_Int (5);
      when 100 =>
         D := Ident_Int (10);
      when 30_000 =>
         D := Ident_Int (15);
      when -30_000 =>
         D := Ident_Int (20);
      when others =>
         D := Ident_Int (25);
   end case;

   if D /= Ident_Int (25) then
      Failed ("IMPROPER VALUE FOR CASE EXPRESSION - 2");
   end if;

   case (A) is
      when 0 =>
         C := Ident_Int (5);
      when 8 =>
         C := Ident_Int (10);
      when 30_000 =>
         C := Ident_Int (15);
      when -30_000 =>
         C := Ident_Int (20);
      when others =>
         C := Ident_Int (25);
   end case;

   if C /= Ident_Int (10) then
      Failed ("IMPROPER VALUE FOR CASE EXPRESSION - 3");
   end if;

   case (B) is
      when 0 =>
         D := Ident_Int (5);
      when 110 =>
         D := Ident_Int (10);
      when 30_000 =>
         D := Ident_Int (15);
      when -30_000 =>
         D := Ident_Int (20);
      when others =>
         D := Ident_Int (25);
   end case;

   if D /= Ident_Int (25) then
      Failed ("IMPROPER VALUE FOR CASE EXPRESSION - 4");
   end if;

   Result;
end C54a13a;
