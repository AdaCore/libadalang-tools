-- C54A13C.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A QUALIFIED EXPRESSION, A
--     TYPE CONVERSION, OR ONE OF THESE IN PARENTHESES, AND ITS
--     SUBTYPE IS NONSTATIC, THEN ANY VALUE OF THE EXPRESSION'S
--     BASE TYPE MAY APPEAR AS A CHOICE.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C54a13c is

   L : Integer := 1;
   R : Integer := 100;

   subtype Int is Integer range L .. R;

   A : Int := 50;

   B : Integer := 50;

   C : Integer;

begin
   Test
     ("C54A13C",
      "CHECK THAT IF A CASE EXPRESSION IS A " &
      "QUALIFIED EXPRESSION, A TYPE CONVERSION, " &
      "OR ONE OF THESE IN PARENTHESES, AND ITS " &
      "SUBTYPE IS NONSTATIC, THEN ANY VALUE OF THE " &
      "EXPRESSION'S BASE TYPE MAY APPEAR AS A CHOICE");

   case Int'(A) is
      when 0 =>
         C := Ident_Int (5);
      when 50 =>
         C := Ident_Int (10);
      when -3_000 =>
         C := Ident_Int (15);
      when others =>
         C := Ident_Int (20);
   end case;

   if C /= Ident_Int (10) then
      Failed ("INCORRECT CHOICE MADE FOR QUALIFIED EXPRESSION IN " & "CASE");
   end if;

   case Int (B) is
      when 0 =>
         C := Ident_Int (5);
      when 50 =>
         C := Ident_Int (10);
      when -3_000 =>
         C := Ident_Int (15);
      when others =>
         C := Ident_Int (20);
   end case;

   if C /= Ident_Int (10) then
      Failed ("INCORRECT CHOICE MADE FOR TYPE CONVERSION IN CASE");
   end if;

   case (Int'(A)) is
      when 0 =>
         C := Ident_Int (5);
      when 50 =>
         C := Ident_Int (10);
      when -3_000 =>
         C := Ident_Int (15);
      when others =>
         C := Ident_Int (20);
   end case;

   if C /= Ident_Int (10) then
      Failed
        ("INCORRECT CHOICE MADE FOR QUALIFIED EXPRESSION IN " &
         "PARENTHESES IN CASE");
   end if;

   case (Int (B)) is
      when 0 =>
         C := Ident_Int (5);
      when 50 =>
         C := Ident_Int (10);
      when -3_000 =>
         C := Ident_Int (15);
      when others =>
         C := Ident_Int (20);
   end case;

   if C /= Ident_Int (10) then
      Failed
        ("INCORRECT CHOICE MADE FOR TYPE CONVERSION IN " &
         "PARENTHESES IN CASE");
   end if;

   Result;
end C54a13c;
