-- C54A13D.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A FUNCTION INVOCATION,
--     ATTRIBUTE, STATIC EXPRESSION, OR ONE OF THESE IN PARENTHESES,
--     THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE MAY APPEAR AS A
--     CHOICE.

-- HISTORY:
--     BCB 07/19/88  CREATED ORIGINAL TEST.
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     GJD 11/15/95  REMOVED ADA 95 INCOMPATIBLE ALTERNATIVE IN FIRST CASE.

with Report; use Report;

procedure C54a13d is

   subtype Int is Integer range -100 .. 100;

   Cons : constant Int := 0;

   C : Int;

   type Enum is (One, Two, Three, Four, Five, Six);

   subtype Subenum is Enum range Three .. Four;

   function Func return Int is
   begin
      return 0;
   end Func;

begin
   Test
     ("C54A13D",
      "CHECK THAT IF A CASE EXPRESSION IS A FUNCTION " &
      "INVOCATION, ATTRIBUTE, STATIC EXPRESSION, OR " &
      "ONE OF THESE IN PARENTHESES, THEN ANY VALUE " &
      "OF THE EXPRESSION'S BASE TYPE MAY APPEAR AS " & "A CHOICE");

   case Func is
      when 0 =>
         C := Ident_Int (5);
      when 100 =>
         C := Ident_Int (10);
      when others =>
         C := Ident_Int (20);
   end case;

   if not Equal (C, 5) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
         "FUNCTION INVOCATION - 1");
   end if;

   case (Func) is
      when 0 =>
         C := Ident_Int (25);
      when 100 =>
         C := Ident_Int (50);
      when -3_000 =>
         C := Ident_Int (75);
      when others =>
         C := Ident_Int (90);
   end case;

   if not Equal (C, 25) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
         "FUNCTION INVOCATION - 2");
   end if;

   case Subenum'First is
      when One =>
         C := Ident_Int (100);
      when Two =>
         C := Ident_Int (99);
      when Three =>
         C := Ident_Int (98);
      when Four =>
         C := Ident_Int (97);
      when Five =>
         C := Ident_Int (96);
      when Six =>
         C := Ident_Int (95);
   end case;

   if not Equal (C, 98) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS AN " & "ATTRIBUTE - 1");
   end if;

   case (Subenum'First) is
      when One =>
         C := Ident_Int (90);
      when Two =>
         C := Ident_Int (89);
      when Three =>
         C := Ident_Int (88);
      when Four =>
         C := Ident_Int (87);
      when Five =>
         C := Ident_Int (86);
      when Six =>
         C := Ident_Int (85);
   end case;

   if not Equal (C, 88) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS AN " & "ATTRIBUTE - 2");
   end if;

   case Cons * 1 is
      when 0 =>
         C := Ident_Int (1);
      when 100 =>
         C := Ident_Int (2);
      when -3_000 =>
         C := Ident_Int (3);
      when others =>
         C := Ident_Int (4);
   end case;

   if not Equal (C, 1) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
         "STATIC EXPRESSION - 1");
   end if;

   case (Cons * 1) is
      when 0 =>
         C := Ident_Int (10);
      when 100 =>
         C := Ident_Int (20);
      when -3_000 =>
         C := Ident_Int (30);
      when others =>
         C := Ident_Int (40);
   end case;

   if not Equal (C, 10) then
      Failed
        ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
         "STATIC EXPRESSION - 2");
   end if;

   Result;
end C54a13d;
