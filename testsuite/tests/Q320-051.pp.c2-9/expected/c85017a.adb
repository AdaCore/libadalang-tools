-- C85017A.ADA

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
--     CHECK THAT RENAMING A PREDEFINED OPERATION WITH AN IDENTIFIER
--     AND THEN RENAMING THE IDENTIFIER AS AN OPERATOR SYMBOL ALLOWS THE
--     NEW NAME TO BE USED IN A STATIC EXPRESSION.

-- HISTORY:
--     JET 03/24/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85017a is

   function Plus (L, R : Integer) return Integer renames "+";
   function Minus (L, R : Integer) return Integer renames "-";

   function "-" (L, R : Integer) return Integer renames Plus;
   function "+" (L, R : Integer) return Integer renames Minus;

   I1 : constant Integer := 10 + 10;
   I2 : constant Integer := 10 - 10;

   type Int is range I1 .. I2;
begin
   Test
     ("C85017A",
      "CHECK THAT RENAMING A PREDEFINED OPERATION WITH " &
      "AN IDENTIFIER AND THEN RENAMING THE IDENTIFIER " &
      "AS AN OPERATOR SYMBOL ALLOWS THE NEW NAME TO BE " &
      "USED IN A STATIC EXPRESSION");

   if I1 /= Ident_Int (0) then
      Failed ("INCORRECT VALUE OF I1: " & Integer'Image (I1));
   end if;

   if I2 /= Ident_Int (20) then
      Failed ("INCORRECT VALUE OF I2: " & Integer'Image (I2));
   end if;

   Result;
end C85017a;
