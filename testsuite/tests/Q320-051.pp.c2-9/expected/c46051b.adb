-- C46051B.ADA

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
--     CHECK THAT ENUMERATION VALUES CAN BE CONVERTED IF THE OPERAND
--     AND TARGET TYPES ARE RELATED BY DERIVATION, EVEN IF THE OPERAND
--     AND TARGET TYPES HAVE DIFFERENT REPRESENTATIONS.

-- HISTORY:
--     JET 07/13/88  CREATED ORIGINAL TEST.
--     RJW 08/28/89  REMOVED APPLICABILITY CRITERIA AND CHANGED
--                   EXTENSION TO 'ADA'.  CHANGED THE CODES IN SECOND
--                   ENUMERATION REPRESENTATION CLAUSE.

with Report; use Report;
procedure C46051b is

   type Enum is (We, Love, Writing, Tests);

   type Enum1 is new Enum;
   for Enum1 use (We => -1, Love => 0, Writing => 3, Tests => 9);

   type Enum2 is new Enum;
   for Enum2 use (We => 10, Love => 15, Writing => 16, Tests => 19);

   type Enum3 is new Enum1;

   E  : Enum  := Enum'Val (Ident_Int (0));
   E1 : Enum1 := Enum1'Val (Ident_Int (1));
   E2 : Enum2 := Enum2'Val (Ident_Int (2));
   E3 : Enum3 := Enum3'Val (Ident_Int (3));

begin
   Test
     ("C46051B",
      "CHECK THAT ENUMERATION VALUES CAN BE " &
      "CONVERTED IF THE OPERAND AND TARGET TYPES " &
      "ARE RELATED BY DERIVATION, EVEN IF THE " &
      "OPERAND AND TARGET TYPES HAVE DIFFERENT " & "REPRESENTATIONS");

   if Enum1 (E) /= We then
      Failed ("INCORRECT CONVERSION OF 'ENUM1 (E)'");
   end if;

   if Enum (E1) /= Love then
      Failed ("INCORRECT CONVERSION OF 'ENUM (E1)'");
   end if;

   if Enum1 (E2) /= Writing then
      Failed ("INCORRECT CONVERSION OF 'ENUM1 (E2)'");
   end if;

   if Enum2 (E3) /= Tests then
      Failed ("INCORRECT CONVERSION OF 'ENUM2 (E3)'");
   end if;

   if Enum (E) /= We then
      Failed ("INCORRECT CONVERSION OF 'ENUM (E)'");
   end if;

   if Enum2 (E1) /= Love then
      Failed ("INCORRECT CONVERSION OF 'ENUM2 (E1)'");
   end if;

   if Enum3 (E2) /= Writing then
      Failed ("INCORRECT CONVERSION OF 'ENUM3 (E2)'");
   end if;

   if Enum (E3) /= Tests then
      Failed ("INCORRECT CONVERSION OF 'ENUM (E3)'");
   end if;

   Result;

exception
   when others =>
      Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "ENUMERATION TYPES");
      Result;
end C46051b;
