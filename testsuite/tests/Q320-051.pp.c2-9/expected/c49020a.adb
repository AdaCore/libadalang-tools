-- C49020A.ADA

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
-- CHECK THAT ENUMERATION LITERALS (INCLUDING CHARACTER LITERALS) CAN BE
-- USED IN STATIC EXPRESSIONS TOGETHER WITH RELATIONAL AND EQUALITY
-- OPERATORS.

-- L.BROWN   09/30/86

with Report; use Report;
procedure C49020a is

   Cas_Bol : Boolean := True;
   Obj1    : Integer := 4;
   type Enum is (Red, Green, Blue, Off, On, 'A', 'B');

begin
   Test
     ("C49020A",
      "ENUMERATION LITERALS (INCLUDING CHARACTER " &
      "LITERALS) TOGETHER WITH RELATIONAL OPERATORS " &
      "CAN BE USED IN STATIC EXPRESSION");

   case Cas_Bol is
      when (Red <= Blue) =>
         Obj1 := 5;
      when (Blue = Green) =>
         Failed ("INCORRECT VALUE RETURNED BY ENUMERATION " & "EXPRESSION 1");
   end case;

   Cas_Bol := True;

   case Cas_Bol is
      when (Green >= On) =>
         Failed ("INCORRECT VALUE RETURNED BY ENUMERATION " & "EXPRESSION 2");
      when (Enum'('A') < Enum'('B')) =>
         Obj1 := 6;
   end case;

   Cas_Bol := True;

   case Cas_Bol is
      when (Blue > 'B') =>
         Failed ("INCORRECT VALUE RETURNED BY ENUMERATION " & "EXPRESSION 3");
      when (Off /= 'A') =>
         Obj1 := 7;
   end case;

   Result;

end C49020a;
