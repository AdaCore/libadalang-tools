-- C49021A.ADA

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
-- CHECK THAT BOOLEAN LITERALS CAN BE USED IN STATIC EXPRESSIONS TOGETHER WITH
-- THE LOGICAL OPERATORS, THE NOT OPERATOR, AND THE RELATIONAL AND EQUALITY
-- OPERATORS.

-- L.BROWN  09/25/86

with Report; use Report;
procedure C49021a is

   Cas_Bol : Boolean  := True;
   X1      : constant := Boolean'Pos ((True and False) or (True and True));
   X2      : constant := Boolean'Pos ((True <= False) and (False >= False));

begin
   Test
     ("C49021A",
      "BOOLEAN LITERALS TOGETHER WITH CERTAIN OPERATORS," &
      "CAN BE USED IN STATIC EXPRESSIONS.");
   if X1 /= 1 then
      Failed ("INCORRECT VALUE RETURNED BY BOOLEAN EXPRESSION 1");
   end if;

   if X2 /= 0 then
      Failed ("INCORRECT VALUE RETURNED BY BOOLEAN EXPRESSION 2");
   end if;

   case Cas_Bol is
      when ((True and False) xor (True xor True)) =>
         Failed ("INCORRECT VALUE RETURNED BY BOOLEAN " & "EXPRESSION 2");
      when others =>
         Cas_Bol := True;
   end case;

   case Cas_Bol is
      when ((True > False) or (False <= True)) =>
         Cas_Bol := True;
      when others =>
         Failed ("INCORRECT VALUE RETURNED BY BOOLEAN " & "EXPRESSION 3");
   end case;

   case Cas_Bol is
      when not ((True or False) = (False and True)) =>
         Cas_Bol := True;
      when others =>
         Failed ("INCORRECT VALUE RETURNED BY BOOLEAN " & "EXPRESSION 4");
   end case;

   case Cas_Bol is
      when (((True = False) or (False and True)) /= (True < True)) =>
         Failed ("INCORRECT VALUE RETURNED BY BOOLEAN " & "EXPRESSION 5");
      when others =>
         Cas_Bol := True;
   end case;

   Result;

end C49021a;
