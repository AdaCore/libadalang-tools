-- C87B07D.ADA

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
-- THE ATTRIBUTES OF THE FORM T'SUCC (X) AND T'PRED (X) TAKE AN OPERAND X OF
-- TYPE T AND RETURN A VALUE OF TYPE T.

-- TRH  15 SEPT 82

with Report; use Report;

procedure C87b07d is

   type New_Int is new Integer;
   type Whole is new Integer range 0 .. Integer'Last;

   function "+" (X, Y : Whole) return Whole renames "*";
   function "+" (X, Y : New_Int) return New_Int renames "-";

begin
   Test
     ("C87B07D",
      "OVERLOADED OPERANDS TO THE ATTRIBUTES " & "'PRED' AND 'SUCC'");

   if Integer'Succ (1 + 1) /= 3 or
     Integer'Succ (3 + 3) + 1 /= 8 or
     New_Int'Succ (1 + 1) /= 1 or
     New_Int'Succ (3 + 3) + 1 /= 0 or
     Whole'Succ (1 + 1) /= 2 or
     Whole'Succ (3 + 3) + 1 /= 10 or
     Integer'Pred (1 + 1) /= 1 or
     Integer'Pred (3 + 3) + 1 /= 6 or
     New_Int'Pred (1 + 1) /= -1 or
     New_Int'Pred (3 + 3) + 1 /= -2 or
     Whole'Pred (1 + 1) /= 0 or
     Whole'Pred (3 + 3) + 1 /= 8
   then
      Failed
        ("RESOLUTION INCORRECT FOR OPERAND OR RESULT OF" &
         " THE 'PRED' OR 'SUCC' ATTRIBUTE");
   end if;

   Result;
end C87b07d;
