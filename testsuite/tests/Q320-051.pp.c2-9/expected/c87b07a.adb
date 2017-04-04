-- C87B07A.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'POS (X), THE OPERAND X MUST BE A VALUE OF
-- TYPE T. THE RESULT IS OF TYPE UNIVERSAL_INTEGER.

-- TRH  13 SEPT 82

with Report; use Report;

procedure C87b07a is

   type Natural is new Integer range 1 .. Integer'Last;
   type Whole is new Integer range 0 .. Integer'Last;
   type Color is (Brown, Red, White);
   type School is (Harvard, Brown, Yale);
   type Sugar is (Dextrose, Cane, Brown);

   function "+" (X, Y : Natural) return Natural renames "*";
   function "+" (X, Y : Whole) return Whole renames "-";

begin
   Test ("C87B07A", "OVERLOADED OPERANDS TO THE 'POS' ATTRIBUTE");

   if Natural'Pos (1 + 1) /= 1 or
     Color'Pos (Brown) /= 0 or
     Whole'Pos (1 + 1) /= 0 or
     School'Pos (Brown) /= 1 or
     Integer'Pos (1 + 1) /= 2 or
     Sugar'Pos (Brown) /= 2
   then
      Failed ("RESOLUTION INCORRECT FOR OPERAND TO 'POS' ATTRIBUTE");
   end if;

   if Natural'Pos (3 + 3) + 1 /= 10 or    -- SECOND "+" IS UNIVERSAL.
     Whole'Pos (3 + 3) + 1 /= 1 or    -- SECOND "+" IS UNIVERSAL.
     Integer'Pos (3 + 3) + 1 /= 7
   then  -- SECOND "+" IS UNIVERSAL.
      Failed
        ("RESOLUTION INCORRECT - 'POS' ATTRIBUTE RETURNS " &
         "A UNIVERSAL_INTEGER VALUE");
   end if;

   Result;
end C87b07a;
