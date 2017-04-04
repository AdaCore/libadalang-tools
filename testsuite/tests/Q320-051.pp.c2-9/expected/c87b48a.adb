-- C87B48A.ADA

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
-- NAMED ACTUAL PARAMETERS CAN RESOLVE OVERLOADING OF SUBPROGRAMS. THIS TEST
-- USES FUNCTIONS AND OPERATOR SYMBOLS ONLY.

-- TRH  13 AUG 82

with Report; use Report;

procedure C87b48a is

   Err, B1, B2 : Boolean := False;

   package A is
      function "-" (X : Boolean) return Boolean;
      function Toggle (X : Boolean) return Boolean renames "-";
   end A;

   package body A is
      function "-" (X : Boolean) return Boolean is
      begin
         return not X;
      end "-";
   end A;

   package B is
      function "-" (Y : Boolean) return Boolean;
      function Toggle (Y : Boolean) return Boolean renames "-";
   end B;

   package body B is
      function "-" (Y : Boolean) return Boolean is
      begin
         Err := True;
         return not Y;
      end "-";
   end B;

   package C is
      function "-" (Z : Boolean) return Boolean;
      function Toggle (Z : Boolean) return Boolean renames "-";
   end C;

   package body C is
      function "-" (Z : Boolean) return Boolean is
      begin
         Err := True;
         return not Z;
      end "-";
   end C;

   use A, B, C;

begin
   Test
     ("C87B48A",
      "RESOLUTION OF OVERLOADED SUBPROGRAMS BY NAMED " & "ACTUAL PARAMETERS");

   B1 := "-" (X => False);
   B2 := Toggle (X => False);

   if Err or else not B1 or else not B2 then
      Failed
        ("RESOLUTION INCORRECT FOR OVERLOADED SUBPROGRAMS" &
         " WITH NAMED ACTUAL PARAMETERS");
   end if;

   Result;
end C87b48a;
