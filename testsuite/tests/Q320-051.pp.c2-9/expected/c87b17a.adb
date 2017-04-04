-- C87B17A.ADA

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

-- THE INITIALIZATION EXPRESSION FOR A DEFAULT DISCRIMINANT IN A TYPE
-- DECLARATION MUST MATCH THE DISCRIMINANT'S EXPLICIT TYPEMARK.
--
-- THE THREE KINDS OF TYPE DECLARATIONS TESTED HERE ARE:
--
--    (A): RECORD TYPE.
--    (B): PRIVATE TYPE.
--    (C): INCOMPLETE RECORD TYPE.

-- TRH  18 JUNE 82

with Report; use Report;

procedure C87b17a is

   type Whole is new Integer range 0 .. Integer'Last;
   type Citrus is (Lemon, Lime, Orange);
   type Hue is (Red, Orange, Yellow);

   function F1 (X, Y : Integer) return Integer is
   begin
      return -1;
   end F1;

   function F1 (X, Y : Whole) return Whole is
   begin
      return 0;
   end F1;

   function F1 (X, Y : Integer) return Hue is
   begin
      return Orange;
   end F1;

   function F1 (X, Y : Integer) return Citrus is
   begin
      return Orange;
   end F1;

begin
   Test
     ("C87B17A",
      "OVERLOADED INITIALIZATION EXPRESSIONS" & " IN DEFAULT DISCRIMINANTS");

   declare

      function "+" (X, Y : Integer) return Integer renames F1;

      function "+" (X, Y : Whole) return Whole renames F1;

      function "+" (X, Y : Integer) return Hue renames F1;

      function "+" (X, Y : Integer) return Citrus renames F1;

      type Rec1 (I1 : Integer := 0 + 0; H1 : Hue := F1 (0, 0)) is record
         null;
      end record;

      package Pvt is
         type Rec2 (H2 : Hue := Orange; W2 : Whole := 0 + 0) is private;
      private
         type Rec2 (H2 : Hue := Orange; W2 : Whole := 0 + 0) is record
            null;
         end record;
      end Pvt;
      use Pvt;

      type Rec3 (C1 : Citrus := Orange; W1 : Whole := "+" (0, 0));

      type Link is access Rec3;

      type Rec3 (C1 : Citrus := Orange; W1 : Whole := "+" (0, 0)) is record
         null;
      end record;

      R1 : Rec1;
      R2 : Rec2;
      R3 : Rec3;

   begin
      if R1.I1 /= -1 or Hue'Pos (R1.H1) /= 1 then
         Failed ("(A): RESOLUTION INCORRECT FOR RECORD TYPES");
      end if;

      if Hue'Pos (R2.H2) /= 1 or R2.W2 /= 0 then
         Failed ("(B): RESOLUTION INCORRECT FOR PRIVATE TYPES");
      end if;

      if Citrus'Pos (R3.C1) /= 2 or R3.W1 /= 0 then
         Failed ("(C): RESOLUTION INCORRECT FOR INCOMPLETE" & " RECORD TYPES");
      end if;
   end;

   Result;
end C87b17a;
