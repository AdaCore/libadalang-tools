-- C87B24B.ADA

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

-- THE RANGE BOUNDS FOR A SLICE MUST BE DISCRETE AND OF THE SAME BASE
-- TYPE AS THE ARRAY INDEX.

-- TRH  15 JULY 82

with Report; use Report;

procedure C87b24b is

   type Piece is array (Integer range <>) of Integer;

   Pi  : Piece (1 .. 8) := (3, 1, 4, 1, 5, 9, 2, 6);
   S1  : Piece (1 .. 3);
   S2  : Piece (4 .. 8);
   Err : Boolean        := False;

   function F1 (X : Integer) return Integer is
   begin
      return X;
   end F1;

   function F1 (X : Integer) return Float is
   begin
      Err := True;
      return 0.0;
   end F1;

   function F2 (X : Integer) return Integer is
   begin
      return X;
   end F2;

   function F2 (X : Integer) return Character is
   begin
      Err := True;
      return 'A';
   end F2;

begin
   Test
     ("C87B24B",
      "OVERLOADING RESOLUTION OF RANGE " & "CONSTRAINTS FOR SLICES");

   declare
      function "+" (X : Integer) return Integer renames F1;

      function "+" (X : Integer) return Float renames F1;

      function "-" (X : Integer) return Integer renames F2;

      function "-" (X : Integer) return Character renames F2;

   begin
      S1 := Pi ("+" (3) .. "-" (5));
      S1 := Pi (F2 (2) .. "+" (4));
      S1 := Pi ("-" (6) .. F1 (8));
      S1 := Pi (F2 (1) .. F2 (3));
      S2 := Pi (F2 (4) .. F1 (8));
      S2 := Pi (2 .. "+" (6));
      S2 := Pi (F1 (1) .. 5);
      S2 := Pi ("+" (3) .. "+" (7));

      if Err then
         Failed (" OVERLOADING RESOLUTION INCORRECT FOR SLICES");
      end if;
   end;

   Result;
end C87b24b;
