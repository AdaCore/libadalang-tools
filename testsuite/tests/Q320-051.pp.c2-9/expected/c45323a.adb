-- C45323A.ADA

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
--     CHECK THAT THE NONASSOCIATIVITY OF REAL ARITHMETIC IS PRESERVED
--     FOR FLOATING POINT PRECISION 5, EVEN WHEN OPTIMIZATION WOULD
--     BENEFIT IF FLOATING POINT ADDITION WERE ASSOCIATIVE.

-- HISTORY:
--     JET 08/10/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C45323a is

   type Float5 is digits 5;

   A, B, C, D, E : Float5;

   function Ident (F : Float5) return Float5 is
   begin
      return F * Float5 (Ident_Int (1));
   end Ident;

begin
   Test
     ("C45323A",
      "CHECK THAT THE NONASSOCIATIVITY OF REAL " &
      "ARITHMETIC IS PRESERVED FOR FLOATING POINT " &
      "PRECISION 5, EVEN WHEN OPTIMIZATION WOULD " &
      "BENEFIT IF FLOATING POINT ADDITION WERE " & "ASSOCIATIVE");

   B := 2#0.1010_1010_1010_1010_10#E3;
   A := -B;
   C := 2#0.1000_0000_0000_0000_00#E-18;
   D := B + C;
   E := A + B + C;

   if Ident (A) + Ident (B) /= 0.0 then
      Failed ("INCORRECT VALUE OF A + B");
   end if;

   if Ident (E) /= Ident (C) then
      Failed ("C DOES NOT EQUAL E");
   end if;

   Result;
end C45323a;
