-- C87B35C.ADA

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
-- THE EXPONENT OPERAND OF A FLOATING POINT EXPONENTIATION MUST BE
-- OF THE TYPE PREDEFINED INTEGER.

-- TRH  4 AUG 82

with Report; use Report;

procedure C87b35c is

   type Fixed is delta 0.01 range 0.0 .. 4.0;
   Err : Boolean := False;

   function F1 (X : Integer) return Integer is
   begin
      return X;
   end F1;

   function F1 (X : Integer) return Float is
   begin
      Err := True;
      return 1.0;
   end F1;

   function F1 (X : Integer) return Fixed is
   begin
      Err := True;
      return 1.0;
   end F1;

begin
   Test
     ("C87B35C",
      "EXPONENT OPERAND FOR FLOATING POINT " &
      "EXPONENTIATION MUST BE OF TYPE PREDEFINED INTEGER");

   declare
      function "+" (X, Y : Integer) return Integer renames Standard."*";

   begin
      if (Float'(2.0)**F1 (3) /= 8.0 or Float'(2.0)**(3 + 1) /= 8.0) then
         Failed
           ("EXPONENT OF FLOATING POINT EXPONENTIATION " &
            "MUST BE PREDEFINED INTEGER (A)");
      end if;
      if (2.0**F1 (3) /= Float'(8.0) or 2.0**(3 + 1) /= Float'(8.0)) then
         Failed
           ("EXPONENT OF FLOATING POINT EXPONENTIATION" &
            "MUST BE PREDEFINED INTEGER (B)");
      end if;
      if Err then
         Failed
           ("EXPONENT OF FLOATING POINT EXPONENTIATION" &
            "MUST BE PREDEFINED INTEGER (C)");
      end if;
   end;

   Result;
end C87b35c;
