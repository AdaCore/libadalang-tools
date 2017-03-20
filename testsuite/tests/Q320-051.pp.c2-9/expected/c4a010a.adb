-- C4A010A.ADA

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
-- CHECK THAT STATIC UNIVERSAL_REAL EXPRESSIONS ARE EVALUATED EXACTLY.

-- SMALL RATIONAL NUMBERS ARE USED IN THIS TEST.

-- JBG 5/3/85

with Report; use Report;
procedure C4a010a is

   C13  : constant := 1.0 / 3.0;
   C47  : constant := 4.0 / 7.0;
   C112 : constant := 13.0 / 12.0;
   Half : constant := 3.5 / 7.0;

begin

   Test
     ("C4A010A",
      "CHECK STATIC UNIVERSAL_REAL ACCURACY FOR " & "SMALL RATIONAL NUMBERS");

   if C13 - C47 /= -5.0 / 21.0 then
      Failed ("REAL SUBTRACTION RESULT INCORRECT");
   end if;

   if C47 + C112 = 1.0 + 55.0 / 84.0 then
      null;
   else
      Failed ("REAL ADDITION RESULT INCORRECT");
   end if;

   if C112 - C13 /= 6.0 / 8.0 then
      Failed ("LCD NOT FOUND");
   end if;

   if 0.1 * 0.1 /= 0.01 then
      Failed ("REAL MULTIPLICATION RESULT INCORRECT");
   end if;

   if C112 / C13 /= 13.0 / 4 then
      Failed ("REAL QUOTIENT RESULT INCORRECT");
   end if;

   if 0.1**4 /= 0.000_1 then
      Failed ("POSITIVE EXPONENTIATION RESULT INCORRECT");
   end if;

   if C13**(-3) /= 27.0 * 0.5 * 2 then
      Failed ("NEGATIVE EXPONENTIATION RESULT INCORRECT");
   end if;

   if Half /= 0.1 / 0.2 then
      Failed ("FRACTIONAL NUMERATOR AND DENOMINATOR");
   end if;

   Result;

end C4a010a;
