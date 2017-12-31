-- C24003C.ADA

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
-- CHECK THAT LEADING ZEROES IN INTEGRAL PARTS AND TRAILING ZEROES IN
-- FRACTIONAL PARTS OF FIXED POINT LITERALS ARE IGNORED.

-- JRK 12/12/79
-- JRK 12/16/80
-- TBN 10/21/85 RENAMED FROM C24003C.TST AND FIXED LINE LENGTH. DTN 11/12/91
-- DELETED SUBPART (B). CHANGED EXTENSION FROM '.TST'
--                  TO '.ADA'.

with Report;
procedure C24003c is

   use Report;

   type Fixed is delta 1.0 range 0.0 .. 1_000.0;
   Fx : Fixed := 69.0E1;

begin

   Test ("C24003C", "LEADING/TRAILING ZEROES IN " & "FIXED POINT LITERALS");

   if 000_000_000_000_000_000_000_000_000_000_000_000_000_069.0E1 /= Fx then
      Failed
        ("LEADING ZEROES IN INTEGRAL PART OF FIXED " &
         "POINT LITERAL NOT IGNORED");
   end if;

   if 69.000_000_000_000_000_000_000_000_000_000_000_000_000_0E1 /= Fx then
      -- MIGHT RAISE NUMERIC_ERROR AT COMPILE-TIME.
      Failed
        ("TRAILING ZEROES IN FRACTIONAL PART OF " &
         "FIXED POINT LITERAL NOT IGNORED");
   end if;

   if 0_000_000_000_000_000_000_000_000_000_000_000_000_000_690.000_00 /= Fx
   then
      Failed
        ("LEADING/TRAILING ZEROES IN MANTISSA OF " &
         "FIXED POINT LITERAL NOT IGNORED");
   end if;

   if 69.0E00000000000000000000000000000000000000001 /= Fx then
      Failed
        ("LEADING ZEROES IN EXPONENT OF FIXED " & "POINT LITERAL NOT IGNORED");
   end if;

   if 16#00_0000_0000_0000_0000_0000_0000_0000_0000_0000_002B.2#E1 /= Fx then
      Failed ("LEADING ZEROES IN BASED FIXED POINT " & "LITERAL NOT IGNORED");
   end if;

   if 16#2B.2000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0#E1 /= Fx then
      Failed ("TRAILING ZEROES IN BASED FIXED POINT " & "LITERAL NOT IGNORED");
   end if;

   Result;
end C24003c;
