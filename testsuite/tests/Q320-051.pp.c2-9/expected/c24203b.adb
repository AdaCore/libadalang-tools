-- C24203B.ADA

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
--     CHECK THAT BASED REAL LITERALS WITH BASES 2 THROUGH 16 ALL
--     YIELD CORRECT VALUES.

--     THIS TEST USES MODEL NUMBERS OF DIGITS 6.

-- HISTORY:
--     DHH 06/15/88 CREATED ORIGINAL TEST.
--     DTN 11/30/95 REMOVED CONFORMANCE CHECKS WHERE RULES RELAXED.

with Report; use Report;
procedure C24203b is

   type Check is digits 6;

begin
   Test
     ("C24203B",
      "CHECK THAT BASED REAL LITERALS WITH BASES " &
      "2 THROUGH 16 ALL YIELD CORRECT VALUES");

   if 2#0.0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1# /=
     2.0**(-61)
   then
      Failed ("INCORRECT VALUE FOR BASE 2 REAL LITERAL");
   end if;

   if 3#0.0000_0000_001# < ((2.0**(-18)) + (251_558.0 * (2.0**(-37)))) or
     3#0.0000_0000_001# > ((2.0**(-18)) + (251_559.0 * (2.0**(-37))))
   then
      Failed ("INCORRECT VALUE FOR BASE 3 REAL LITERAL");
   end if;

   if 4#1333_3333.213# /= 32_767.609_375 then
      Failed ("INCORRECT VALUE FOR BASE 4 REAL LITERAL");
   end if;

   if 5#202_1444.4241_121# < 32_749.906_25 or
     5#202_1444.4241_121# > 32_749.921_875
   then
      Failed ("INCORRECT VALUE FOR BASE 5 REAL LITERAL");
   end if;

   if 6#41_1355.5310_43# /= 32_759.921_875 then
      Failed ("INCORRECT VALUE FOR BASE 6 REAL LITERAL");
   end if;

   if 7#16_4366.6253_44# < 32_780.906_25 or
     7#16_4366.6253_44# > 32_780.937_5
   then
      Failed ("INCORRECT VALUE FOR BASE 7 REAL LITERAL");
   end if;

   if 8#7_7777.07# /= 32_767.109_375 then
      Failed ("INCORRECT VALUE FOR BASE 8 REAL LITERAL");
   end if;

   if 9#4_8888.8203_14# < 32_804.906_25 or
     9#4_8888.8203_14# > 32_804.937_5
   then
      Failed ("INCORRECT VALUE FOR BASE 9 REAL LITERAL");
   end if;

   if 10#3_2767.9218_75# /= 32_767.921_875 then
      Failed ("INCORRECT VALUE FOR BASE 10 REAL LITERAL");
   end if;

   if 11#2_267A.A066_82# < 32_757.906_25 or
     11#2_267A.A066_82# > 32_757.921_875
   then
      Failed ("INCORRECT VALUE FOR BASE 11 REAL LITERAL");
   end if;

   if 12#1_6B5B.B09# /= 32_759.921_875 then
      Failed ("INCORRECT VALUE FOR BASE 12 REAL LITERAL");
   end if;

   if 13#1_1B9C.BB61_6# < 32_746.906_25 or
     13#1_1B9C.BB61_6# > 32_746.921_875
   then
      Failed ("INCORRECT VALUE FOR BASE 13 REAL LITERAL");
   end if;

   if 14#BD1D.CC98_A7# /= 32_759.921_875 then
      Failed ("INCORRECT VALUE FOR BASE 14 REAL LITERAL");
   end if;

   if 15#3D_2818_8D45_8811_1111_1111.0# < (((2.0**21) - 2.0) * (2.0**63)) then
      Failed ("INCORRECT VALUE FOR BASE 15 REAL LITERAL");
   end if;

   Result;
end C24203b;
