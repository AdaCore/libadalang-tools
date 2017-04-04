-- C4A014A.ADA

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
-- CHECK THAT ROUNDING IS DONE CORRECTLY FOR STATIC UNIVERSAL REAL EXPRESSIONS.

-- JBG 5/3/85
-- JBG 11/3/85 DECLARE INTEGER CONSTANTS INSTEAD OF UNIVERSAL INTEGER DTN
-- 11/27/91 DELETED SUBPART (B).

with Report; use Report;
procedure C4a014a is

   C15  : constant := 1.5;
   C25  : constant := 2.5;
   Cn15 : constant := -1.5;
   Cn25 : constant := -2.5;

   C15r  : constant Integer := Integer (C15);
   C25r  : constant Integer := Integer (C25);
   Cn15r : constant Integer := Integer (Cn15);
   Cn25r : constant Integer := Integer (Cn25);

   C15_1 : Boolean := 1 = C15r;
   C15_2 : Boolean := 2 = C15r;
   C25_2 : Boolean := 2 = C25r;
   C25_3 : Boolean := 3 = C25r;

   Cn15_N1 : Boolean := -1 = Cn15r;
   Cn15_N2 : Boolean := -2 = Cn15r;
   Cn25_N2 : Boolean := -2 = Cn25r;
   Cn25_N3 : Boolean := -3 = Cn25r;

begin

   Test
     ("C4A014A",
      "CHECK ROUNDING TO INTEGER FOR UNIVERSAL REAL " & "EXPRESSIONS");

   if 1 /= Integer (1.4) then
      Failed ("INTEGER(1.4) DOES NOT EQUAL 1");
   end if;

   if 2 /= Integer (1.6) then
      Failed ("INTEGER(1.6) DOES NOT EQUAL 2");
   end if;

   if -1 /= Integer (-1.4) then
      Failed ("INTEGER(-1.4) DOES NOT EQUAL -1");
   end if;

   if -2 /= Integer (-1.6) then
      Failed ("INTEGER(-1.6) DOES NOT EQUAL -2");
   end if;

   if not (C15_1 or C15_2) or (not (C25_2 or C25_3)) then
      Failed ("ROUNDING OF POSITIVE VALUES NOT CORRECT");
   end if;

   if not (Cn15_N1 or Cn15_N2) or (not (Cn25_N2 or Cn25_N3)) then
      Failed ("ROUNDING OF NEGATIVE VALUES NOT CORRECT");
   end if;

   Result;

end C4a014a;
