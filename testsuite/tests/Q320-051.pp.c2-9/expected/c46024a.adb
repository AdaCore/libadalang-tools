-- C46024A.ADA

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
--     CHECK FLOATING POINT CONVERSIONS WHEN THE TARGET TYPE IS A
--     FIXED POINT TYPE, FOR DIGITS 5.

-- HISTORY:
--     JET 02/19/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C46024a is

   type Float5 is digits 5;
   type Fix1 is delta 2#0.01# range -16#20.0# .. 16#20.0#;
   type Fix2 is delta 2#0.0001# range -16#80.0# .. 16#80.0#;
   type Fix3 is delta 2#0.0000_01# range -16#200.0# .. 16#200.0#;

   F5, F5a, F5b : Float5;

   generic
      type F is delta <>;
   function Identg (A : F) return F;

   function Identg (A : F) return F is
   begin
      return A + F (Ident_Int (0));
   end Identg;

   function Ident1 is new Identg (Fix1);
   function Ident2 is new Identg (Fix2);
   function Ident3 is new Identg (Fix3);

begin
   Test
     ("C46024A",
      "CHECK FLOATING POINT CONVERSIONS WHEN THE " &
      "TARGET TYPE IS A FIXED POINT TYPE, FOR " &
      "5-DIGIT PRECISION");

   if Fix1 (Float5'(2#0.1000_0000_0000_0000_00#E-1)) /= Ident1 (2#0.01#) then
      Failed ("INCORRECT RESULT FROM CONVERSION (1)");
   end if;

   if Fix1 (Float5'(-2#0.1111_1110_0000_0000_00#E5)) /=
     Ident1 (-2#1_1111.11#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (2)");
   end if;

   if Fix1 (Float5'(-2#0.1010_0111_1111_1111_11#E4)) < Ident1 (-2#1010.10#) or
     Fix1 (Float5'(-2#0.1010_0111_1111_1111_11#E4)) > Ident1 (-2#1010.01#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (3)");
   end if;

   if Fix2 (Float5'(-2#0.1000_0000_0000_0000_00#E-3)) /=
     Ident2 (-2#0.0001#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (4)");
   end if;

   if Fix2 (Float5'(2#0.1111_1111_1110_0000_00#E7)) /=
     Ident2 (2#111_1111.1111#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (5)");
   end if;

   F5 := 2#0.1010_1010_1010_1010_10#E5;
   if Fix2 (F5) < Ident2 (2#1_0101.0101#) or
     Fix2 (F5) > Ident2 (2#1_0101.0110#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (6)");
   end if;

   if Fix3 (Float5'(2#0.1000_0000_0000_0000_00#E-5)) /=
     Ident3 (2#0.0000_01#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (7)");
   end if;

   if Fix3 (Float5'(-2#0.1111_1111_1111_1110_00#E9)) /=
     Ident3 (-2#1_1111_1111.1111_11#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (8)");
   end if;

   F5 := -2#0.1010_1010_1010_1010_10#E8;
   if Fix3 (F5) < Ident3 (-2#1010_1010.1010_11#) or
     Fix3 (F5) > Ident3 (-2#1010_1010.1010_10#)
   then
      Failed ("INCORRECT RESULT FROM CONVERSION (9)");
   end if;

   F5a := 2#0.1010_1010_1010_1010_10#E4;
   F5b := 2#0.1010_1010_1010_1010_10#E5;

   if Fix1 (F5a) = Ident1 (2#1010.11#) and
     Fix1 (-F5a) = Ident1 (-2#1010.11#) and
     Fix1 (F5b) = Ident1 (2#1_0101.01#) and
     Fix1 (-F5b) = Ident1 (-2#1_0101.01#)
   then
      Comment ("CONVERSION ROUNDS TO NEAREST");
   elsif Fix1 (F5a) = Ident1 (2#1010.10#) and
     Fix1 (-F5b) = Ident1 (-2#1_0101.10#)
   then
      Comment ("CONVERSION ROUNDS TO LEAST FIXED-POINT VALUE");
   elsif Fix1 (F5b) = Ident1 (2#1_0101.10#) and
     Fix1 (-F5a) = Ident1 (-2#1010.10#)
   then
      Comment ("CONVERSION ROUNDS TO GREATEST FIXED-POINT VALUE");
   elsif Fix1 (F5a) = Ident1 (2#1010.10#) and
     Fix1 (-F5a) = Ident1 (-2#1010.10#)
   then
      Comment ("CONVERSION ROUNDS TOWARD ZERO");
   elsif Fix1 (F5b) = Ident1 (2#1_0101.10#) and
     Fix1 (-F5b) = Ident1 (-2#1_0101.10#)
   then
      Comment ("CONVERSION ROUNDS AWAY FROM ZERO");
   else
      Comment ("UNABLE TO DETERMINE CONVERSION PATTERN");
   end if;

   Result;

end C46024a;
