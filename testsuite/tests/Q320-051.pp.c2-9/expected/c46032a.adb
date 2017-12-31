-- C46032A.ADA

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
--     CHECK CONVERSIONS TO FIXED POINT TYPES WHEN THE OPERAND TYPE
--     IS A FLOATING POINT TYPE OF 5 DIGITS PRECISION.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C46032a is

   type Fix1 is delta 2#0.01# range -16#20.0# .. 16#20.0#;
   type Fix2 is delta 2#0.0001# range -16#80.0# .. 16#80.0#;
   type Fix3 is delta 2#0.0000_01# range -16#200.0# .. 16#200.0#;

   type Float5 is digits 5;

   F5 : Float5;

   function Ident5 (X : Float5) return Float5 is
   begin
      return X * Float5 (Ident_Int (1));
   end Ident5;

begin
   Test
     ("C46032A",
      "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
      "THE OPERAND TYPE IS A FLOATING POINT TYPE " & "OF 5 DIGITS PRECISION");

   F5 := Ident5 (2#0.1100_0000_0000_0000_00#E0);
   if Fix1 (F5) /= 16#0.C# then
      Failed ("INCORRECT RESULT FROM CONVERSION (1)");
   end if;

   F5 := Ident5 (2#0.1111_1110_0000_0000_00#E5);
   if Fix1 (F5) /= 16#1F.C# then
      Failed ("INCORRECT RESULT FROM CONVERSION (2)");
   end if;

   F5 := Ident5 (-2#0.1010_1010_1010_1010_10#E2);
   if Fix1 (F5) < -16#2.C# or Fix1 (F5) > -16#2.8# then
      Failed ("INCORRECT RESULT FROM CONVERSION (3)");
   end if;

   F5 := Ident5 (2#0.1111_0000_0000_0000_00#E0);
   if Fix2 (F5) /= 16#0.F# then
      Failed ("INCORRECT RESULT FROM CONVERSION (4)");
   end if;

   F5 := Ident5 (-2#0.1111_1110_0000_0000_00#E7);
   if Fix2 (F5) /= -16#7F.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (5)");
   end if;

   F5 := Ident5 (2#0.1111_1111_1101_0000_00#E7);
   if Fix2 (F5) < 16#7F.E# or Fix2 (F5) > 16#7F.F# then
      Failed ("INCORRECT RESULT FROM CONVERSION (6)");
   end if;

   F5 := Ident5 (2#0.1000_0000_0000_0000_00#E-5);
   if Fix3 (F5) /= 16#0.04# then
      Failed ("INCORRECT RESULT FROM CONVERSION (7)");
   end if;

   F5 := -Ident5 (2#0.1010_1010_1010_1010_00#E9);
   if Fix3 (F5) /= -16#155.54# then
      Failed ("INCORRECT RESULT FROM CONVERSION (8)");
   end if;

   F5 := Ident5 (2#0.1000_0000_0000_0010_11#E9);
   if Fix3 (F5) < 16#100.04# or Fix3 (F5) > 16#100.08# then
      Failed ("INCORRECT RESULT FROM CONVERSION (9)");
   end if;

   Result;

end C46032a;
