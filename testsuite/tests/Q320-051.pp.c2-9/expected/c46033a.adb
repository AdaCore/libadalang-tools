-- C46033A.ADA

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
--     IS ANOTHER FIXED POINT TYPE.

-- HISTORY:
--     JET 07/12/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C46033a is

   type Fix1 is delta 2#0.01# range -16#20.0# .. 16#20.0#;
   type Fix2 is delta 2#0.0001# range -16#80.0# .. 16#80.0#;
   type Fix3 is delta 2#0.0000_01# range -16#200.0# .. 16#200.0#;

   F1 : Fix1;
   F2 : Fix2;
   F3 : Fix3;

   generic
      type F is delta <>;
   function Ident_G (X : F) return F;

   function Ident_G (X : F) return F is
   begin
      return X + F (Ident_Int (0));
   end Ident_G;

   function Ident is new Ident_G (Fix1);
   function Ident is new Ident_G (Fix2);
   function Ident is new Ident_G (Fix3);

begin
   Test
     ("C46033A",
      "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
      "THE OPERAND TYPE IS ANOTHER FIXED POINT TYPE");

   F1 := Ident (-16#1F.C#);
   if Fix1 (F1) /= -16#1F.C# then
      Failed ("INCORRECT RESULT FROM CONVERSION (1)");
   end if;

   F1 := Ident (16#0.4#);
   if Fix2 (F1) /= 16#0.4# then
      Failed ("INCORRECT RESULT FROM CONVERSION (2)");
   end if;

   F1 := Ident (-16#10.4#);
   if Fix3 (F1) /= -16#10.4# then
      Failed ("INCORRECT RESULT FROM CONVERSION (3)");
   end if;

   F2 := Ident (16#3.3#);
   if Fix1 (F2) < 16#3.0# or Fix1 (F2) > 16#3.4# then
      Failed ("INCORRECT RESULT FROM CONVERSION (4)");
   end if;

   F2 := Ident (-16#40.1#);
   if Fix2 (F2) /= -16#40.1# then
      Failed ("INCORRECT RESULT FROM CONVERSION (5)");
   end if;

   F2 := Ident (16#0.0#);
   if Fix3 (F2) /= 16#0.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (6)");
   end if;

   F3 := Ident (-16#0.04#);
   if Fix1 (F3) < -16#0.4# or Fix1 (F3) > -16#0.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (7)");
   end if;

   F3 := -Ident (16#55.A8#);
   if Fix2 (F3) < -16#55.B# or Fix2 (F3) > -16#55.A# then
      Failed ("INCORRECT RESULT FROM CONVERSION (8)");
   end if;

   F3 := Ident (16#101.84#);
   if Fix3 (F3) /= 16#101.84# then
      Failed ("INCORRECT RESULT FROM CONVERSION (9)");
   end if;

   Result;

end C46033a;
