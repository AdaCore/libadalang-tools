-- C46031A.ADA

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
--     IS AN INTEGER TYPE.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C46031a is

   type Fix1 is delta 2#0.01# range -16#20.0# .. 16#20.0#;
   type Fix2 is delta 2#0.0001# range -16#80.0# .. 16#80.0#;
   type Fix3 is delta 2#0.0000_01# range -16#200.0# .. 16#200.0#;

   type New_Int is new Integer range -16#200# .. 16#200#;

   I : Integer;
   J : New_Int;

   function Ident_New (X : New_Int) return New_Int is
   begin
      return X * New_Int (Ident_Int (1));
   end Ident_New;

begin
   Test
     ("C46031A",
      "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
      "THE OPERAND TYPE IS AN INTEGER TYPE");

   I := Ident_Int (-16#1F#);
   if Fix1 (I) /= -16#1F.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (1)");
   end if;

   J := Ident_New (0);
   if Fix1 (J) /= 0.0 then
      Failed ("INCORRECT RESULT FROM CONVERSION (2)");
   end if;

   I := Ident_Int (16#7F#);
   if Fix2 (I) /= 16#7F.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (3)");
   end if;

   J := Ident_New (16#1#);
   if Fix2 (J) /= 16#1.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (4)");
   end if;

   I := Ident_Int (-16#55#);
   if Fix3 (I) /= -16#55.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (5)");
   end if;

   J := Ident_New (-16#1#);
   if Fix3 (J) /= -16#1.0# then
      Failed ("INCORRECT RESULT FROM CONVERSION (6)");
   end if;

   Result;

end C46031a;
