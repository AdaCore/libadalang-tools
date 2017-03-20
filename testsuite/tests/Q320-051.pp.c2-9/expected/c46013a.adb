-- C46013A.ADA

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
--     CHECK THAT INTEGER CONVERSIONS ARE PERFORMED CORRECTLY WHEN THE
--     OPERAND TYPE IS A FIXED POINT TYPE.

-- HISTORY:
--     JET 02/09/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C46013a is

   type Fix1 is delta 2#0.01# range -16#20.0# .. 16#20.0#;
   type Fix2 is delta 2#0.0001# range -16#80.0# .. 16#80.0#;
   type Fix3 is delta 2#0.0000_01# range -16#200.0# .. 16#200.0#;
   type Fix4 is new Fix1;

   F1 : Fix1 := 7.75;
   F2 : Fix2 := -111.25;
   F3 : Fix3 := 0.875;
   F4 : Fix4 := -15.25;

   type Int is range -512 .. 512;

   function Ident (I : Int) return Int is
   begin
      return I * Int (Ident_Int (1));
   end Ident;

begin
   Test
     ("C46013A",
      "CHECK THAT INTEGER CONVERSIONS ARE PERFORMED " &
      "CORRECTLY WHEN THE OPERAND TYPE IS A FIXED " &
      "POINT TYPE");

   if Integer (Fix1'(-7.25)) /= Ident_Int (-7) then
      Failed ("INCORRECT VALUE (1)");
   end if;

   if Integer (Fix1'(6.75)) /= Ident_Int (7) then
      Failed ("INCORRECT VALUE (2)");
   end if;

   if Integer (F1) /= Ident_Int (8) then
      Failed ("INCORRECT VALUE (3)");
   end if;

   if Int (Fix1'(-7.25)) /= Ident (-7) then
      Failed ("INCORRECT VALUE (4)");
   end if;

   if Integer (Fix1'(3.33)) /= Ident_Int (3) and
     Integer (Fix1'(3.33)) /= Ident_Int (4)
   then
      Failed ("INCORRECT VALUE (5)");
   end if;

   if Integer (Fix1'(-2.5)) = Ident_Int (-2) and
     Integer (Fix1'(-1.5)) = Ident_Int (-1) and
     Integer (Fix1'(1.5)) = Ident_Int (2) and
     Integer (Fix1'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX1 HALF VALUES ROUND UP");
   elsif Integer (Fix1'(-2.5)) = Ident_Int (-3) and
     Integer (Fix1'(-1.5)) = Ident_Int (-2) and
     Integer (Fix1'(1.5)) = Ident_Int (1) and
     Integer (Fix1'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX1 HALF VALUES ROUND DOWN");
   elsif Integer (Fix1'(-2.5)) = Ident_Int (-2) and
     Integer (Fix1'(-1.5)) = Ident_Int (-2) and
     Integer (Fix1'(1.5)) = Ident_Int (2) and
     Integer (Fix1'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX1 HALF VALUES ROUND TO EVEN");
   elsif Integer (Fix1'(-2.5)) = Ident_Int (-2) and
     Integer (Fix1'(-1.5)) = Ident_Int (-1) and
     Integer (Fix1'(1.5)) = Ident_Int (1) and
     Integer (Fix1'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX1 HALF VALUES ROUND TOWARD ZERO");
   elsif Integer (Fix1'(-2.5)) = Ident_Int (-3) and
     Integer (Fix1'(-1.5)) = Ident_Int (-2) and
     Integer (Fix1'(1.5)) = Ident_Int (2) and
     Integer (Fix1'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX1 HALF VALUES ROUND AWAY FROM ZERO");
   else
      Comment ("FIX1 HALF VALUES ROUND ERRATICALLY");
   end if;

   if Integer (Fix2'(-127.937_5)) /= Ident_Int (-128) then
      Failed ("INCORRECT VALUE (6)");
   end if;

   if Integer (Fix2'(127.062_5)) /= Ident_Int (127) then
      Failed ("INCORRECT VALUE (7)");
   end if;

   if Integer (F2) /= Ident_Int (-111) then
      Failed ("INCORRECT VALUE (8)");
   end if;

   if Int (Fix2'(-0.25)) /= Ident (0) then
      Failed ("INCORRECT VALUE (9)");
   end if;

   if Integer (Fix2'(66.67)) /= Ident_Int (67) and
     Integer (Fix2'(66.67)) /= Ident_Int (66)
   then
      Failed ("INCORRECT VALUE (10)");
   end if;

   if Integer (Fix2'(-2.5)) = Ident_Int (-2) and
     Integer (Fix2'(-1.5)) = Ident_Int (-1) and
     Integer (Fix2'(1.5)) = Ident_Int (2) and
     Integer (Fix2'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX2 HALF VALUES ROUND UP");
   elsif Integer (Fix2'(-2.5)) = Ident_Int (-3) and
     Integer (Fix2'(-1.5)) = Ident_Int (-2) and
     Integer (Fix2'(1.5)) = Ident_Int (1) and
     Integer (Fix2'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX2 HALF VALUES ROUND DOWN");
   elsif Integer (Fix2'(-2.5)) = Ident_Int (-2) and
     Integer (Fix2'(-1.5)) = Ident_Int (-2) and
     Integer (Fix2'(1.5)) = Ident_Int (2) and
     Integer (Fix2'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX2 HALF VALUES ROUND TO EVEN");
   elsif Integer (Fix2'(-2.5)) = Ident_Int (-2) and
     Integer (Fix2'(-1.5)) = Ident_Int (-1) and
     Integer (Fix2'(1.5)) = Ident_Int (1) and
     Integer (Fix2'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX2 HALF VALUES ROUND TOWARD ZERO");
   elsif Integer (Fix2'(-2.5)) = Ident_Int (-3) and
     Integer (Fix2'(-1.5)) = Ident_Int (-2) and
     Integer (Fix2'(1.5)) = Ident_Int (2) and
     Integer (Fix2'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX2 HALF VALUES ROUND AWAY FROM ZERO");
   else
      Comment ("FIX2 HALF VALUES ROUND ERRATICALLY");
   end if;

   if Integer (Fix3'(-0.25)) /= Ident_Int (0) then
      Failed ("INCORRECT VALUE (11)");
   end if;

   if Integer (Fix3'(511.75)) /= Ident_Int (512) then
      Failed ("INCORRECT VALUE (12)");
   end if;

   if Integer (F3) /= Ident_Int (1) then
      Failed ("INCORRECT VALUE (13)");
   end if;

   if Int (Fix3'(-7.0)) /= Ident (-7) then
      Failed ("INCORRECT VALUE (14)");
   end if;

   if Integer (Fix3'(-66.67)) /= Ident_Int (-67) and
     Integer (Fix3'(-66.67)) /= Ident_Int (-66)
   then
      Failed ("INCORRECT VALUE (15)");
   end if;

   if Integer (Fix3'(-2.5)) = Ident_Int (-2) and
     Integer (Fix3'(-1.5)) = Ident_Int (-1) and
     Integer (Fix3'(1.5)) = Ident_Int (2) and
     Integer (Fix3'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX3 HALF VALUES ROUND UP");
   elsif Integer (Fix3'(-2.5)) = Ident_Int (-3) and
     Integer (Fix3'(-1.5)) = Ident_Int (-2) and
     Integer (Fix3'(1.5)) = Ident_Int (1) and
     Integer (Fix3'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX3 HALF VALUES ROUND DOWN");
   elsif Integer (Fix3'(-2.5)) = Ident_Int (-2) and
     Integer (Fix3'(-1.5)) = Ident_Int (-2) and
     Integer (Fix3'(1.5)) = Ident_Int (2) and
     Integer (Fix3'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX3 HALF VALUES ROUND TO EVEN");
   elsif Integer (Fix3'(-2.5)) = Ident_Int (-2) and
     Integer (Fix3'(-1.5)) = Ident_Int (-1) and
     Integer (Fix3'(1.5)) = Ident_Int (1) and
     Integer (Fix3'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX3 HALF VALUES ROUND TOWARD ZERO");
   elsif Integer (Fix3'(-2.5)) = Ident_Int (-3) and
     Integer (Fix3'(-1.5)) = Ident_Int (-2) and
     Integer (Fix3'(1.5)) = Ident_Int (2) and
     Integer (Fix3'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX3 HALF VALUES ROUND AWAY FROM ZERO");
   else
      Comment ("FIX3 HALF VALUES ROUND ERRATICALLY");
   end if;

   if Integer (Fix4'(-7.25)) /= Ident_Int (-7) then
      Failed ("INCORRECT VALUE (16)");
   end if;

   if Integer (Fix4'(6.75)) /= Ident_Int (7) then
      Failed ("INCORRECT VALUE (17)");
   end if;

   if Integer (F4) /= Ident_Int (-15) then
      Failed ("INCORRECT VALUE (18)");
   end if;

   if Int (Fix4'(-31.75)) /= Ident (-32) then
      Failed ("INCORRECT VALUE (19)");
   end if;

   if Integer (Fix4'(3.33)) /= Ident_Int (3) and
     Integer (Fix4'(3.33)) /= Ident_Int (4)
   then
      Failed ("INCORRECT VALUE (20)");
   end if;

   if Integer (Fix4'(-2.5)) = Ident_Int (-2) and
     Integer (Fix4'(-1.5)) = Ident_Int (-1) and
     Integer (Fix4'(1.5)) = Ident_Int (2) and
     Integer (Fix4'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX4 HALF VALUES ROUND UP");
   elsif Integer (Fix4'(-2.5)) = Ident_Int (-3) and
     Integer (Fix4'(-1.5)) = Ident_Int (-2) and
     Integer (Fix4'(1.5)) = Ident_Int (1) and
     Integer (Fix4'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX4 HALF VALUES ROUND DOWN");
   elsif Integer (Fix4'(-2.5)) = Ident_Int (-2) and
     Integer (Fix4'(-1.5)) = Ident_Int (-2) and
     Integer (Fix4'(1.5)) = Ident_Int (2) and
     Integer (Fix4'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX4 HALF VALUES ROUND TO EVEN");
   elsif Integer (Fix4'(-2.5)) = Ident_Int (-2) and
     Integer (Fix4'(-1.5)) = Ident_Int (-1) and
     Integer (Fix4'(1.5)) = Ident_Int (1) and
     Integer (Fix4'(2.5)) = Ident_Int (2)
   then
      Comment ("FIX4 HALF VALUES ROUND TOWARD ZERO");
   elsif Integer (Fix4'(-2.5)) = Ident_Int (-3) and
     Integer (Fix4'(-1.5)) = Ident_Int (-2) and
     Integer (Fix4'(1.5)) = Ident_Int (2) and
     Integer (Fix4'(2.5)) = Ident_Int (3)
   then
      Comment ("FIX4 HALF VALUES ROUND AWAY FROM ZERO");
   else
      Comment ("FIX4 HALF VALUES ROUND ERRATICALLY");
   end if;

   Result;

end C46013a;
