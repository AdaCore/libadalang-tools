-- C35A07A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE FIRST AND LAST ATTRIBUTES YIELD
-- CORRECT VALUES.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/25/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure C35a07a is

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
   -- 'MANTISSA VALUE.

   type Middle_M3 is delta 0.5 range 0.0 .. 2.5;
   type Middle_M15 is delta 2.0**(-6) range -512.0 .. 512.0;
   type Like_Duration_M23 is delta 0.020 range -86_400.0 .. 86_400.0;
   type Decimal_M18 is delta 0.1 range -10_000.0 .. 10_000.0;
   type Decimal_M4 is delta 100.0 range -1_000.0 .. 1_000.0;
   -- LARGEST MODEL NUMBER IS 960.0.

   -------------------------------------------------------------------

   subtype St_Left_Edge_M6 is
     Middle_M15 delta 2.0**(-6) range Ident_Int (1) * (-1.0) .. 1.0;
   subtype St_Middle_M3 is Like_Duration_M23 delta 0.5 range 0.0 .. 2.5;
   subtype St_Decimal_M7 is Decimal_M18 delta 10.0 range -1_000.0 .. 1_000.0;
   -- LARGEST MODEL NUMBER IS 1016.0.
   subtype St_Decimal_M3 is Decimal_M4 delta 100.0 range -500.0 .. 500.0;
   -- LARGEST MODEL NUMBER IS 448.0.
   subtype St_Middle_M15 is Middle_M15 range 6.0 .. 3.0;

begin

   Test
     ("C35A07A",
      "CHECK THAT FOR FIXED POINT TYPES THE FIRST " &
      "AND LAST ATTRIBUTES YIELD CORRECT VALUES - " &
      "BASIC TYPES");

   -------------------------------------------------------------------

   if Middle_M3'First /= Ident_Int (1) * 0.0 then
      Failed ("MIDDLE_M3'FIRST /= 0.0");
   end if;
   if Middle_M3'Last /= Ident_Int (1) * 2.5 then
      Failed ("MIDDLE_M3'LAST /= 2.5");
   end if;

   -------------------------------------------------------------------

   if Like_Duration_M23'First /= Ident_Int (1) * (-86_400.0) then
      Failed ("LIKE_DURATION_M23'FIRST /= -86_400.0");
   end if;
   if Like_Duration_M23'Last /= Ident_Int (1) * 86_400.0 then
      Failed ("LIKE_DURATION_M23'LAST  /=  86_400.0");
   end if;

   -------------------------------------------------------------------

   if Decimal_M18'First /= Ident_Int (1) * (-10_000.0) then
      Failed ("DECIMAL_M18'FIRST /= -10_000.0");
   end if;
   if Decimal_M18'Last /= Ident_Int (1) * 10_000.0 then
      Failed ("DECIMAL_M18'LAST /= 10_000.0");
   end if;

   -------------------------------------------------------------------

   if St_Middle_M3'First /= Ident_Int (1) * 0.0 then
      Failed ("ST_MIDDLE_M3'FIRST /= 0.0");
   end if;
   if St_Middle_M3'Last /= Ident_Int (1) * 2.5 then
      Failed ("ST_MIDDLE_M3'LAST /= 2.5");
   end if;

   -------------------------------------------------------------------

   if St_Decimal_M7'First /= Ident_Int (1) * (-1_000.0) then
      Failed ("ST_DECIMAL_M7'FIRST /= -1000.0");
   end if;
   if St_Decimal_M7'Last /= Ident_Int (1) * 1_000.0 then
      Failed ("ST_DECIMAL_M7'LAST /= 1000.0");
   end if;

   -------------------------------------------------------------------

   if St_Middle_M15'First /= Ident_Int (1) * 6.0 then
      Failed ("ST_MIDDLE_M15'FIRST /= 6.0");
   end if;
   if St_Middle_M15'Last /= Ident_Int (1) * 3.0 then
      Failed ("ST_MIDDLE_M15'LAST /= 3.0");
   end if;

   -------------------------------------------------------------------

   Result;

end C35a07a;
