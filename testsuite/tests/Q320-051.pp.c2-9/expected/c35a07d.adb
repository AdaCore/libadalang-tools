-- C35A07D.ADA

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

-- CASE D: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC.

-- WRG 8/25/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
with System; use System;
procedure C35a07d is

   Pi      : constant := 3.14159_26535_89793_23846;
   Two_Pi  : constant := 2 * Pi;
   Half_Pi : constant := Pi / 2;

   Mm : constant := Max_Mantissa;

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
   -- 'MANTISSA VALUE.

   type Pixel_M10 is delta 1.0 / 1_024.0 range 0.0 .. 1.0;
   type Ruler_M8 is delta 1.0 / 16.0 range 0.0 .. 12.0;

   type Hours_M16 is delta 24.0 * 2.0**(-15) range 0.0 .. 24.0;
   type Miles_M16 is delta 3_000.0 * 2.0**(-15) range 0.0 .. 3_000.0;

   type Symmetric_Degrees_M7 is delta 2.0 range -180.0 .. 180.0;
   type Natural_Degrees_M15 is delta 2.0**(-6) range 0.0 .. 360.0;
   type Symmetric_Radians_M16 is delta Pi * 2.0**(-15) range -Pi .. Pi;
   -- 'SMALL = 2.0 ** (-14) = 0.00006_10351_5625.
   type Natural_Radians_M8 is delta Two_Pi * 2.0**(-7) range 0.0 .. Two_Pi;
   -- 'SMALL = 2.0 ** ( -5) = 0.03125.

   -------------------------------------------------------------------

   subtype St_Miles_M8 is
     Miles_M16 delta 3_000.0 * 2.0**(-15) range 0.0 .. 10.0;
   subtype St_Natural_Degrees_M11 is
     Natural_Degrees_M15 delta 0.25 range 0.0 .. 360.0;
   subtype St_Symmetric_Radians_M8 is
     Symmetric_Radians_M16 delta Half_Pi * 2.0**(-7) range -Half_Pi .. Half_Pi;
-- 'SMALL = 2.0 ** ( -7) = 0.00781_25.

begin

   Test
     ("C35A07D",
      "CHECK THAT FOR FIXED POINT TYPES THE FIRST " &
      "AND LAST ATTRIBUTES YIELD CORRECT VALUES - " &
      "TYPICAL TYPES");

   -------------------------------------------------------------------

   if Pixel_M10'First /= Ident_Int (1) * 0.0 then
      Failed ("PIXEL_M10'FIRST /= 0.0");
   end if;

   -------------------------------------------------------------------

   if Ruler_M8'First /= Ident_Int (1) * 0.0 then
      Failed ("RULER_M8'FIRST /= 0.0");
   end if;
   if Ruler_M8'Last /= Ident_Int (1) * 12.0 then
      Failed ("RULER_M8'LAST /= 12.0");
   end if;

   -------------------------------------------------------------------

   if Hours_M16'First /= Ident_Int (1) * 0.0 then
      Failed ("HOURS_M16'FIRST /= 0.0");
   end if;
   if Hours_M16'Last /= Ident_Int (1) * 24.0 then
      Failed ("HOURS_M16'LAST /= 24.0");
   end if;

   -------------------------------------------------------------------

   if Miles_M16'First /= Ident_Int (1) * 0.0 then
      Failed ("MILES_M16'FIRST /= 0.0");
   end if;
   if Miles_M16'Last /= Ident_Int (1) * 3_000.0 then
      Failed ("MILES_M16'LAST /= 3000.0");
   end if;

   -------------------------------------------------------------------

   if Symmetric_Degrees_M7'First /= Ident_Int (1) * (-180.0) then
      Failed ("SYMMETRIC_DEGREES_M7'FIRST /= -180.0");
   end if;
   if Symmetric_Degrees_M7'Last /= Ident_Int (1) * 180.0 then
      Failed ("SYMMETRIC_DEGREES_M7'LAST /= 180.0");
   end if;

   -------------------------------------------------------------------

   if Natural_Degrees_M15'First /= Ident_Int (1) * 0.0 then
      Failed ("NATURAL_DEGREES_M15'FIRST /= 0.0");
   end if;
   if Natural_Degrees_M15'Last /= Ident_Int (1) * 360.0 then
      Failed ("NATURAL_DEGREES_M15'LAST /= 360.0");
   end if;

   -------------------------------------------------------------------

   -- PI IS IN 3.0 + 2319 * 'SMALL .. 3.0 + 2320 * 'SMALL.
   if Symmetric_Radians_M16'First not in
       -3.14160_15625 .. -3.14154_05273_4375
   then
      Failed
        ("SYMMETRIC_RADIANS_M16'FIRST NOT IN " &
         "-3.14160_15625 .. -3.14154_05273_4375");
   end if;
   if Symmetric_Radians_M16'Last not in
       3.14154_05273_4375 .. 3.14160_15625
   then
      Failed
        ("SYMMETRIC_RADIANS_M16'LAST NOT IN " &
         "3.14154_05273_4375 .. 3.14160_15625");
   end if;

   -------------------------------------------------------------------

   if Natural_Radians_M8'First /= Ident_Int (1) * 0.0 then
      Failed ("NATURAL_RADIANS_M8'FIRST /= 0.0");
   end if;
   -- TWO_PI IS IN 201 * 'SMALL .. 202 * 'SMALL.
   if Natural_Radians_M8'Last not in 6.281_25 .. 6.312_5 then
      Failed ("NATURAL_RADIANS_M8'LAST NOT IN 6.28125 .. 6.3125");
   end if;

   -------------------------------------------------------------------

   if St_Miles_M8'First /= Ident_Int (1) * 0.0 then
      Failed ("ST_MILES_M8'FIRST /= 0.0");
   end if;
   if St_Miles_M8'Last /= Ident_Int (1) * 10.0 then
      Failed ("ST_MILES_M8'LAST /= 10.0");
   end if;

   -------------------------------------------------------------------

   if St_Natural_Degrees_M11'First /= Ident_Int (1) * 0.0 then
      Failed ("ST_NATURAL_DEGREES_M11'FIRST /= 0.0");
   end if;
   if St_Natural_Degrees_M11'Last /= Ident_Int (1) * 360.0 then
      Failed ("ST_NATURAL_DEGREES_M11'LAST /= 360.0");
   end if;

   -------------------------------------------------------------------

   -- HALF_PI IS IN 201 * 'SMALL .. 202 * 'SMALL.
   if St_Symmetric_Radians_M8'First not in -1.57812_5 .. -1.57031_25 then
      Failed
        ("ST_SYMMETRIC_RADIANS_M8'FIRST NOT IN " &
         "-1.57812_5 .. -1.57031_25");
   end if;
   if St_Symmetric_Radians_M8'Last not in 1.57031_25 .. 1.57812_5 then
      Failed
        ("ST_SYMMETRIC_RADIANS_M8'LAST NOT IN " & "1.57031_25 .. 1.57812_5");
   end if;

   -------------------------------------------------------------------

   Result;

end C35a07d;
