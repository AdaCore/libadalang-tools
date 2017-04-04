-- C35A05D.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE FORE AND AFT ATTRIBUTES YIELD THE
-- CORRECT VALUES.

-- CASE D: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC.

-- WRG 8/14/86

with Report; use Report;
procedure C35a05d is

   Pi      : constant := 3.14159_26535_89793_23846;
   Two_Pi  : constant := 2 * Pi;
   Half_Pi : constant := Pi / 2;

   Mm : constant := 23;

   -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S 'MANTISSA VALUE.

   type Micro_Angle_Error_M15 is delta 16.0 range -(2.0**19) .. 2.0**19;
   type Track_Range_M15 is delta 0.125 range -(2.0**12) .. 2.0**12;
   type Seconds_Mm is delta 2.0**(8 - Mm) range -(2.0**8) .. 2.0**8;
   type Range_Cell_Mm is
     delta 2.0**(-5) range -(2.0**(Mm - 5)) .. 2.0**(Mm - 5);

   type Pixel_M10 is delta 1.0 / 1_024.0 range 0.0 .. 1.0;
   type Ruler_M8 is delta 1.0 / 16.0 range 0.0 .. 12.0;

   type Hours_M16 is delta 24.0 * 2.0**(-15) range 0.0 .. 24.0;
   type Miles_M16 is delta 3_000.0 * 2.0**(-15) range 0.0 .. 3_000.0;

   type Symmetric_Degrees_M7 is delta 2.0 range -180.0 .. 180.0;
   type Natural_Degrees_M15 is delta 2.0**(-6) range 0.0 .. 360.0;
   type Symmetric_Radians_M16 is delta Pi * 2.0**(-15) range -Pi .. Pi;
   type Natural_Radians_M8 is delta Two_Pi * 2.0**(-7) range 0.0 .. Two_Pi;

   -------------------------------------------------------------------

   subtype St_Miles_M8 is
     Miles_M16 delta 3_000.0 * 2.0**(-15) range 0.0 .. 10.0;
   subtype St_Natural_Degrees_M11 is
     Natural_Degrees_M15 delta 0.25 range 0.0 .. 360.0;
   subtype St_Symmetric_Radians_M8 is
     Symmetric_Radians_M16 delta Half_Pi * 2.0**(-7) range -Half_Pi .. Half_Pi;

   -------------------------------------------------------------------

   procedure Check_Fore_And_Aft
     (Name         : String;
      Actual_Fore  : Integer;
      Correct_Fore : Positive;
      Actual_Aft   : Integer;
      Correct_Aft  : Positive)
   is
   begin
      if Actual_Fore /= Ident_Int (Correct_Fore) then
         Failed (Name & "'FORE =" & Integer'Image (Actual_Fore));
      end if;
      if Actual_Aft /= Ident_Int (Correct_Aft) then
         Failed (Name & "'AFT  =" & Integer'Image (Actual_Aft));
      end if;
   end Check_Fore_And_Aft;

begin

   Test
     ("C35A05D",
      "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
      "TYPICAL TYPES");

   Check_Fore_And_Aft
     ("MICRO_ANGLE_ERROR_M15",
      Micro_Angle_Error_M15'Fore,
      7,
      Micro_Angle_Error_M15'Aft,
      1);

   Check_Fore_And_Aft
     ("TRACK_RANGE_M15",
      Track_Range_M15'Fore,
      5,
      Track_Range_M15'Aft,
      1);

   Check_Fore_And_Aft ("SECONDS_MM", Seconds_Mm'Fore, 4, Seconds_Mm'Aft, 5);

   Check_Fore_And_Aft
     ("RANGE_CELL_MM",
      Range_Cell_Mm'Fore,
      7,
      Range_Cell_Mm'Aft,
      2);

   Check_Fore_And_Aft ("PIXEL_M10", Pixel_M10'Fore, 2, Pixel_M10'Aft, 4);

   Check_Fore_And_Aft ("RULER_M8", Ruler_M8'Fore, 3, Ruler_M8'Aft, 2);

   Check_Fore_And_Aft ("HOURS_M16", Hours_M16'Fore, 3, Hours_M16'Aft, 4);

   Check_Fore_And_Aft ("MILES_M16", Miles_M16'Fore, 5, Miles_M16'Aft, 2);

   Check_Fore_And_Aft
     ("SYMMETRIC_DEGREES_M7",
      Symmetric_Degrees_M7'Fore,
      4,
      Symmetric_Degrees_M7'Aft,
      1);

   Check_Fore_And_Aft
     ("NATURAL_DEGREES_M15",
      Natural_Degrees_M15'Fore,
      4,
      Natural_Degrees_M15'Aft,
      2);

   Check_Fore_And_Aft
     ("SYMMETRIC_RADIANS_M16",
      Symmetric_Radians_M16'Fore,
      2,
      Symmetric_Radians_M16'Aft,
      5);

   Check_Fore_And_Aft
     ("NATURAL_RADIANS_M8",
      Natural_Radians_M8'Fore,
      2,
      Natural_Radians_M8'Aft,
      2);

   Check_Fore_And_Aft ("ST_MILES_M8", St_Miles_M8'Fore, 3, St_Miles_M8'Aft, 2);

   Check_Fore_And_Aft
     ("ST_NATURAL_DEGREES_M11",
      St_Natural_Degrees_M11'Fore,
      4,
      St_Natural_Degrees_M11'Aft,
      1);

   Check_Fore_And_Aft
     ("ST_SYMMETRIC_RADIANS_M8",
      St_Symmetric_Radians_M8'Fore,
      2,
      St_Symmetric_Radians_M8'Aft,
      2);

   Result;

end C35a05d;
