-- C35A05Q.ADA

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

-- CASE Q: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC,
--         FOR GENERICS.

-- WRG 8/20/86

with Report; use Report;
with System; use System;
procedure C35a05q is

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

   type Fore_And_Aft is record
      Fore, Aft : Integer;
   end record;

   generic
      type T is delta <>;
   function Attributes return Fore_And_Aft;

   function Attributes return Fore_And_Aft is
   begin
      return (Ident_Int (T'Fore), Ident_Int (T'Aft));
   end Attributes;

   -------------------------------------------------------------------

   procedure Check_Attributes (Name         : String;
      Actual_Attributes, Correct_Attributes : Fore_And_Aft)
   is
   begin
      if Actual_Attributes.Fore /= Correct_Attributes.Fore then
         Failed
           ("GENERIC 'FORE FOR " & Name & " =" &
            Integer'Image (Actual_Attributes.Fore));
      end if;
      if Actual_Attributes.Aft /= Correct_Attributes.Aft then
         Failed
           ("GENERIC 'AFT  FOR " & Name & " =" &
            Integer'Image (Actual_Attributes.Aft));
      end if;
   end Check_Attributes;

   -------------------------------------------------------------------

   function Fa_Micro_Angle_Error_M15 is new Attributes (Micro_Angle_Error_M15);
   function Fa_Track_Range_M15 is new Attributes (Track_Range_M15);
   function Fa_Seconds_Mm is new Attributes (Seconds_Mm);
   function Fa_Range_Cell_Mm is new Attributes (Range_Cell_Mm);
   function Fa_Pixel_M10 is new Attributes (Pixel_M10);
   function Fa_Ruler_M8 is new Attributes (Ruler_M8);
   function Fa_Hours_M16 is new Attributes (Hours_M16);
   function Fa_Miles_M16 is new Attributes (Miles_M16);
   function Fa_Symmetric_Degrees_M7 is new Attributes (Symmetric_Degrees_M7);
   function Fa_Natural_Degrees_M15 is new Attributes (Natural_Degrees_M15);
   function Fa_Symmetric_Radians_M16 is new Attributes (Symmetric_Radians_M16);
   function Fa_Natural_Radians_M8 is new Attributes (Natural_Radians_M8);
   function Fa_St_Miles_M8 is new Attributes (St_Miles_M8);
   function Fa_St_Natural_Degrees_M11 is new Attributes
     (St_Natural_Degrees_M11);
   function Fa_St_Symmetric_Radians_M8 is new Attributes
     (St_Symmetric_Radians_M8);

begin

   Test
     ("C35A05Q",
      "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
      "TYPICAL TYPES, GENERICS");

   Check_Attributes
     ("MICRO_ANGLE_ERROR_M15", Fa_Micro_Angle_Error_M15, (7, 1));

   Check_Attributes ("TRACK_RANGE_M15", Fa_Track_Range_M15, (5, 1));

   Check_Attributes ("SECONDS_MM", Fa_Seconds_Mm, (4, 5));

   Check_Attributes ("RANGE_CELL_MM", Fa_Range_Cell_Mm, (7, 2));

   Check_Attributes ("PIXEL_M10", Fa_Pixel_M10, (2, 4));

   Check_Attributes ("RULER_M8", Fa_Ruler_M8, (3, 2));

   Check_Attributes ("HOURS_M16", Fa_Hours_M16, (3, 4));

   Check_Attributes ("MILES_M16", Fa_Miles_M16, (5, 2));

   Check_Attributes ("SYMMETRIC_DEGREES_M7", Fa_Symmetric_Degrees_M7, (4, 1));

   Check_Attributes ("NATURAL_DEGREES_M15", Fa_Natural_Degrees_M15, (4, 2));

   Check_Attributes
     ("SYMMETRIC_RADIANS_M16", Fa_Symmetric_Radians_M16, (2, 5));

   Check_Attributes ("NATURAL_RADIANS_M8", Fa_Natural_Radians_M8, (2, 2));

   Check_Attributes ("ST_MILES_M8", Fa_St_Miles_M8, (3, 2));

   Check_Attributes
     ("ST_NATURAL_DEGREES_M11", Fa_St_Natural_Degrees_M11, (4, 1));

   Check_Attributes
     ("ST_SYMMETRIC_RADIANS_M8", Fa_St_Symmetric_Radians_M8, (2, 2));

   Result;

end C35a05q;
