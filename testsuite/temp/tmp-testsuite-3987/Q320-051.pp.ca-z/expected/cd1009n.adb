-- CD1009N.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE MAY BE GIVEN
--     IN THE VISIBLE OR PRIVATE PART OF A PACKAGE FOR A RECORD TYPE
--     DECLARED IN THE VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     VCL 10/08/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP', CORRECTED
--                    CHECKS FOR FAILURE.

with System;
with Report; use Report;
procedure Cd1009n is
begin
   Test
     ("CD1009N",
      "A RECORD REPRESENTATION CLAUSE MAY BE GIVEN " &
      "IN THE VISIBLE OR PRIVATE PART OF A PACKAGE " &
      "FOR A RECORD TYPE DECLARED IN THE " &
      "VISIBLE PART OF THE SAME PACKAGE");
   declare
      package Pack is
         Units_Per_Integer : constant :=
           (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

         type Check_Type_1 is record
            I1 : Integer range 0 .. 255;
            B1 : Boolean;
            B2 : Boolean;
            I2 : Integer range 0 .. 15;
         end record;
         for Check_Type_1 use record
            I1 at 0 * Units_Per_Integer range 0 .. Integer'Size - 1;
            B1 at 1 * Units_Per_Integer range 0 .. Boolean'Size - 1;
            B2 at 2 * Units_Per_Integer range 0 .. Boolean'Size - 1;
            I2 at 3 * Units_Per_Integer range 0 .. Integer'Size - 1;
         end record;

         type Check_Type_2 is record
            I1 : Integer range 0 .. 255;
            B1 : Boolean;
            B2 : Boolean;
            I2 : Integer range 0 .. 15;
         end record;

      private
         for Check_Type_2 use record
            I1 at 0 * Units_Per_Integer range 0 .. Integer'Size - 1;
            B1 at 1 * Units_Per_Integer range 0 .. Boolean'Size - 1;
            B2 at 2 * Units_Per_Integer range 0 .. Boolean'Size - 1;
            I2 at 3 * Units_Per_Integer range 0 .. Integer'Size - 1;
         end record;
      end Pack;

      use Pack;

      R1 : Check_Type_1;

      R2 : Check_Type_2;
   begin
      if R1.I1'First_Bit /= 0 or
        R1.I1'Last_Bit /= Integer'Size - 1 or
        R1.I1'Position /= 0
      then
         Failed ("INCORRECT REPRESENTATION FOR R1.I1");
      end if;

      if R1.B1'First_Bit /= 0 or
        R1.B1'Last_Bit /= Boolean'Size - 1 or
        R1.B1'Position /= 1 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R1.B1");
      end if;

      if R1.B2'First_Bit /= 0 or
        R1.B2'Last_Bit /= Boolean'Size - 1 or
        R1.B2'Position /= 2 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R1.B2");
      end if;

      if R1.I2'First_Bit /= 0 or
        R1.I2'Last_Bit /= Integer'Size - 1 or
        R1.I2'Position /= 3 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R1.I2");
      end if;

      if R2.I1'First_Bit /= 0 or
        R2.I1'Last_Bit /= Integer'Size - 1 or
        R2.I1'Position /= 0
      then
         Failed ("INCORRECT REPRESENTATION FOR R2.I1");
      end if;

      if R2.B1'First_Bit /= 0 or
        R2.B1'Last_Bit /= Boolean'Size - 1 or
        R2.B1'Position /= 1 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R2.B1");
      end if;

      if R2.B2'First_Bit /= 0 or
        R2.B2'Last_Bit /= Boolean'Size - 1 or
        R2.B2'Position /= 2 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R2.B2");
      end if;

      if R2.I2'First_Bit /= 0 or
        R2.I2'Last_Bit /= Integer'Size - 1 or
        R2.I2'Position /= 3 * Units_Per_Integer
      then
         Failed ("INCORRECT REPRESENTATION FOR R2.I2");
      end if;
   end;

   Result;
end Cd1009n;
