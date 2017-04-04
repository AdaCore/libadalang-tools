-- CXA5015.A
--
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
--
-- OBJECTIVE:
--      Check that the following representation-oriented attributes are
--      available and that the produce correct results:
--      'Denorm, 'Signed_Zeros, 'Exponent 'Fraction, 'Compose, 'Scaling,
--      'Floor, 'Ceiling, 'Rounding, 'Unbiased_Rounding, 'Truncation,
--      'Remainder, 'Adjacent, 'Copy_Sign, 'Leading_Part, 'Machine, and
--      'Model_Small.
--
-- TEST DESCRIPTION:
--      This test checks whether certain attributes of floating point types
--      are available from an implementation.  Where attribute correctness
--      can be verified in a straight forward manner, the appropriate checks
--      are included here.  However, this test is not intended to ensure the
--      correctness of the results returned from all of the attributes
--      examined in this test; that process will occur in the tests of the
--      Numerics_Annex.
--
--
-- CHANGE HISTORY:
--      26 Jun 95   SAIC    Initial prerelease version.
--      29 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      01 DEC 97   EDS     Fix value for checking the S'Adjacent attribute
--!

with Report;

procedure Cxa5015 is

   subtype Float_Subtype is Float range -10.0 .. 10.0;
   type Derived_Float_1 is digits 8;
   type Derived_Float_2 is new Derived_Float_1 range -10.0 .. 10.0E10;

   use type Float, Float_Subtype, Derived_Float_1, Derived_Float_2;

   Tc_Boolean   : Boolean;
   Tc_Float     : Float;
   Tc_Sfloat    : Float_Subtype;
   Tc_Dfloat_1  : Derived_Float_1;
   Tc_Dfloat_2  : Derived_Float_2;
   Tc_Tolerance : Float := 0.001;

   function Not_Equal
     (Actual_Result, Expected_Result, Tolerance : Float) return Boolean
   is
   begin
      return abs (Actual_Result - Expected_Result) > Tolerance;
   end Not_Equal;

begin

   Report.Test
     ("CXA5015",
      "Check that certain representation-oriented " &
      "attributes are available and that they " &
      "produce correct results");

   -- New Representation-Oriented Attributes.
   --
   -- Check the S'Denorm attribute.

   Tc_Boolean := Float'Denorm;
   Tc_Boolean := Float_Subtype'Denorm;
   Tc_Boolean := Derived_Float_1'Denorm;
   Tc_Boolean := Derived_Float_2'Denorm;

   -- Check the S'Signed_Zeroes attribute.

   Tc_Boolean := Float'Signed_Zeros;
   Tc_Boolean := Float_Subtype'Signed_Zeros;
   Tc_Boolean := Derived_Float_1'Signed_Zeros;
   Tc_Boolean := Derived_Float_2'Signed_Zeros;

   -- New Primitive Function Attributes.
   --
   -- Check the S'Exponent attribute.

   Tc_Float    := 0.5;
   Tc_Sfloat   := 0.99;
   Tc_Dfloat_1 := 2.45;
   Tc_Dfloat_2 := 2.65;

   if Float'Exponent (Tc_Float) > Float_Subtype'Exponent (Tc_Sfloat) or
     Float'Exponent (Tc_Float) > 2
   then
      Report.Failed ("Incorrect result from the 'Exponent attribute");
   end if;

   -- Check the S'Fraction attribute.

   if Not_Equal
       (Float'Fraction (Tc_Float),
        Tc_Float * Float (Float'Machine_Radix)**(-Float'Exponent (Tc_Float)),
        Tc_Tolerance)
   then
      Report.Failed ("Incorrect result from the 'Fraction attribute - 1");
   end if;

   if Float'Fraction (Tc_Float) <
     (1.0 / Float (Float'Machine_Radix)) - Tc_Tolerance or
     Float'Fraction (Tc_Float) >= 1.0 - Tc_Tolerance
   then
      Report.Failed ("Incorrect result from the 'Fraction attribute - 2");
   end if;

   -- Check the S'Compose attribute.

   if Not_Equal
       (Float'Compose (Tc_Float, 3),
        Tc_Float *
        Float (Float'Machine_Radix)**(3 - Float'Exponent (Tc_Float)),
        Tc_Tolerance)
   then
      Report.Failed ("Incorrect result from the 'Compose attribute");
   end if;

   -- Check the S'Scaling attribute.

   if Not_Equal
       (Float'Scaling (Tc_Float, 2),
        Tc_Float * Float (Float'Machine_Radix)**2,
        Tc_Tolerance)
   then
      Report.Failed ("Incorrect result from the 'Scaling attribute");
   end if;

   -- Check the S'Floor attribute.

   Tc_Float    := 0.99;
   Tc_Sfloat   := 1.00;
   Tc_Dfloat_1 := 2.50;
   Tc_Dfloat_2 := -2.50;

   if Float'Floor (Tc_Float) /= 0.0 or
     Float_Subtype'Floor (Tc_Sfloat) /= 1.0 or
     Derived_Float_1'Floor (Tc_Dfloat_1) /= 2.0 or
     Derived_Float_2'Floor (Tc_Dfloat_2) /= -3.0
   then
      Report.Failed ("Incorrect result from the 'Floor attribute");
   end if;

   -- Check the S'Ceiling attribute.

   Tc_Float    := 0.99;
   Tc_Sfloat   := 1.00;
   Tc_Dfloat_1 := 2.50;
   Tc_Dfloat_2 := -2.99;

   if Float'Ceiling (Tc_Float) /= 1.0 or
     Float_Subtype'Ceiling (Tc_Sfloat) /= 1.0 or
     Derived_Float_1'Ceiling (Tc_Dfloat_1) /= 3.0 or
     Derived_Float_2'Ceiling (Tc_Dfloat_2) /= -2.0
   then
      Report.Failed ("Incorrect result from the 'Ceiling attribute");
   end if;

   -- Check the S'Rounding attribute.

   Tc_Float    := 0.49;
   Tc_Sfloat   := 1.00;
   Tc_Dfloat_1 := 2.50;
   Tc_Dfloat_2 := -2.50;

   if Float'Rounding (Tc_Float) /= 0.0 or
     Float_Subtype'Rounding (Tc_Sfloat) /= 1.0 or
     Derived_Float_1'Rounding (Tc_Dfloat_1) /= 3.0 or
     Derived_Float_2'Rounding (Tc_Dfloat_2) /= -3.0
   then
      Report.Failed ("Incorrect result from the 'Rounding attribute");
   end if;

   -- Check the S'Unbiased_Rounding attribute.

   Tc_Float    := 0.50;
   Tc_Sfloat   := 1.50;
   Tc_Dfloat_1 := 2.50;
   Tc_Dfloat_2 := -2.50;

   if Float'Unbiased_Rounding (Tc_Float) /= 0.0 or
     Float_Subtype'Unbiased_Rounding (Tc_Sfloat) /= 2.0 or
     Derived_Float_1'Unbiased_Rounding (Tc_Dfloat_1) /= 2.0 or
     Derived_Float_2'Unbiased_Rounding (Tc_Dfloat_2) /= -2.0
   then
      Report.Failed
        ("Incorrect result from the 'Unbiased_Rounding " & "attribute");
   end if;

   -- Check the S'Truncation attribute.

   Tc_Float    := -0.99;
   Tc_Sfloat   := 1.50;
   Tc_Dfloat_1 := 2.99;
   Tc_Dfloat_2 := -2.50;

   if Float'Truncation (Tc_Float) /= 0.0 or
     Float_Subtype'Truncation (Tc_Sfloat) /= 1.0 or
     Derived_Float_1'Truncation (Tc_Dfloat_1) /= 2.0 or
     Derived_Float_2'Truncation (Tc_Dfloat_2) /= -2.0
   then
      Report.Failed ("Incorrect result from the 'Truncation attribute");
   end if;

   -- Check the S'Remainder attribute.

   Tc_Float    := 9.0;
   Tc_Sfloat   := 7.5;
   Tc_Dfloat_1 := 5.0;
   Tc_Dfloat_2 := 8.0;

   if Float'Remainder (Tc_Float, 2.0) /= 1.0 or
     Float_Subtype'Remainder (Tc_Sfloat, 3.0) /= 1.5 or
     Derived_Float_1'Remainder (Tc_Dfloat_1, 2.0) /= 1.0 or
     Derived_Float_2'Remainder (Tc_Dfloat_2, 4.0) /= 0.0
   then
      Report.Failed ("Incorrect result from the 'Remainder attribute");
   end if;

   -- Check the S'Adjacent attribute.

   Tc_Float  := 4.0;
   Tc_Sfloat := -1.0;

   if Float'Adjacent (Tc_Float, Tc_Float) /= Tc_Float or
     Float_Subtype'Adjacent (Tc_Sfloat, -1.0) /= Tc_Sfloat
   then
      Report.Failed ("Incorrect result from the 'Adjacent attribute");
   end if;

   -- Check the S'Copy_Sign attribute.

   Tc_Float    := 0.0;
   Tc_Sfloat   := -1.0;
   Tc_Dfloat_1 := 5.0;
   Tc_Dfloat_2 := -2.5;

   if Float'Copy_Sign (Tc_Float, -2.0) /= 0.0 or
     Float_Subtype'Copy_Sign (Tc_Sfloat, 4.0) /= 1.0 or
     Derived_Float_1'Copy_Sign (Tc_Dfloat_1, -2.0) /= -5.0 or
     Derived_Float_2'Copy_Sign (Tc_Dfloat_2, -2.0) /= -2.5
   then
      Report.Failed ("Incorrect result from the 'Copy_Sign attribute");
   end if;

   -- Check the S'Leading_Part attribute.

   Tc_Float    := 0.0;
   Tc_Sfloat   := -1.0;
   Tc_Dfloat_1 := 5.88;
   Tc_Dfloat_2 := -2.52;

   -- Leading part obtained in the variables.
   Tc_Float    := Float'Leading_Part (Tc_Float, 2);
   Tc_Sfloat   := Float_Subtype'Leading_Part (Tc_Sfloat, 2);
   Tc_Dfloat_1 := Derived_Float_1'Leading_Part (Tc_Dfloat_1, 2);
   Tc_Dfloat_2 := Derived_Float_2'Leading_Part (Tc_Dfloat_2, 2);

   -- Checking for the leading part of the variables at this point should
   -- produce the same values.
   if Float'Leading_Part (Tc_Float, 2) /= Tc_Float or
     Float_Subtype'Leading_Part (Tc_Sfloat, 2) /= Tc_Sfloat or
     Derived_Float_1'Leading_Part (Tc_Dfloat_1, 2) /= Tc_Dfloat_1 or
     Derived_Float_2'Leading_Part (Tc_Dfloat_2, 2) /= Tc_Dfloat_2
   then
      Report.Failed ("Incorrect result from the 'Leading_Part attribute");
   end if;

   -- Check the S'Machine attribute.

   Tc_Float    := 0.0;
   Tc_Sfloat   := -1.0;
   Tc_Dfloat_1 := 5.88;
   Tc_Dfloat_2 := -2.52;

   -- Closest machine number obtained in the variables.
   Tc_Float    := Float'Machine (Tc_Float);
   Tc_Sfloat   := Float_Subtype'Machine (Tc_Sfloat);
   Tc_Dfloat_1 := Derived_Float_1'Machine (Tc_Dfloat_1);
   Tc_Dfloat_2 := Derived_Float_2'Machine (Tc_Dfloat_2);

   -- Checking for the closest machine number to each of the variables at this
   -- point should produce the same values.
   if Float'Machine (Tc_Float) /= Tc_Float or
     Float_Subtype'Machine (Tc_Sfloat) /= Tc_Sfloat or
     Derived_Float_1'Machine (Tc_Dfloat_1) /= Tc_Dfloat_1 or
     Derived_Float_2'Machine (Tc_Dfloat_2) /= Tc_Dfloat_2
   then
      Report.Failed ("Incorrect result from the 'Machine attribute");
   end if;

   -- New Model-Oriented Attributes.
   --
   -- Check the S'Model_Small attribute.

   if Not_Equal
       (Float'Model_Small,
        Float (Float'Machine_Radix)**(Float'Model_Emin - 1),
        Tc_Tolerance)
   then
      Report.Failed ("Incorrect result from the 'Model_Small attribute");
   end if;

   Report.Result;

end Cxa5015;
