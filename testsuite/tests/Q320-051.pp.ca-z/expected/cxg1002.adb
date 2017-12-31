-- CXG1002.A
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
--      Check that the subprograms defined in the package
--      Ada.Numerics.Generic_Complex_Types provide the prescribed results.
--      Specifically, check the various versions of functions "+" and "-".
--
-- TEST DESCRIPTION:
--      This test checks that the subprograms "+" and "-" defined in the
--      Generic_Complex_Types package provide the results prescribed for the
--      evaluation of these complex arithmetic operations.  The functions
--      Re and Im are used to extract the appropriate component of the
--      complex result, in order that the prescribed result component can be
--      verified.
--      The generic package is instantiated with a real type (new Float),
--      and the results produced by the specified subprograms are verified.
--
-- SPECIAL REQUIREMENTS:
--      This test can be run in either "relaxed" or "strict" mode.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Numerics.Generic_Complex_Types;
with Report;

procedure Cxg1002 is

begin

   Report.Test
     ("CXG1002",
      "Check that the subprograms defined in " &
      "the package Ada.Numerics.Generic_Complex_Types " &
      "provide the prescribed results");

   Test_Block :
   declare

      type Real_Type is new Float;

      package Complex_Pack is new Ada.Numerics.Generic_Complex_Types
        (Real_Type);
      use Complex_Pack;

      -- Declare a zero valued complex number using the record aggregate
      -- approach.

      Complex_Zero : constant Complex_Pack.Complex := (0.0, 0.0);

      Tc_Complex, Tc_Complex_Right, Tc_Complex_Left : Complex_Pack.Complex :=
        Complex_Zero;

      Tc_Real : Real_Type := 0.0;

      Tc_Imaginary : Complex_Pack.Imaginary;

   begin

      -- Check that the imaginary component of the result of a binary addition
      -- operator that yields a result of complex type is exact when either of
      -- its operands is of pure-real type.

      Tc_Complex := Compose_From_Cartesian (2.0, 3.0);
      Tc_Real    := 3.0;

      if Im ("+" (Left => Tc_Complex, Right => Tc_Real)) /= 3.0 or
        Im ("+" (Tc_Complex, 6.0)) /= 3.0 or
        Im (Tc_Complex + Tc_Real) /= 3.0 or Im (Tc_Complex + 5.0) /= 3.0 or
        Im ((7.0, 2.0) + 1.0) /= 2.0 or Im ((7.0, 5.0) + (-2.0)) /= 5.0 or
        Im ((-7.0, -2.0) + 1.0) /= -2.0 or Im ((-7.0, -3.0) + (-3.0)) /= -3.0
      then
         Report.Failed
           ("Incorrect results from Function ""+"" with " &
            "one Complex and one Real argument - 1");
      end if;

      if Im ("+" (Left => Tc_Real, Right => Tc_Complex)) /= 3.0 or
        Im ("+" (4.0, Tc_Complex)) /= 3.0 or
        Im (Tc_Real + Tc_Complex) /= 3.0 or Im (9.0 + Tc_Complex) /= 3.0 or
        Im (1.0 + (7.0, -9.0)) /= -9.0 or Im ((-2.0) + (7.0, 2.0)) /= 2.0 or
        Im (1.0 + (-7.0, -5.0)) /= -5.0 or Im ((-3.0) + (-7.0, 16.0)) /= 16.0
      then
         Report.Failed
           ("Incorrect results from Function ""+"" with " &
            "one Complex and one Real argument - 2");
      end if;

      -- Check that the imaginary component of the result of a binary
      -- subtraction operator that yields a result of complex type is
      -- exact when its right operand is of pure-real type.

      Tc_Complex := (8.0, -4.0);
      Tc_Real    := 2.0;

      if Im ("-" (Left => Tc_Complex, Right => Tc_Real)) /= -4.0 or
        Im ("-" (Tc_Complex, 5.0)) /= -4.0 or
        Im (Tc_Complex - Tc_Real) /= -4.0 or Im (Tc_Complex - 4.0) /= -4.0 or
        Im ((6.0, 5.0) - 1.0) /= 5.0 or Im ((6.0, 13.0) - 7.0) /= 13.0 or
        Im ((-5.0, 3.0) - (2.0)) /= 3.0 or Im ((-5.0, -6.0) - (-3.0)) /= -6.0
      then
         Report.Failed
           ("Incorrect results from Function ""-"" with " &
            "one Complex and one Real argument");
      end if;

      -- Check that the real component of the result of a binary addition
      -- operator that yields a result of complex type is exact when either
      -- of its operands is of pure-imaginary type.

      Tc_Complex := (5.0, 0.0);

      if Re ("+" (Left => Tc_Complex, Right => I)) /= 5.0 or
        Re ("+" (Complex_Pack.J, Tc_Complex)) /= 5.0 or
        Re ((-8.0, 5.0) + (2.0 * I)) /= -8.0 or
        Re ((2.0, 5.0) + (-2.0 * I)) /= 2.0 or
        Re ((-20.0, -5.0) + (3.0 * I)) /= -20.0 or
        Re ((6.0, -5.0) + (-3.0 * I)) /= 6.0 then
         Report.Failed
           ("Incorrect results from Function ""+"" with " &
            "one Complex and one Imaginary argument");
      end if;

      -- Check that the real component of the result of a binary subtraction
      -- operator that yields a result of complex type is exact when its right
      -- operand is of pure-imaginary type.

      Tc_Complex := Tc_Complex + I;    -- Should produce (5.0, 1.0)

      if Re ("-" (Tc_Complex, I)) /= 5.0 or
        Re ((-4.0, 4.0) - (2.0 * I)) /= -4.0 or
        Re ((9.0, 4.0) - (5.0 * I)) /= 9.0 or
        Re ((16.0, -5.0) - (3.0 * I)) /= 16.0 or
        Re ((-3.0, -5.0) - (-4.0 * I)) /= -3.0 then
         Report.Failed
           ("Incorrect results from Function ""-"" with " &
            "one Complex and one Imaginary argument");
      end if;

      -- Check that the result of a binary addition operation is exact when
      -- one of its operands is of real type and the other is of pure-imaginary
      -- type; the operator is analogous to the Compose_From_Cartesian
      -- function; it performs no arithmetic.

      Tc_Complex := Complex_Pack."+" (5.0, Complex_Pack.I);

      if Tc_Complex /= (5.0, 1.0) or (4.0 + I) /= (4.0, 1.0) or
        "+" (Left => J, Right => 3.0) /= (3.0, 1.0) then
         Report.Failed
           ("Incorrect results from Function ""+"" with " &
            "one Real and one Imaginary argument");
      end if;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxg1002;
