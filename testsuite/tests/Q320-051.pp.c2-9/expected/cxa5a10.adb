-- CXA5A10.A
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
--      Check that the functions Exp and Sqrt, and the exponentiation
--      operator "**" provide correct results.
--
-- TEST DESCRIPTION:
--      This test examines both the versions of Exp, Sqrt, and "**"
--      resulting from the instantiation of the
--      Ada.Numerics.Generic_Elementary_Functions with a type derived from
--      type Float, as well as the preinstantiated version of this package
--      for type Float.
--      Prescribed results (stated as such in the reference manual),
--      including instances prescribed to raise exceptions, are examined
--      in the test cases.  In addition, certain evaluations are performed
--      for the preinstantiated package where the actual function result is
--      compared with the expected result (within an epsilon range of
--      accuracy).
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXA5A00.A   (foundation code)
--         CXA5A10.A
--
--
-- CHANGE HISTORY:
--      17 Apr 95   SAIC    Initial prerelease version.
--      13 Jun 95   SAIC    Incorporated use of Dont_Optimize procedure, and
--                          use of Result_Within_Range function overloaded for
--                          FXA5A00.New_Float_Type.
--      18 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      01 Oct 01   RLB     Protected Constraint_Error exception tests by
--                          first testing for 'Machine_Overflows.
--
--!

with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Elementary_Functions;
with Fxa5a00;
with Report;

procedure Cxa5a10 is
begin

   Report.Test
     ("CXA5A10",
      "Check that Exp, Sqrt, and the ""**"" operator " &
      "provide correct results");

   Test_Block : declare

      use Fxa5a00, Ada.Numerics;
      use Ada.Exceptions;

      package Gef is new Ada.Numerics.Generic_Elementary_Functions (New_Float);
      package Ef renames Ada.Numerics.Elementary_Functions;

      use Gef, Ef;

      Arg, Float_Result : Float;
      New_Float_Result  : New_Float;

      Flag_1,
      Flag_2,
      Flag_3,
      Flag_4,
      Incorrect_Inverse_Base_E,
      Incorrect_Inverse_Base_2,
      Incorrect_Inverse_Base_8,
      Incorrect_Inverse_Base_10,
      Incorrect_Inverse_Base_16 : Boolean :=
        False;

      procedure Dont_Optimize_Float is new Dont_Optimize (Float);
      procedure Dont_Optimize_New_Float is new Dont_Optimize (New_Float);

   begin

      -- Testing of the "**" operator, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the exponentiation operator
      -- when the value of the Left parameter (operand) is negative.

      begin
         New_Float_Result := Gef."**" (Left => -10.0, Right => 2.0);
         Report.Failed
           ("Argument_Error not raised by the instantiated " &
            "version of the exponentiation operator when the " &
            "value of the Left parameter is negative");
         Dont_Optimize_New_Float (New_Float_Result, 1);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by the " &
               "instantiated version of the exponentiation " &
               "operator when the value of the Left parameter " &
               "is negative");
      end;

      begin
         Float_Result := (-Fxa5a00.Small)**4.0;
         Report.Failed
           ("Argument_Error not raised by the preinstantiated " &
            "version of the exponentiation operator when the " &
            "value of the Left parameter is negative");
         Dont_Optimize_Float (Float_Result, 2);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by the " &
               "preinstantiated version of the exponentiation " &
               "operator when the value of the Left parameter " &
               "is negative");
      end;

      -- Check that Argument_Error is raised by the exponentiation operator
      -- when both parameters (operands) have the value 0.0.

      begin
         New_Float_Result := Gef."**" (0.0, Right => 0.0);
         Report.Failed
           ("Argument_Error not raised by the instantiated " &
            "version of the exponentiation operator when " &
            "both operands are zero");
         Dont_Optimize_New_Float (New_Float_Result, 3);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by the " &
               "instantiated version of the exponentiation " &
               "operator when both operands are zero");
      end;

      begin
         Float_Result := 0.0**0.0;
         Report.Failed
           ("Argument_Error not raised by the preinstantiated " &
            "version of the exponentiation operator when both " &
            "operands are zero");
         Dont_Optimize_Float (Float_Result, 4);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by the " &
               "preinstantiated version of the exponentiation " &
               "operator when both operands are zero");
      end;

      -- Check that Constraint_Error is raised by the exponentiation operator
      -- when the value of the left parameter (operand) is zero, and the value
      -- of the right parameter (exponent) is negative. This check applies only
      -- if Machine_Overflows is true [A.5.1(28, 30)].

      if New_Float'Machine_Overflows = True then
         begin
            New_Float_Result := Gef."**" (0.0, Right => -2.0);
            Report.Failed
              ("Constraint_Error not raised by the instantiated " &
               "version of the exponentiation operator when " &
               "the left parameter is 0.0, and the right " &
               "parameter is negative");
            Dont_Optimize_New_Float (New_Float_Result, 5);
         exception
            when Constraint_Error =>
               null;  -- OK, expected exception.
            when others =>
               Report.Failed
                 ("Unexpected exception raised by the " &
                  "instantiated version of the exponentiation " &
                  "operator when the left parameter is 0.0, " &
                  "and the right parameter is negative");
         end;
      end if;

      if Float'Machine_Overflows = True then
         begin
            Float_Result := 0.0**(-Fxa5a00.Small);
            Report.Failed
              ("Constraint_Error not raised by the " &
               "preinstantiated version of the exponentiation " &
               "operator when the left parameter is 0.0, and the " &
               "right parameter is negative");
            Dont_Optimize_Float (Float_Result, 6);
         exception
            when Constraint_Error =>
               null;  -- OK, expected exception.
            when others =>
               Report.Failed
                 ("Unexpected exception raised by the " &
                  "preinstantiated version of the exponentiation " &
                  "operator when the left parameter is 0.0, and " &
                  "the right parameter is negative");
         end;
      end if;

      -- Prescribed results.
      -- Check that exponentiation by a 0.0 exponent yields the value one.

      if Gef."**" (Left => 10.0, Right => 0.0) /= 1.0 or
        Ef."**" (Fxa5a00.Large, Right => 0.0) /= 1.0 or
        Gef."**" (3.0, 0.0) /= 1.0 or
        Fxa5a00.Small**0.0 /= 1.0
      then
         Report.Failed
           ("Incorrect results returned from the ""**"" " &
            "operator when the value of the exponent is 0.0");
      end if;

      -- Check that exponentiation by a unit exponent yields the value of the
      -- left operand.

      if Gef."**" (Left => 50.0, Right => 1.0) /= 50.0 or
        Ef."**" (Fxa5a00.Large, Right => 1.0) /= Fxa5a00.Large or
        Gef."**" (6.0, 1.0) /= 6.0 or
        Fxa5a00.Small**1.0 /= Fxa5a00.Small
      then
         Report.Failed
           ("Incorrect results returned from the ""**"" " &
            "operator when the value of the exponent is 1.0");
      end if;

      -- Check that exponentiation of the value 1.0 yields the value 1.0.

      if Gef."**" (Left => 1.0, Right => 16.0) /= 1.0 or
        Ef."**" (1.0, Right => Fxa5a00.Large) /= 1.0 or
        Gef."**" (1.0, 3.0) /= 1.0 or
        1.0**Fxa5a00.Small /= 1.0
      then
         Report.Failed
           ("Incorrect results returned from the ""**"" " &
            "operator when the value of the operand is 1.0");
      end if;

      -- Check that exponentiation of the value 0.0 yields the value 0.0.

      if Gef."**" (Left => 0.0, Right => 10.0) /= 0.0 or
        Ef."**" (0.0, Right => Fxa5a00.Large) /= 0.0 or
        Gef."**" (0.0, 4.0) /= 0.0 or
        0.0**Fxa5a00.Small /= 0.0
      then
         Report.Failed
           ("Incorrect results returned from the ""**"" " &
            "operator when the value of the operand is 0.0");
      end if;

      -- Check that exponentiation of various operands with a variety of of
      -- exponent values yield correct results.

      if not Result_Within_Range (Gef."**" (5.0, 2.0), 25.0, 0.01) or
        not Result_Within_Range (Gef."**" (1.225, 1.5), 1.36, 0.01) or
        not Result_Within_Range (Gef."**" (0.26, 2.0), 0.068, 0.001) or
        not Result_Within_Range (Ef."**" (E, 5.0), 148.4, 0.1) or
        not Result_Within_Range (Ef."**" (10.0, E), 522.7, 0.1) or
        not Result_Within_Range (Ef."**" (E, (-3.0)), 0.050, 0.001) or
        not Result_Within_Range (Gef."**" (10.0, (-2.0)), 0.010, 0.001)
      then
         Report.Failed
           ("Incorrect results returned from the ""**"" " &
            "operator with a variety of  operand and exponent " &
            "values");
      end if;

      -- Use the following loops to check for internal consistency between
      -- inverse functions.

      declare
         -- Use the relative error value to account for non-exact computations.
         Tc_Relative_Error : Float := 0.005;
      begin
         for I in 1 .. 5 loop
            for J in 0 .. 5 loop
               if not Incorrect_Inverse_Base_E and
                 not Fxa5a00.Result_Within_Range
                   (Float (I)**Float (J),
                    E**(Float (J) * Ef.Log (Float (I))),
                    Tc_Relative_Error)
               then
                  Incorrect_Inverse_Base_E := True;
                  Report.Failed
                    ("Incorrect Log-** Inverse calc for Base e " &
                     "with i= " &
                     Integer'Image (I) &
                     "  and j= " &
                     Integer'Image (J));
               end if;
               if not Incorrect_Inverse_Base_2 and
                 not Fxa5a00.Result_Within_Range
                   (Float (I)**Float (J),
                    2.0**(Float (J) * Ef.Log (Float (I), 2.0)),
                    Tc_Relative_Error)
               then
                  Incorrect_Inverse_Base_2 := True;
                  Report.Failed
                    ("Incorrect Log-** Inverse calc for Base 2 " &
                     "with i= " &
                     Integer'Image (I) &
                     "  and j= " &
                     Integer'Image (J));
               end if;
               if not Incorrect_Inverse_Base_8 and
                 not Fxa5a00.Result_Within_Range
                   (Float (I)**Float (J),
                    8.0**(Float (J) * Ef.Log (Float (I), 8.0)),
                    Tc_Relative_Error)
               then
                  Incorrect_Inverse_Base_8 := True;
                  Report.Failed
                    ("Incorrect Log-** Inverse calc for Base 8 " &
                     "with i= " &
                     Integer'Image (I) &
                     "  and j= " &
                     Integer'Image (J));
               end if;
               if not Incorrect_Inverse_Base_10 and
                 not Fxa5a00.Result_Within_Range
                   (Float (I)**Float (J),
                    10.0**(Float (J) * Ef.Log (Float (I), 10.0)),
                    Tc_Relative_Error)
               then
                  Incorrect_Inverse_Base_10 := True;
                  Report.Failed
                    ("Incorrect Log-** Inverse calc for Base 10 " &
                     "with i= " &
                     Integer'Image (I) &
                     "   and j= " &
                     Integer'Image (J));
               end if;
               if not Incorrect_Inverse_Base_16 and
                 not Fxa5a00.Result_Within_Range
                   (Float (I)**Float (J),
                    16.0**(Float (J) * Ef.Log (Float (I), 16.0)),
                    Tc_Relative_Error)
               then
                  Incorrect_Inverse_Base_16 := True;
                  Report.Failed
                    ("Incorrect Log-** Inverse calc for Base 16 " &
                     "with i= " &
                     Integer'Image (I) &
                     "   and j= " &
                     Integer'Image (J));
               end if;
            end loop;
         end loop;
      end;

      -- Reset Flags.
      Incorrect_Inverse_Base_E  := False;
      Incorrect_Inverse_Base_2  := False;
      Incorrect_Inverse_Base_8  := False;
      Incorrect_Inverse_Base_10 := False;
      Incorrect_Inverse_Base_16 := False;

      -- Testing of Exp Function, both instantiated and pre-instantiated
      -- version.

      -- Check that the result of the Exp Function, when provided an X
      -- parameter value of 0.0, is 1.0.

      if Gef.Exp (X => 0.0) /= 1.0 or Ef.Exp (0.0) /= 1.0 then
         Report.Failed
           ("Incorrect result returned by Function Exp when " &
            "given a parameter value of 0.0");
      end if;

      -- Check that the Exp Function provides correct results when provided a
      -- variety of input parameter values.

      if not Result_Within_Range (Gef.Exp (0.001), 1.01, 0.01) or
        not Result_Within_Range (Ef.Exp (0.1), 1.11, 0.01) or
        not Result_Within_Range (Gef.Exp (1.269_7), 3.56, 0.01) or
        not Result_Within_Range (Ef.Exp (3.252_5), 25.9, 0.1) or
        not Result_Within_Range (Gef.Exp (-0.219_8), 0.803, 0.001) or
        not Result_Within_Range (Ef.Exp (-1.662_1), 0.190, 0.001) or
        not Result_Within_Range (Gef.Exp (-2.388_8), 0.092, 0.001) or
        not Result_Within_Range (Ef.Exp (-5.441_5), 0.004, 0.001)
      then
         Report.Failed
           ("Incorrect result from Function Exp when provided " &
            "a variety of input parameter values");
      end if;

      -- Use the following loops to check for internal consistency between
      -- inverse functions.

      Arg := 0.01;
      while Arg < 10.0 loop
         if not Incorrect_Inverse_Base_E and
           Fxa5a00.Result_Within_Range
             (Ef.Exp (Arg),
              E**(Arg * Ef.Log (Arg)),
              0.001)
         then
            Incorrect_Inverse_Base_E := True;
            Report.Failed ("Incorrect Exp-** Inverse calc for Base e");
         end if;
         if not Incorrect_Inverse_Base_2 and
           Fxa5a00.Result_Within_Range
             (Ef.Exp (Arg),
              2.0**(Arg * Ef.Log (Arg, 2.0)),
              0.001)
         then
            Incorrect_Inverse_Base_2 := True;
            Report.Failed ("Incorrect Exp-** Inverse calc for Base 2");
         end if;
         if not Incorrect_Inverse_Base_8 and
           Fxa5a00.Result_Within_Range
             (Ef.Exp (Arg),
              8.0**(Arg * Ef.Log (Arg, 8.0)),
              0.001)
         then
            Incorrect_Inverse_Base_8 := True;
            Report.Failed ("Incorrect Exp-** Inverse calc for Base 8");
         end if;
         if not Incorrect_Inverse_Base_10 and
           Fxa5a00.Result_Within_Range
             (Ef.Exp (Arg),
              10.0**(Arg * Ef.Log (Arg, 10.0)),
              0.001)
         then
            Incorrect_Inverse_Base_10 := True;
            Report.Failed ("Incorrect Exp-** Inverse calc for Base 10");
         end if;
         if not Incorrect_Inverse_Base_16 and
           Fxa5a00.Result_Within_Range
             (Ef.Exp (Arg),
              16.0**(Arg * Ef.Log (Arg, 16.0)),
              0.001)
         then
            Incorrect_Inverse_Base_16 := True;
            Report.Failed ("Incorrect Exp-** Inverse calc for Base 16");
         end if;
         Arg := Arg + 0.01;
      end loop;

      -- Testing of Sqrt Function, both instantiated and pre-instantiated
      -- version.

      -- Check that Argument_Error is raised by the Sqrt Function when the
      -- value of the input parameter X is negative.

      begin
         Float_Result := Ef.Sqrt (X => -Fxa5a00.Small);
         Report.Failed
           ("Argument_Error not raised by Function Sqrt " &
            "when provided a small negative input parameter " &
            "value");
         Dont_Optimize_Float (Float_Result, 7);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by Function Sqrt " &
               "when provided a small negative input parameter " &
               "value");
      end;

      begin
         New_Float_Result := Gef.Sqrt (X => -64.0);
         Report.Failed
           ("Argument_Error not raised by Function Sqrt " &
            "when provided a large negative input parameter " &
            "value");
         Dont_Optimize_New_Float (New_Float_Result, 8);
      exception
         when Argument_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by Function Sqrt " &
               "when provided a large negative input parameter " &
               "value");
      end;

      -- Check that the Sqrt Function, when given an X parameter value of 0.0,
      -- returns a result of 0.0.

      if Gef.Sqrt (X => 0.0) /= 0.0 or Ef.Sqrt (0.0) /= 0.0 then
         Report.Failed
           ("Incorrect result from Function Sqrt when provided " &
            "an input parameter value of 0.0");
      end if;

      -- Check that the Sqrt Function, when given an X parameter input value of
      -- 1.0, returns a result of 1.0.

      if Gef.Sqrt (X => 1.0) /= 1.0 or Ef.Sqrt (1.0) /= 1.0 then
         Report.Failed
           ("Incorrect result from Function Sqrt when provided " &
            "an input parameter value of 1.0");
      end if;

      -- Check that the Sqrt Function provides correct results when provided a
      -- variety of input parameter values.

      if not Fxa5a00.Result_Within_Range (Gef.Sqrt (0.032_7), 0.181, 0.001) or
        not Fxa5a00.Result_Within_Range (Ef.Sqrt (0.180_8), 0.425, 0.001) or
        not Fxa5a00.Result_Within_Range (Gef.Sqrt (1.055_6), 1.03, 0.01) or
        not Fxa5a00.Result_Within_Range (Ef.Sqrt (32.820_8), 5.73, 0.01) or
        not Fxa5a00.Result_Within_Range (Ef.Sqrt (27_851.0), 166.9, 0.1) or
        not Fxa5a00.Result_Within_Range (Ef.Sqrt (61_203.4), 247.4, 0.1) or
        not Fxa5a00.Result_Within_Range (Ef.Sqrt (655_891.0), 809.9, 0.1)
      then
         Report.Failed
           ("Incorrect result from Function Sqrt when provided " &
            "a variety of input parameter values");
      end if;

      -- Check internal consistency between functions.

      Arg := 0.01;
      while Arg < 10.0 loop
         if not Flag_1 and
           not Fxa5a00.Result_Within_Range
             (Arg,
              Ef.Sqrt (Arg) * Ef.Sqrt (Arg),
              0.01)
         then
            Report.Failed ("Inconsistency found in Case 1");
            Flag_1 := True;
         end if;
         if not Flag_2 and
           not Fxa5a00.Result_Within_Range (Arg, Ef.Sqrt (Arg)**2.0, 0.01)
         then
            Report.Failed ("Inconsistency found in Case 2");
            Flag_2 := True;
         end if;
         if not Flag_3 and
           not Fxa5a00.Result_Within_Range
             (Ef.Log (Arg),
              Ef.Log (Sqrt (Arg)**2.0),
              0.01)
         then
            Report.Failed ("Inconsistency found in Case 3");
            Flag_3 := True;
         end if;
         if not Flag_4 and
           not Fxa5a00.Result_Within_Range
             (Ef.Log (Arg),
              2.00 * Ef.Log (Ef.Sqrt (Arg)),
              0.01)
         then
            Report.Failed ("Inconsistency found in Case 4");
            Flag_4 := True;
         end if;
         Arg := Arg + 1.0;
      end loop;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " &
            "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxa5a10;
