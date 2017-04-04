-- CXF3A04.A
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
--      Check that the edited output string value returned by Function Image
--      is correct.
--
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--
--      The results of the Image function are examined under a number of
--      circumstances.  The generic package Decimal_Output is instantiated
--      twice, for decimal data with delta 0.01 and delta 1.0.  Each version
--      of Image is called with both default parameters and user-provided
--      parameters.  The results of each call to Image are compared to an
--      expected edited output result string.
--
--      In addition, three calls to Image are designed to raise Layout_Error,
--      due to the combination of decimal value and picture string provided
--      as input parameters.  If Layout_Error is not raised, or an alternate
--      exception is raised instead, test failure results.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A04.A
--
--
-- CHANGE HISTORY:
--      22 JAN 95   SAIC    Initial prerelease version.
--      11 MAR 97   PWB.CTA Corrected incorrect index expression
--!

with Fxf3a00;
with Ada.Text_Io.Editing;
with Report;

procedure Cxf3a04 is
begin

   Report.Test
     ("CXF3A04",
      "Check that the string value returned by " &
      "Function Image is correct, based on the " &
      "numerical data and picture formatting " &
      "parameters provided to the function");

   Test_Block : declare

      use Ada.Text_Io;

      -- Instantiate the Decimal_Output generic package for the two data
      -- types, using the default values for the Default_Currency,
      -- Default_Fill, Default_Separator, and Default_Radix_Mark
      -- parameters.

      package Pack_Ndp is new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_Ndp);

      package Pack_2dp is new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp);

      Tc_Currency   : constant String    := "$";
      Tc_Fill       : constant Character := '*';
      Tc_Separator  : constant Character := ',';
      Tc_Radix_Mark : constant Character := '.';

      Tc_Picture : Editing.Picture;

   begin

      Two_Decimal_Place_Data :
         -- Use a decimal fixed point type with delta 0.01 (two decimal places)
         -- and valid picture strings. Evaluate the result of function Image
         -- with the expected edited output result string.
          declare

         Tc_Loop_End : constant :=                                     -- 10
           Fxf3a00.Number_Of_2dp_Items - Fxf3a00.Number_Of_Foreign_Strings;

      begin
         -- The first 10 picture strings in the Valid_Strings array
         -- correspond to data values of a decimal type with delta 0.01.

         -- Compare string result of Image with expected edited output
         -- string.  Evaluate data using both default parameters of Image
         -- and user-provided parameter values.
         for I in 1 .. Tc_Loop_End loop

            -- Create the picture object from the picture string.
            Tc_Picture := Editing.To_Picture (Fxf3a00.Valid_Strings (I).all);

            -- Use the default parameters for this loop evaluation of Image.
            if Pack_2dp.Image (Fxf3a00.Data_With_2dp (I), Tc_Picture) /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect result from Function Image, " &
                  "when used with a decimal type with delta " &
                  "0.01, picture string " &
                  Fxf3a00.Valid_Strings (I).all &
                  ", and the default parameters of Image");
            end if;

            -- Use user-provided parameters for this loop evaluation of Image.

            if Pack_2dp.Image
                (Item       => Fxf3a00.Data_With_2dp (I),
                 Pic        => Tc_Picture,
                 Currency   => Tc_Currency,
                 Fill       => Tc_Fill,
                 Separator  => Tc_Separator,
                 Radix_Mark => Tc_Radix_Mark) /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect result from Function Image, " &
                  "when used with a decimal type with delta " &
                  "0.01, picture string " &
                  Fxf3a00.Valid_Strings (I).all &
                  ", and user-provided parameters");
            end if;

         end loop;

      exception
         when others =>
            Report.Failed ("Exception raised in Two_Decimal_Place_Data block");
      end Two_Decimal_Place_Data;

      No_Decimal_Place_Data :
         -- Use a decimal fixed point type with delta 1.00 (no decimal places)
         -- and valid picture strings. Evaluate the result of function Image
         -- with the expected result string.
          declare

         use Editing, Fxf3a00;

         Tc_Offset     : constant := 10;
         Tc_Loop_Start : constant := Tc_Offset + 1;                   -- 11
         Tc_Loop_End   : constant :=
           Tc_Loop_Start + Number_Of_Ndp_Items - 1;         -- 22

      begin
         -- The following evaluations correspond to data values of a
         -- decimal type with delta 1.0.

         -- Compare string result of Image with expected edited output
         -- string.  Evaluate data using both default parameters of Image
         -- and user-provided parameter values.
         -- Note: TC_Offset is used to align corresponding data the various
         --       data tables in foundation package FXF3A00.

         for I in Tc_Loop_Start .. Tc_Loop_End loop

            -- Create the picture object from the picture string.
            Tc_Picture := To_Picture (Valid_Strings (I).all);

            -- Use the default parameters for this loop evaluation of Image.
            if not
              (Pack_Ndp.Image (Data_With_Ndp (I - Tc_Offset), Tc_Picture) =
               Edited_Output (Tc_Offset + I).all)
            then
               Report.Failed
                 ("Incorrect result from Function Image, " &
                  "when used with a decimal type with delta " &
                  "1.0, picture string " &
                  Valid_Strings (I).all &
                  ", and the default parameters of Image");
            end if;

            -- Use user-provided parameters for this loop evaluation of Image.
            if Pack_Ndp.Image
                (Item       => Data_With_Ndp (I - Tc_Offset),
                 Pic        => Tc_Picture,
                 Currency   => Tc_Currency,
                 Fill       => Tc_Fill,
                 Separator  => Tc_Separator,
                 Radix_Mark => Tc_Radix_Mark) /=
              Edited_Output (Tc_Offset + I).all
            then
               Report.Failed
                 ("Incorrect result from Function Image, " &
                  "when used with a decimal type with delta " &
                  "1.0, picture string " &
                  Valid_Strings (I).all &
                  ", and user-provided parameters");
            end if;

         end loop;

      exception
         when others =>
            Report.Failed ("Exception raised in No_Decimal_Place_Data block");
      end No_Decimal_Place_Data;

      Exception_Block :
         -- The following three calls of Function Image, using the specific
         -- decimal values and picture strings provided, will cause
         -- a Layout_Error to be raised.
         -- The first two evaluations use the instantiation of Decimal_Output
         -- with a decimal type with delta 0.01, while the last evaluation
         -- uses the instantiation with decimal type with delta 1.0.

         -- Note: The data and the picture strings used in the following
         --       evaluations are not themselves erroneous, but when used in
         --       combination will cause Layout_Error to be raised.

 begin

         for I in 1 .. Fxf3a00.Number_Of_Erroneous_Conditions loop    -- 1..3
            begin
               -- Create the picture object from the picture string.
               Tc_Picture :=
                 Editing.To_Picture (Fxf3a00.Erroneous_Strings (I).all);

               -- Layout_Error must be raised by the following calls to
               -- Function Image.

               if I < 3 then  -- Choose the appropriate instantiation.
                  declare
                     N : constant Natural := Pack_2dp.Length (Tc_Picture);
                     Tc_String : String (1 .. N);
                  begin
                     Tc_String :=
                       Pack_2dp.Image (Fxf3a00.Erroneous_Data (I), Tc_Picture);
                  end;
               else
                  declare
                     use Fxf3a00;
                     N : constant Natural :=
                       Pack_Ndp.Length (Tc_Picture, Tc_Currency);
                     Tc_String : String (1 .. N);
                  begin
                     Tc_String :=
                       Pack_Ndp.Image
                         (Item       => Decimal_Type_Ndp (Erroneous_Data (I)),
                          Pic        => Tc_Picture,
                          Currency   => Tc_Currency,
                          Fill       => Tc_Fill,
                          Separator  => Tc_Separator,
                          Radix_Mark => Tc_Radix_Mark);
                  end;
               end if;

               Report.Failed
                 ("Layout_Error not raised by combination " &
                  "# " &
                  Integer'Image (I) &
                  " " &
                  "of decimal data and picture string");

            exception
               when Layout_Error =>
                  null;    -- Expected exception.
               when others =>
                  Report.Failed
                    ("Incorrect exception raised by combination " &
                     "# " &
                     Integer'Image (I) &
                     " " &
                     "of decimal data and picture string");
            end;
         end loop;

      exception
         when others =>
            Report.Failed ("Unexpected exception raised in Exception_Block");
      end Exception_Block;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxf3a04;
