-- CXF3A05.A
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
--      Check that Function Image produces correct results when provided
--      non-default parameters for Currency, Fill, Separator, and
--      Radix_Mark at either the time of package Decimal_Output instantiation,
--      or in a call to Image.  Check non-default parameters that are
--      appropriate for foreign currency representations.
--
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--
--      The results of the Image function, resulting from several different
--      instantiations of Decimal_Output, are compared with expected
--      edited output string results.  The primary focus of this test is to
--      examine the effect of non-default parameters, provided during the
--      instantiation of package Decimal_Output, or provided as part of a
--      call to Function Image (that resulted from an instantiation of
--      Decimal_Output that used default parameters).  The non-default
--      parameters provided correspond to foreign currency representations.
--
--      For each picture string/decimal data combination examined, two
--      evaluations of Image are performed.  These correspond to the two
--      methods of providing the appropriate non-default parameters described
--      above.  Both forms of Function Image should produce the same expected
--      edited output string.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A05.A
--
--
-- CHANGE HISTORY:
--      26 JAN 95   SAIC    Initial prerelease version.
--      17 FEB 97   PWB.CTA Correct array indices for Foreign_Strings array
--                          references.
--!

with Fxf3a00;
with Ada.Text_Io.Editing;
with Report;

procedure Cxf3a05 is
begin

   Report.Test
     ("CXF3A05",
      "Check that Function Image produces " &
      "correct results when provided non-default " &
      "parameters for Currency, Fill, Separator, " &
      "and Radix_Mark, appropriate to foreign " &
      "currency representations");

   Test_Block : declare

      use Ada.Text_Io;

      -- Instantiate the Decimal_Output generic package for the several
      -- combinations of Default_Currency, Default_Fill, Default_Separator,
      -- and Default_Radix_Mark.

      package Pack_Def is                   -- Uses default parameter values.
      new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp);

      package Pack_Ff is new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp,
         Default_Currency   => "FF",
         Default_Fill       => '*',
         Default_Separator  => '.',
         Default_Radix_Mark => ',');

      package Pack_Dm is new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp,
         Default_Currency   => "DM",
         Default_Fill       => '*',
         Default_Separator  => ',',
         Default_Radix_Mark => '.');

      package Pack_Chf is new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp,
         Default_Currency   => "CHF",
         Default_Fill       => '*',
         Default_Separator  => ',',
         Default_Radix_Mark => '.');

      Tc_Picture    : Editing.Picture;
      Tc_Start_Loop : constant := 11;
      Tc_End_Loop   : constant :=
        Tc_Start_Loop +                       -- 20
        Fxf3a00.Number_Of_Foreign_Strings - 1;

   begin

      -- In the case of each particular type of foreign string examined, two
      -- versions of Function Image are examined. First, a version of the
      -- function that originated from an instantiation of Decimal_Output with
      -- non-default parameters is checked. This version of Image is called
      -- making use of default parameters in the actual function call. In
      -- addition, a version of Function Image is checked that resulted from
      -- an instantiation of Decimal_Output using default parameters, but which
      -- uses non-default parameters in the function call.

      for I in Tc_Start_Loop .. Tc_End_Loop loop

         -- Create the picture object from the picture string.

         Tc_Picture :=
           Editing.To_Picture
             (Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all);

         -- Based on the ordering of the specific foreign picture strings in
         -- the FXF3A00.Foreign_Strings table, the following conditional is
         -- used to determine which type of currency is being examined as the
         -- loop executes.

         if I < Tc_Start_Loop + Fxf3a00.Number_Of_Ff_Strings then -- (11-14)
            -- Process the FF picture strings.

            -- Check the result of Function Image from an instantiation of
            -- Decimal_Output that provided non-default actual parameters at
            -- the time of package instantiation, and uses default parameters
            -- in the call of Image.

            if Pack_Ff.Image
                (Item => Fxf3a00.Data_With_2dp (I),
                 Pic  => Tc_Picture) /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with FF " &
                  "related parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all);
            end if;

            -- Check the result of Function Image that originated from an
            -- instantiation of Decimal_Output where default parameters were
            -- used at the time of package Instantiation, but where non-default
            -- parameters are provided in the call of Image.

            if Pack_Def.Image
                (Item       => Fxf3a00.Data_With_2dp (I),
                 Pic        => Tc_Picture,
                 Currency   => "FF",
                 Fill       => '*',
                 Separator  => '.',
                 Radix_Mark => ',') /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with default " &
                  "parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all &
                  ", and FF related parameters in call to Image");
            end if;

         elsif I <
           Tc_Start_Loop +                                -- (15-19)
             Fxf3a00.Number_Of_Ff_Strings +
             Fxf3a00.Number_Of_Dm_Strings
         then
            -- Process the DM picture strings.

            -- Non-default instantiation parameters, default function call
            -- parameters.

            if Pack_Dm.Image
                (Item => Fxf3a00.Data_With_2dp (I),
                 Pic  => Tc_Picture) /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with DM " &
                  "related parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all);
            end if;

            -- Default instantiation parameters, non-default function call
            -- parameters.

            if Pack_Def.Image
                (Item       => Fxf3a00.Data_With_2dp (I),
                 Pic        => Tc_Picture,
                 Currency   => "DM",
                 Fill       => '*',
                 Separator  => ',',
                 Radix_Mark => '.') /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with default " &
                  "parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all &
                  ", and DM related parameters in call to Image");
            end if;

         else                                                     -- (i=20)
            -- Process the CHF string.

            -- Non-default instantiation parameters, default function call
            -- parameters.

            if Pack_Chf.Image (Fxf3a00.Data_With_2dp (I), Tc_Picture) /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with CHF " &
                  "related parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all);
            end if;

            -- Default instantiation parameters, non-default function call
            -- parameters.

            if Pack_Def.Image
                (Fxf3a00.Data_With_2dp (I),
                 Tc_Picture,
                 "CHF",
                 '*',
                 ',',
                 '.') /=
              Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Incorrect output from Function Image " &
                  "from package instantiated with default " &
                  "parameters, using picture string " &
                  Fxf3a00.Foreign_Strings (I - Tc_Start_Loop + 1).all &
                  ", and CHF related parameters in call to Image");
            end if;

         end if;

      end loop;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxf3a05;
