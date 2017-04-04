-- CXF3A07.A
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
--      Check that Ada.Text_IO.Editing.Put and Ada.Strings.Fixed.Move
--      have the same effect in putting edited output results into string
--      variables.
--
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--
--      The operation of the two above subprograms are examined twice, first
--      with the output of an edited output string to a receiving string
--      object of equal size, the other to a receiving string object of
--      larger size, where justification and padding are considered.
--      The procedure Editing.Put will place an edited output string into
--      a larger receiving string with right justification and blank fill.
--      Procedure Move has parameter control of justification and fill, and
--      in this test will mirror Put by specifying right justification and
--      blank fill.
--
--      In the cases where the edited output string is of shorter length
--      than the receiving string object, a blank-filled constant string
--      will be catenated to the front of the expected edited output string
--      for comparison with the receiving string object, enabling direct
--      string comparison for result verification.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A07.A
--
--
-- CHANGE HISTORY:
--      30 JAN 95   SAIC    Initial prerelease version.
--      11 MAR 97   PWB.CTA Fixed string lengths
--!

with Fxf3a00;
with Ada.Text_Io.Editing;
with Ada.Strings.Fixed;
with Report;

procedure Cxf3a07 is
begin

   Report.Test
     ("CXF3A07",
      "Check that Ada.Text_IO.Editing.Put and " &
      "Ada.Strings.Fixed.Move have the same " &
      "effect in putting edited output results " &
      "into string variables");
   Test_Block : declare

      use Ada.Text_Io;

      -- Instantiate the Decimal_Output generic package for two different
      -- decimal data types.

      package Pack_2dp is          -- Uses decimal type with delta 0.01.
      new Editing.Decimal_Output
        (Fxf3a00.Decimal_Type_2dp);

      package Pack_Ndp is          -- Uses decimal type with delta 1.0.
      new Editing.Decimal_Output
        (Num                => Fxf3a00.Decimal_Type_Ndp,
         Default_Currency   => "$",
         Default_Fill       => '*',
         Default_Separator  => ',',
         Default_Radix_Mark => '.');

      Tc_Picture    : Editing.Picture;
      Tc_Start_Loop : Integer := 0;
      Tc_End_Loop   : Integer := 0;
      Tc_Offset     : Integer := 0;
      Tc_Length     : Natural := 0;

      Tc_Put_String_20,                   -- Longer than the longest edited
      Tc_Move_String_20 : String (1 .. 20);  -- output string.

      Tc_Put_String_17,                   -- Exact length of longest edited
      Tc_Move_String_17 : String
        (1 .. 17);  -- output string in 2DP-US data set.

      Tc_Put_String_8,                    -- Exact length of longest edited
      Tc_Move_String_8 : String
        (1 .. 8);   -- output string in NDP-US data set.

   begin

      -- Examine cases where the output string is longer than the length of the
      -- edited output result. Use the instantiation of Decimal_Output specific
      -- to data with two decimal places.

      Tc_Start_Loop := 1;
      Tc_End_Loop   :=
        Fxf3a00.Number_Of_2dp_Items -              -- 10
        Fxf3a00.Number_Of_Foreign_Strings;

      for I in Tc_Start_Loop .. Tc_End_Loop loop                    -- 1..10

         -- Create the picture object from the picture string.

         Tc_Picture :=
           Editing.To_Picture
             (Fxf3a00.Valid_Strings (I).all,
              Blank_When_Zero => False);

         -- Determine the actual length of the edited output string that is
         -- expected from Put and Image.

         Tc_Length := Pack_2dp.Length (Pic => Tc_Picture, Currency => "$");

         -- Determine the difference in length between the receiving string
         -- object and the expected length of the edited output string. Define
         -- a blank filled string constant with length equal to this length
         -- difference.

         declare
            Tc_Length_Diff   : Integer := Tc_Put_String_20'Length - Tc_Length;
            Tc_Buffer_String : constant String (1 .. Tc_Length_Diff) :=
              (others => ' ');
         begin

            -- Fill the two receiving string objects with edited output, using
            -- the two different methods (Put and Move).

            Pack_2dp.Put
              (To         => Tc_Put_String_20,
               Item       => Fxf3a00.Data_With_2dp (I),
               Pic        => Tc_Picture,
               Currency   => "$",
               Fill       => '*',
               Separator  => ',',
               Radix_Mark => '.');

            Ada.Strings.Fixed.Move
              (Source =>
                 Pack_2dp.Image
                   (Item       => Fxf3a00.Data_With_2dp (I),
                    Pic        => Tc_Picture,
                    Currency   => "$",
                    Fill       => '*',
                    Separator  => ',',
                    Radix_Mark => '.'),
               Target  => Tc_Move_String_20,
               Drop    => Ada.Strings.Error,
               Justify => Ada.Strings.Right,
               Pad     => Ada.Strings.Space);

            -- Each receiving string object is now filled with the edited
            -- output result, right justified. Compare these two string objects
            -- with the expected edited output value, which is appended to the
            -- blank filled string whose length is the difference between the
            -- expected edited output length and the length of the receiving
            -- strings.

            if Tc_Buffer_String & Fxf3a00.Edited_Output (I).all /=
              Tc_Put_String_20 or
              Tc_Buffer_String & Fxf3a00.Edited_Output (I).all /=
                Tc_Move_String_20
            then
               Report.Failed
                 ("Failed case where the output string is " &
                  "longer than the length of the edited " &
                  "output result, loop #" &
                  Integer'Image (I));
            end if;

         exception
            when Layout_Error =>
               Report.Failed
                 ("Layout_Error raised when the output string " &
                  "is longer than the length of the edited " &
                  "output result, loop #" &
                  Integer'Image (I));
            when others =>
               Report.Failed
                 ("Exception raised when the output string is " &
                  "longer than the length of the edited " &
                  "output result, loop #" &
                  Integer'Image (I));
         end;
      end loop;

      -- Repeat the above loop, but only evaluate three cases - those where
      -- the length of the expected edited output string is the exact length
      -- of the receiving strings (no justification will be required within the
      -- string. This series of evaluations again uses decimal data with two
      -- decimal places.

      for I in Tc_Start_Loop .. Tc_End_Loop loop                    -- 1..10

         case I is
            when 1 | 5 | 7 =>

               -- Create the picture object from the picture string.
               Tc_Picture :=
                 Editing.To_Picture (Fxf3a00.Valid_Strings (I).all);

               -- Fill the two receiving string objects with edited output,
               -- using the two different methods (Put and Move). Use default
               -- parameters in the various calls where possible.

               Pack_2dp.Put
                 (To   => Tc_Put_String_17,
                  Item => Fxf3a00.Data_With_2dp (I),
                  Pic  => Tc_Picture);

               Ada.Strings.Fixed.Move
                 (Source =>
                    Pack_2dp.Image
                      (Item => Fxf3a00.Data_With_2dp (I),
                       Pic  => Tc_Picture),
                  Target => Tc_Move_String_17);

               -- Each receiving string object is now filled with the edited
               -- output result. Compare these two string objects with the
               -- expected edited output value.

               if Fxf3a00.Edited_Output (I).all /= Tc_Put_String_17 or
                 Fxf3a00.Edited_Output (I).all /= Tc_Move_String_17
               then
                  Report.Failed
                    ("Failed case where the output string is " &
                     "the exact length of the edited output " &
                     "result, loop #" &
                     Integer'Image (I));
               end if;

            when others =>
               null;
         end case;
      end loop;

      -- Evaluate a mix of cases, where the expected edited output string
      -- length is either exactly as long or shorter than the receiving output
      -- string parameter. This series of evaluations uses decimal data with no
      -- decimal places.

      Tc_Start_Loop := Tc_End_Loop + 1;                           -- 11
      Tc_End_Loop   := Tc_Start_Loop +                            -- 22
      Fxf3a00.Number_Of_Ndp_Items - 1;
      Tc_Offset := Fxf3a00.Number_Of_Foreign_Strings;         -- 10
      -- This offset is required due to the arrangement of data within the
      -- tables found in FXF3A00.

      for I in Tc_Start_Loop .. Tc_End_Loop loop                    -- 11..22

         -- Create the picture object from the picture string.

         Tc_Picture := Editing.To_Picture (Fxf3a00.Valid_Strings (I).all);

         -- Determine the actual length of the edited output string that is
         -- expected from Put and Image.

         Tc_Length := Pack_Ndp.Length (Tc_Picture);

         -- Fill the two receiving string objects with edited output, using the
         -- two different methods (Put and Move).

         Pack_Ndp.Put
           (Tc_Put_String_8,
            Fxf3a00.Data_With_Ndp (I - Tc_Offset),
            Tc_Picture);

         Ada.Strings.Fixed.Move
           (Pack_Ndp.Image (Fxf3a00.Data_With_Ndp (I - Tc_Offset), Tc_Picture),
            Tc_Move_String_8,
            Ada.Strings.Error,
            Ada.Strings.Right,
            Ada.Strings.Space);

         -- Determine if there is a difference in length between the receiving
         -- string object and the expected length of the edited output string.
         -- If so, then define a blank filled string constant with length equal
         -- to this length difference.

         if Tc_Length < Tc_Put_String_8'Length then
            declare
               Tc_Length_Diff : Integer := Tc_Put_String_8'Length - Tc_Length;
               Tc_Buffer_String : constant String (1 .. Tc_Length_Diff) :=
                 (others => ' ');
            begin

               -- Each receiving string object is now filled with the edited
               -- output result, right justified. Compare these two string
               -- objects with the expected edited output value, which is
               -- appended to the blank filled string whose length is the
               -- difference between the expected edited output length and
               -- the length of the receiving strings.

               if Tc_Buffer_String &
                 Fxf3a00.Edited_Output (I + Tc_Offset).all /=
                 Tc_Put_String_8 or
                 Tc_Buffer_String &
                     Fxf3a00.Edited_Output (I + Tc_Offset).all /=
                   Tc_Move_String_8
               then
                  Report.Failed
                    ("Failed case where the output string is " &
                     "longer than the length of the edited " &
                     "output result, loop #" &
                     Integer'Image (I) &
                     ", using data with no decimal places");
               end if;
            end;
         else

            -- Compare these two string objects with the expected edited output
            -- value, which is appended to the blank filled string whose length
            -- is the difference between the expected edited output length and
            -- the length of the receiving strings.

            if Fxf3a00.Edited_Output (I + Tc_Offset).all /= Tc_Put_String_8 or
              Fxf3a00.Edited_Output (I + Tc_Offset).all /= Tc_Move_String_8
            then
               Report.Failed
                 ("Failed case where the output string is " &
                  "the same length as the edited output " &
                  "result, loop #" &
                  Integer'Image (I) &
                  ", using data with no decimal places");
            end if;
         end if;
      end loop;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxf3a07;
