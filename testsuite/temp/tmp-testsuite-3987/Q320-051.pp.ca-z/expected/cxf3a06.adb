-- CXF3A06.A
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
--      Check that Ada.Text_IO.Editing.Put and Ada.Text_IO.Put have the same
--      effect.
--
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--
--      The testing approach used in this test is that of writing edited
--      output data to a text file, using two different approaches.  First,
--      Ada.Text_IO.Put is used, with a call to an instantiated version of
--      Function Image supplied as the actual for parameter Item.  The
--      second approach is to use a version of Function Put from an
--      instantiation of Ada.Text_IO.Editing.Decimal_Output, with the
--      appropriate parameters for decimal data, picture, and format
--      specific parameters.  A call to New_Line follows each Put, so that
--      each entry is placed on a separate line in the text file.
--
--      Edited output for decimal data with two decimal places is in the
--      first loop, and once the data has been written to the file, the
--      text file is closed, then opened in In_File mode.  The edited
--      output data is read from the file, and data on successive lines
--      is compared with the expected edited output result.  The edited
--      output data produced by both of the Put procedures should be
--      identical.
--
--      This process is repeated for decimal data with no decimal places.
--      The file is reopened in Append_File mode, and the edited output
--      data is added to the file in the same manner as described above.
--      The file is closed, and reopened to verify the data written.
--      The data written above (with two decimal places) is skipped, then
--      the data to be verified is extracted as above and verified against
--      the expected edited output string values.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable only to implementations that support
--      external text files.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A06.A
--
--
-- CHANGE HISTORY:
--      26 JAN 95   SAIC    Initial prerelease version.
--      26 FEB 97   PWB.CTA Made input buffers sufficiently long
--                          and removed code depending on shorter buffers
--!

with Fxf3a00;
with Ada.Text_Io.Editing;
with Report;

procedure Cxf3a06 is
   use Ada;
begin

   Report.Test
     ("CXF3A06",
      "Check that Ada.Text_IO.Editing.Put and " &
      "Ada.Text_IO.Put have the same effect");

   Test_For_Text_Io_Support : declare
      Text_File     : Ada.Text_Io.File_Type;
      Text_Filename : constant String := Report.Legal_File_Name (1);
   begin

      -- Use_Error will be raised if Text_IO operations or external files
      -- are not supported.

      Text_Io.Create (Text_File, Text_Io.Out_File, Text_Filename);

      Test_Block : declare
         use Ada.Text_Io;

         -- Instantiate the Decimal_Output generic package for two
         -- different decimal data types.

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
         Tc_Start_Loop : constant := 1;
         Tc_End_Loop_1 : constant :=
           Fxf3a00.Number_Of_2dp_Items -    -- 20-10
           Fxf3a00.Number_Of_Foreign_Strings;
         Tc_End_Loop_2 : constant := Fxf3a00.Number_Of_Ndp_Items;     -- 12
         Tc_Offset     : constant := Fxf3a00.Number_Of_2dp_Items;     -- 20

         Tc_String_1, Tc_String_2 : String (1 .. 255) := (others => ' ');
         Tc_Last_1, Tc_Last_2     : Natural           := 0;

      begin

         -- Use the two versions of Put, for data with two decimal points,
         -- to write edited output strings to the text file.  Use a separate
         -- line for each string entry.

         for I in Tc_Start_Loop .. Tc_End_Loop_1 loop               -- 1..10

            -- Create the picture object from the picture string.

            Tc_Picture := Editing.To_Picture (Fxf3a00.Valid_Strings (I).all);

            -- Use the Text_IO version of Put to place an edited output
            -- string into a text file. Use default parameters in the call
            -- to Image for Currency, Fill, Separator, and Radix_Mark.

            Text_Io.Put
              (Text_File,
               Pack_2dp.Image
                 (Item => Fxf3a00.Data_With_2dp (I),
                  Pic  => Tc_Picture));
            Text_Io.New_Line (Text_File);

            -- Use the version of Put from the instantiation of
            -- Decimal_Output to place an edited output string on a separate
            -- line of the Text_File.  Use default parameters for Currency,
            -- Fill, Separator, and Radix_Mark.

            Pack_2dp.Put
              (File => Text_File,
               Item => Fxf3a00.Data_With_2dp (I),
               Pic  => Tc_Picture);
            Text_Io.New_Line (Text_File);

         end loop;

         Text_Io.Close (Text_File);

         -- Reopen the text file in In_File mode, and verify the edited
         -- output found on consecutive lines of the file.

         Text_Io.Open (Text_File, Text_Io.In_File, Text_Filename);

         for I in Tc_Start_Loop .. Tc_End_Loop_1 loop
            -- Read successive lines in the text file.
            Text_Io.Get_Line (Text_File, Tc_String_1, Tc_Last_1);
            Text_Io.Get_Line (Text_File, Tc_String_2, Tc_Last_2);

            -- Compare the two strings for equality with the expected edited
            -- output result.  Failure results if strings don't match, or if
            -- a reading error occurred from the attempted Get_Line resulting
            -- from an improperly formed edited output string.

            if Tc_String_1 (1 .. Tc_Last_1) /= Fxf3a00.Edited_Output (I).all or
              Tc_String_2 (1 .. Tc_Last_2) /= Fxf3a00.Edited_Output (I).all
            then
               Report.Failed
                 ("Failed comparison of two edited output " &
                  "strings from data with two decimal points " &
                  ", loop number = " &
                  Integer'Image (I));
            end if;
         end loop;

         Text_Io.Close (Text_File);

         -- Reopen the text file in Append_File mode.
         -- Use the two versions of Put, for data with no decimal points,
         -- to write edited output strings to the text file.  Use a separate
         -- line for each string entry.

         Text_Io.Open (Text_File, Text_Io.Append_File, Text_Filename);

         for I in Tc_Start_Loop .. Tc_End_Loop_2 loop               -- 1..12

            -- Create the picture object from the picture string specific to
            -- data with no decimal points.  Use appropriate offset into the
            -- Valid_Strings array to account for the string data used above.

            Tc_Picture :=
              Editing.To_Picture
                (Fxf3a00.Valid_Strings (I + Tc_End_Loop_1).all);

            -- Use the Text_IO version of Put to place an edited output
            -- string into a text file.  Use non-default parameters in the
            -- call to Image for Currency, Fill, Separator, and Radix_Mark.

            Text_Io.Put
              (Text_File,
               Pack_Ndp.Image
                 (Item       => Fxf3a00.Data_With_Ndp (I),
                  Pic        => Tc_Picture,
                  Currency   => "$",
                  Fill       => '*',
                  Separator  => ',',
                  Radix_Mark => '.'));
            Text_Io.New_Line (Text_File);

            -- Use the version of Put from the instantiation of
            -- Decimal_Output to place an edited output string on a separate
            -- line of the Text_File.  Use non-default parameters for
            -- Currency, Fill, Separator, and Radix_Mark.

            Pack_Ndp.Put
              (File       => Text_File,
               Item       => Fxf3a00.Data_With_Ndp (I),
               Pic        => Tc_Picture,
               Currency   => "$",
               Fill       => '*',
               Separator  => ',',
               Radix_Mark => '.');
            Text_Io.New_Line (Text_File);

         end loop;

         Text_Io.Close (Text_File);

         -- Reopen the text file in In_File mode, and verify the edited
         -- output found on consecutive lines of the file.

         Text_Io.Open (Text_File, Text_Io.In_File, Text_Filename);

         -- Read past data that has been verified above, skipping two lines
         -- of the data file for each loop.

         for I in Tc_Start_Loop .. Tc_End_Loop_1 loop               -- 1..10
            Text_Io.Skip_Line (Text_File, 2);
         end loop;

         -- Verify the last data set that was written to the file.

         for I in Tc_Start_Loop .. Tc_End_Loop_2 loop               -- 1..12
            Text_Io.Get_Line (Text_File, Tc_String_1, Tc_Last_1);
            Text_Io.Get_Line (Text_File, Tc_String_2, Tc_Last_2);

            -- Compare the two strings for equality with the expected edited
            -- output result.  Failure results if strings don't match, or if
            -- a reading error occurred from the attempted Get_Line resulting
            -- from an improperly formed edited output string.

            if Tc_String_1 (1 .. Tc_Last_1) /=
              Fxf3a00.Edited_Output (I + Tc_Offset).all or
              Tc_String_2 (1 .. Tc_Last_2) /=
                Fxf3a00.Edited_Output (I + Tc_Offset).all
            then
               Report.Failed
                 ("Failed comparison of two edited output " &
                  "strings from data with no decimal points " &
                  ", loop number = " &
                  Integer'Image (I));
            end if;

         end loop;

      exception
         when others =>
            Report.Failed ("Exception raised in Test_Block");
      end Test_Block;

      -- Delete the external file.
      if Text_Io.Is_Open (Text_File) then
         Text_Io.Delete (Text_File);
      else
         Text_Io.Open (Text_File, Text_Io.In_File, Text_Filename);
         Text_Io.Delete (Text_File);
      end if;

   exception

      -- Since Use_Error can be raised if, for the specified mode,
      -- the environment does not support Text_IO operations, the
      -- following handlers are included:

      when Text_Io.Use_Error =>
         Report.Not_Applicable ("Use_Error raised on Text_IO Create");

      when Text_Io.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Text_IO Create");

      when others =>
         Report.Failed ("Unexpected exception raised in Create block");

   end Test_For_Text_Io_Support;

   Report.Result;

end Cxf3a06;
