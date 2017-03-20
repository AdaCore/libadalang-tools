-- CXG1003.A
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
--      Check that the subprograms defined in the package Text_IO.Complex_IO
--      provide correct results.
--
-- TEST DESCRIPTION:
--      The generic package Ada.Numerics.Generic_Complex_Types is instantiated
--      with a real type (new Float). The resulting new package is used as
--      the generic actual to package Complex_IO.
--      Two different versions of Put and Get are examined in this test,
--      those that input/output complex data values from/to Text_IO files,
--      and those that input/output complex data values from/to strings.
--      Two procedures are defined to perform the file data manipulations;
--      one to place complex data into the file, and one to retrieve the data
--      from the file and verify its correctness.
--      Complex data is also put into string variables using the Procedure
--      Put for strings, and this data is then retrieved and reconverted into
--      complex values using the Get procedure.
--
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations that:
--         support Annex G,
--         support Text_IO and external files
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Dec 94   SAIC    Modified Width parameter in Get function calls.
--      16 Nov 95   SAIC    Corrected visibility problems for ACVC 2.0.1.
--      29 Sep 96   SAIC    Incorporated reviewer comments.
--
--!

with Ada.Text_Io.Complex_Io;
with Ada.Numerics.Generic_Complex_Types;
with Report;

procedure Cxg1003 is
begin

   Report.Test
     ("CXG1003",
      "Check that the subprograms defined in " &
      "the package Text_IO.Complex_IO " &
      "provide correct results");

   Test_For_Text_Io_Support : declare
      use Ada;

      Data_File     : Ada.Text_Io.File_Type;
      Data_Filename : constant String := Report.Legal_File_Name;

   begin

      -- An application creates a text file in mode Out_File, with the
      -- intention of entering complex data into the file as appropriate.
      -- In the event that the particular environment where the application
      -- is running does not support Text_IO, Use_Error or Name_Error will be
      -- raised on calls to Text_IO operations.  Either of these exceptions
      -- will be handled to produce a Not_Applicable result.

      Text_Io.Create
        (File => Data_File,
         Mode => Ada.Text_Io.Out_File,
         Name => Data_Filename);

      Test_Block : declare

         Tc_Verbose : Boolean := False;

         type Real_Type is new Float;

         package Complex_Pack is new Ada.Numerics.Generic_Complex_Types
           (Real_Type);

         package C_Io is new Ada.Text_Io.Complex_Io (Complex_Pack);

         use Ada.Text_Io, C_Io;
         use type Complex_Pack.Complex;

         Number_Of_Complex_Items : constant := 6;
         Number_Of_Error_Items   : constant := 2;

         Tc_Complex             : Complex_Pack.Complex;
         Tc_Last_Character_Read : Positive;

         Complex_Array : array
           (1 .. Number_Of_Complex_Items) of Complex_Pack.Complex :=
           ((3.0, 9.0),
            (4.0, 7.0),
            (5.0, 6.0),
            (6.0, 3.0),
            (2.0, 5.0),
            (3.0, 7.0));

         procedure Load_Data_File (The_File : in out Text_Io.File_Type) is
            use Ada.Text_Io;
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- This procedure is designed to load complex data into a data
            -- file twice, first using Text_IO, then Complex_IO.  In this
            -- first case, the complex data values are entered as strings,
            -- assuming a variety of legal formats, as provided in the
            -- reference manual.

            Put_Line (The_File, "(3.0, 9.0)");
            Put_Line (The_File, "+4. +7.");    -- Relaxed real literal format.
            Put_Line (The_File, "(5.0 6.)");
            Put_Line (The_File, "6., 3.0");
            Put_Line (The_File, "  (  2.0  ,  5.0  )  ");
            Put_Line (The_File, "(");           -- Complex data separated over
            Put_Line (The_File, "3.0");         -- several (5) lines.
            Put_Line (The_File, " , ");
            Put_Line (The_File, "7.0  ");
            Put_Line (The_File, ")");

            if Tc_Verbose then
               Report.Comment
                 ("Complex values entered into data file using " &
                  "Text_IO, Procedure Load_Data_File");
            end if;

            -- Use the Complex_IO procedure Put to enter Complex data items
            -- into the data file.
            -- Note: Data is being entered into the file for the *second* time
            --       at this point. (Using Complex_IO here, Text_IO above)

            for I in 1 .. Number_Of_Complex_Items loop
               C_Io.Put
                 (File => The_File,
                  Item => Complex_Array (I),
                  Fore => 1,
                  Aft  => 1,
                  Exp  => 0);
            end loop;

            if Tc_Verbose then
               Report.Comment
                 ("Complex values entered into data file using " &
                  "Complex_IO, Procedure Load_Data_File");
            end if;

            Put_Line (The_File, "(5A,3)");      -- data to raise Data_Error.
            Put_Line (The_File, "(3.0,,8.0)");  -- data to raise Data_Error.

         end Load_Data_File;

         procedure Process_Data_File (The_File : in out Text_Io.File_Type) is
            Tc_Complex : Complex_Pack.Complex := (0.0, 0.0);
            Tc_Width   : Integer              := 0;
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- Use procedure Get (for Files) to extract the complex data from
            -- the Text_IO file.  This data was placed into the file using
            -- Text_IO.

            for I in 1 .. Number_Of_Complex_Items loop

               C_Io.Get (The_File, Tc_Complex, Tc_Width);

               if Tc_Complex /= Complex_Array (I) then
                  Report.Failed
                    ("Incorrect complex data read from file " &
                     "when using Text_IO procedure Get, " &
                     "data item #" &
                     Integer'Image (I));
               end if;
            end loop;

            if Tc_Verbose then
               Report.Comment
                 ("First set of complex values extracted " &
                  "from data file using Complex_IO, " &
                  "Procedure Process_Data_File");
            end if;

            -- Use procedure Get (for Files) to extract the complex data from
            -- the Text_IO file.  This data was placed into the file using
            -- procedure Complex_IO.Put.
            -- Note: Data is being extracted from the file for the *second*
            --       time at this point (Using Complex_IO here, Text_IO above)

            for I in 1 .. Number_Of_Complex_Items loop

               C_Io.Get (The_File, Tc_Complex, Tc_Width);

               if Tc_Complex /= Complex_Array (I) then
                  Report.Failed
                    ("Incorrect complex data read from file " &
                     "when using Complex_IO procedure Get, " &
                     "data item #" &
                     Integer'Image (I));
               end if;
            end loop;

            if Tc_Verbose then
               Report.Comment
                 ("Second set of complex values extracted " &
                  "from data file using Complex_IO, " &
                  "Procedure Process_Data_File");
            end if;

            -- The final items in the Data_File are complex values with
            -- incorrect syntax, which should raise Data_Error on an attempt
            -- to read them from the file.
            Tc_Width := 10;
            for I in 1 .. Number_Of_Error_Items loop
               begin
                  C_Io.Get (The_File, Tc_Complex, Tc_Width);
                  Report.Failed
                    ("Exception Data_Error not raised when Complex_IO.Get " &
                     "was used to read complex data with incorrect " &
                     "syntax from the Data_File, data item #" &
                     Integer'Image (I));
               exception
                  when Ada.Text_Io.Data_Error => -- OK, expected exception.
                     Text_Io.Skip_Line (The_File);
                  when others =>
                     Report.Failed
                       ("Unexpected exception raised when Complex_IO.Get " &
                        "was used to read complex data with incorrect " &
                        "syntax from the Data_File, data item #" &
                        Integer'Image (I));
               end;
            end loop;

            if Tc_Verbose then
               Report.Comment
                 ("Erroneous set of complex values extracted " &
                  "from data file using Complex_IO, " &
                  "Procedure Process_Data_File");
            end if;

         exception
            when others =>
               Report.Failed
                 ("Unexpected exception raised in Process_Data_File");
         end Process_Data_File;

      begin  -- Test_Block.

         -- Place complex values into data file.

         Load_Data_File (Data_File);
         Text_Io.Close (Data_File);

         if Tc_Verbose then
            Report.Comment ("Data file loaded with Complex values");
         end if;

         -- Read complex values from data file.

         Text_Io.Open (Data_File, Text_Io.In_File, Data_Filename);
         Process_Data_File (Data_File);

         if Tc_Verbose then
            Report.Comment ("Complex values extracted from data file");
         end if;

         -- Verify versions of Procedures Put and Get for Strings.

         declare
            Tc_String_Array : array
              (1 .. Number_Of_Complex_Items) of String (1 .. 15) :=
              (others => (others => ' '));
         begin

            -- Place complex values into strings using the Procedure Put.

            for I in 1 .. Number_Of_Complex_Items loop
               C_Io.Put
                 (To   => Tc_String_Array (I),
                  Item => Complex_Array (I),
                  Aft  => 1,
                  Exp  => 0);
            end loop;

            if Tc_Verbose then
               Report.Comment ("Complex values placed into string array");
            end if;

            -- Check the format of the strings containing a complex number.
            -- The resulting strings are of 15 character length, with the
            -- real component left justified within the string, followed by
            -- a comma, and with the imaginary component and closing
            -- parenthesis right justified in the string, with blank fill
            -- for the balance of the string.

            if Tc_String_Array (1) /= "(3.0,      9.0)" or
              Tc_String_Array (2) /= "(4.0,      7.0)" or
              Tc_String_Array (3) /= "(5.0,      6.0)" or
              Tc_String_Array (4) /= "(6.0,      3.0)" or
              Tc_String_Array (5) /= "(2.0,      5.0)" or
              Tc_String_Array (6) /= "(3.0,      7.0)"
            then
               Report.Failed
                 ("Incorrect format for complex values that " &
                  "have been placed into string variables " &
                  "using the Complex_IO.Put procedure for " &
                  "strings");
            end if;

            if Tc_Verbose then
               Report.Comment ("String format of Complex values verified");
            end if;

            -- Get complex values from strings using the Procedure Get.
            -- Compare with expected complex values.

            for I in 1 .. Number_Of_Complex_Items loop

               C_Io.Get
                 (From => Tc_String_Array (I),
                  Item => Tc_Complex,
                  Last => Tc_Last_Character_Read);

               if Tc_Complex /= Complex_Array (I) then
                  Report.Failed
                    ("Incorrect complex data value obtained " &
                     "from String following use of Procedures " &
                     "Put and Get from Strings, Complex_Array " &
                     "item #" &
                     Integer'Image (I));
               end if;
            end loop;

            if Tc_Verbose then
               Report.Comment ("Complex values removed from String array");
            end if;

            -- Verify that Layout_Error is raised if the given string is
            -- too short to hold the formatted output.
            Layout_Error_On_Put : declare
               Much_Too_Short : String (1 .. 2);
               Complex_Value  : Complex_Pack.Complex := (5.0, 0.0);
            begin
               C_Io.Put (Much_Too_Short, Complex_Value);
               Report.Failed
                 ("Layout_Error not raised by Procedure Put " &
                  "when the given string was too short to " &
                  "hold the formatted output");
            exception
               when Layout_Error =>
                  null;  -- OK, expected exception.
               when others =>
                  Report.Failed
                    ("Unexpected exception raised by Procedure Put when " &
                     "the given string was too short to hold the " &
                     "formatted output");
            end Layout_Error_On_Put;

            if Tc_Verbose then
               Report.Comment ("Layout Errors verified");
            end if;

         exception
            when others =>
               Report.Failed
                 ("Unexpected exception raised during the " &
                  "evaluation of Put and Get for Strings");
         end;

         -- Place complex values into strings using a variety of legal
         -- complex data formats.
         declare

            type String_Ptr is access String;

            Tc_Complex_String_Array : array
              (1 .. Number_Of_Complex_Items) of String_Ptr :=
              (new String'("(3.0, 9.0  )"),
               new String'("+4.0  +7.0"),
               new String'("(5.0 6.0)"),
               new String'("6.0, 3.0"),
               new String'("  (   2.0   , 5.0  )  "),
               new String'("(3.0              7.0)"));

            -- The following array contains Positive values that correspond
            -- to the last character that will be read by Procedure Get when
            -- given each of the above strings as input.

            Tc_Last_Char_Array : array
              (1 .. Number_Of_Complex_Items) of Positive :=
              (12, 10, 9, 8, 20, 22);

         begin

            -- Get complex values from strings using the Procedure Get.
            -- Compare with expected complex values.

            for I in 1 .. Number_Of_Complex_Items loop

               C_Io.Get
                 (Tc_Complex_String_Array (I).all,
                  Tc_Complex,
                  Tc_Last_Character_Read);

               if Tc_Complex /= Complex_Array (I) then
                  Report.Failed
                    ("Incorrect complex data value obtained from " &
                     "Procedure Get with complex data input of: " &
                     Tc_Complex_String_Array (I).all);
               end if;

               if Tc_Last_Character_Read /= Tc_Last_Char_Array (I) then
                  Report.Failed
                    ("Incorrect value returned as the last character of " &
                     "the input string processed by Procedure Get, " &
                     "string value : " &
                     Tc_Complex_String_Array (I).all &
                     "  expected last character value read : " &
                     Positive'Image (Tc_Last_Char_Array (I)) &
                     "  last character value read : " &
                     Positive'Image (Tc_Last_Character_Read));
               end if;

            end loop;

            if Tc_Verbose then
               Report.Comment
                 ("Complex values removed from strings and " &
                  "verified against expected values");
            end if;

         exception
            when others =>
               Report.Failed
                 ("Unexpected exception raised during the " &
                  "evaluation of Get for Strings");
         end;

      exception
         when others =>
            Report.Failed ("Exception raised in Test_Block");
      end Test_Block;

      -- Delete the external file.
      if Ada.Text_Io.Is_Open (Data_File) then
         Ada.Text_Io.Delete (Data_File);
      else
         Ada.Text_Io.Open (Data_File, Ada.Text_Io.In_File, Data_Filename);
         Ada.Text_Io.Delete (Data_File);
      end if;

   exception

      -- Since Use_Error can be raised if, for the specified mode,
      -- the environment does not support Text_IO operations, the
      -- following handlers are included:

      when Ada.Text_Io.Use_Error =>
         Report.Not_Applicable ("Use_Error raised on Text_IO Create");

      when Ada.Text_Io.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Text_IO Create");

      when others =>
         Report.Failed ("Unexpected exception raised on text file Create");

   end Test_For_Text_Io_Support;

   Report.Result;

end Cxg1003;
