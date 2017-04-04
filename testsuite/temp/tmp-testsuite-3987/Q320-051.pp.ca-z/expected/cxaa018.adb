-- CXAA018.A
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
--      Check that the subprograms defined in the package Text_IO.Modular_IO
--      provide correct results.
--
-- TEST DESCRIPTION:
--      This test checks that the subprograms defined in the
--      Ada.Text_IO.Modular_IO package provide correct results.
--      A modular type is defined and used to instantiate the generic
--      package Ada.Text_IO.Modular_IO.  Values of the modular type are
--      written to a Text_IO file, and to a series of string variables, using
--      different versions of the procedure Put from the instantiated IO
--      package.  These modular data items are retrieved from the file and
--      string variables using the appropriate instantiated version of
--      procedure Get.  A variety of Base and Width parameter values are
--      used in the procedure calls.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that support Text_IO
--      processing and external files.
--
--
-- CHANGE HISTORY:
--      03 Jul 95   SAIC    Initial prerelease version.
--      01 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Ada.Text_Io;
with System;
with Report;

procedure Cxaa018 is
begin

   Report.Test
     ("CXAA018",
      "Check that the subprograms defined in " &
      "the package Text_IO.Modular_IO provide " &
      "correct results");

   Test_For_Text_Io_Support : declare
      Data_File     : Ada.Text_Io.File_Type;
      Data_Filename : constant String := Report.Legal_File_Name;
   begin

      -- An application creates a text file in mode Out_File, with the
      -- intention of entering modular data into the file as appropriate.
      -- In the event that the particular environment where the application
      -- is running does not support Text_IO, Use_Error or Name_Error will be
      -- raised on calls to Text_IO operations.  Either of these exceptions
      -- will be handled to produce a Not_Applicable result.

      Ada.Text_Io.Create
        (File => Data_File,
         Mode => Ada.Text_Io.Out_File,
         Name => Data_Filename);

      Test_Block : declare

         type Mod_Type is mod System.Max_Binary_Modulus;
         -- Max_Binary_Modulus must be at least 2**16, which would result
         -- in a base range of 0..65535 (zero to one less than the given
         -- modulus) for this modular type.

         package Mod_Io is new Ada.Text_Io.Modular_Io (Mod_Type);
         use Ada.Text_Io, Mod_Io;
         use type Mod_Type;

         Number_Of_Modular_Items : constant := 6;
         Number_Of_Error_Items   : constant := 1;

         Tc_Modular             : Mod_Type;
         Tc_Last_Character_Read : Positive;

         Modular_Array : array (1 .. Number_Of_Modular_Items) of Mod_Type :=
           (0, 97, 255, 1_025, 12_097, 65_535);

         procedure Load_File (The_File : in out Ada.Text_Io.File_Type) is
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- This procedure is designed to load Modular_Type data into a
            -- data file.
            --
            -- Use the Modular_IO procedure Put to enter modular data items
            -- into the data file.

            for I in 1 .. Number_Of_Modular_Items loop
               -- Use default Base parameter of 10.
               Mod_Io.Put
                 (File  => Data_File,
                  Item  => Modular_Array (I),
                  Width => 6,
                  Base  => Mod_Io.Default_Base);
            end loop;

            -- Enter data into the file such that on the corresponding "Get"
            -- of this data, Data_Error must be raised.  This value is outside
            -- the base range of Modular_Type.
            -- Text_IO is used to enter the value in the file.

            for I in 1 .. Number_Of_Error_Items loop
               Ada.Text_Io.Put (The_File, "-10");
            end loop;

         end Load_File;

         procedure Process_File (The_File : in out Ada.Text_Io.File_Type) is
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- Use procedure Get (for Files) to extract the modular data from
            -- the Text_IO file.

            for I in 1 .. Number_Of_Modular_Items loop
               Mod_Io.Get (The_File, Tc_Modular, Width => 6);

               if Tc_Modular /= Modular_Array (I) then
                  Report.Failed
                    ("Incorrect modular data read from file " &
                     "data item #" &
                     Integer'Image (I));
               end if;
            end loop;

            -- The final item in the Data_File is a modular value that is
            -- outside the base range 0..Num'Last.  This value should raise
            -- Data_Error on an attempt to "Get" it from the file.

            for I in 1 .. Number_Of_Error_Items loop
               begin
                  Mod_Io.Get (The_File, Tc_Modular, Mod_Io.Default_Width);
                  Report.Failed
                    ("Exception Data_Error not raised when Get " &
                     "was used to read modular data outside base " &
                     "range of type, item # " &
                     Integer'Image (I));
               exception
                  when Ada.Text_Io.Data_Error =>
                     null; -- OK, expected exception.
                  when others =>
                     Report.Failed
                       ("Unexpected exception raised when Get " &
                        "was used to read modular data outside " &
                        "base range of type from Data_File, " &
                        "data item #" &
                        Integer'Image (I));
               end;
            end loop;

         exception
            when others =>
               Report.Failed ("Unexpected exception raised in Process_File");
         end Process_File;

      begin  -- Test_Block.

         -- Place modular values into data file.

         Load_File (Data_File);
         Ada.Text_Io.Close (Data_File);

         -- Read modular values from data file.

         Ada.Text_Io.Open (Data_File, Ada.Text_Io.In_File, Data_Filename);
         Process_File (Data_File);

         -- Verify versions of Modular_IO procedures Put and Get for Strings.

         Modular_Io_In_Strings : declare
            Tc_String_Array : array
              (1 .. Number_Of_Modular_Items) of String (1 .. 30) :=
              (others => (others => ' '));
         begin

            -- Place modular values into strings using the Procedure Put,
            -- Use a variety of different "Base" parameter values.
            -- Note: This version of Put uses the length of the given
            --       string as the value of the "Width" parameter.

            for I in 1 .. 2 loop
               Mod_Io.Put
                 (To   => Tc_String_Array (I),
                  Item => Modular_Array (I),
                  Base => Mod_Io.Default_Base);
            end loop;
            for I in 3 .. 4 loop
               Mod_Io.Put (Tc_String_Array (I), Modular_Array (I), Base => 2);
            end loop;
            for I in 5 .. 6 loop
               Mod_Io.Put (Tc_String_Array (I), Modular_Array (I), 16);
            end loop;

            -- Get modular values from strings using the Procedure Get.
            -- Compare with expected modular values.

            for I in 1 .. Number_Of_Modular_Items loop

               Mod_Io.Get
                 (From => Tc_String_Array (I),
                  Item => Tc_Modular,
                  Last => Tc_Last_Character_Read);

               if Tc_Modular /= Modular_Array (I) then
                  Report.Failed
                    ("Incorrect modular data value obtained " &
                     "from String following use of Procedures " &
                     "Put and Get from Strings, Modular_Array " &
                     "item #" &
                     Integer'Image (I));
               end if;
            end loop;

         exception
            when others =>
               Report.Failed
                 ("Unexpected exception raised during the " &
                  "evaluation of Put and Get for Strings");
         end Modular_Io_In_Strings;

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

end Cxaa018;
