-- CXAC004.A
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
--      Check that the Stream_Access type and Stream function found in package
--      Ada.Text_IO.Text_Streams allows a text file to be processed with the
--      functionality of streams.
--
-- TEST DESCRIPTION:
--      This test verifies that the package Ada.Text_IO.Text_Streams is
--      available and that the functionality it contains allows a text file to
--      be manipulated as a stream.
--      The test defines data objects of a variety of types that can be stored
--      in a text file.  A text file and associated text stream are then
--      defined, and the 'Write attribute is used to enter the individual data
--      items into the text stream.  Once all the individual data items have
--      been written to the stream, the 'Output attribute is used to write
--      arrays of these same data objects to the stream.
--      The text file is reset to serve as an input file, and the 'Read
--      attribute is used to extract the individual data items from the
--      stream.  These items are then verified against the data originally
--      written to the stream. Finally, the 'Input attribute is used to
--      extract the data arrays from the stream. These arrays are then
--      verified against the original data written to the stream.
--
-- APPLICABILITY CRITERIA:
--      Applicable to implementations that support external text files.
--
-- CHANGE HISTORY:
--      06 Jul 95   SAIC    Initial prerelease version.
--      26 Feb 97   PWB.CTA Allowed for non-support of some IO operations;
--                          removed requirement for support of decimal types.
--!

with Report;
with Ada.Text_Io;
with Ada.Text_Io.Text_Streams;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

procedure Cxac004 is

   Data_File     : Ada.Text_Io.File_Type;
   Data_Filename : constant String :=
     Report.Legal_File_Name (Nam => "CXAC004");
   Incomplete : exception;

begin

   Report.Test
     ("CXAC004",
      "Check that the Stream_Access type and Stream " &
      "function found in package " &
      "Ada.Text_IO.Text_Streams allows a text file to " &
      "be processed with the functionality of streams");

   Test_For_Io_Support : begin

      -- Check for Text_IO support in creating the data file.  If the
      -- implementation does not support external files, Name_Error or
      -- Use_Error will be raised at the point of the following call to
      -- Create, resulting in a Not_Applicable test result.

      Ada.Text_Io.Create (Data_File, Ada.Text_Io.Out_File, Data_Filename);

   exception

      when Ada.Text_Io.Use_Error | Ada.Text_Io.Name_Error =>
         Report.Not_Applicable
           ("Files not supported - Create as Out_File for Text_IO");
         raise Incomplete;

   end Test_For_Io_Support;

   Test_Block : declare
      use Ada.Characters.Latin_1, Ada.Strings.Unbounded;
      Tc_Items : constant := 3;

      -- Declare types and objects that will be used as data values to be
      -- written to and read from the text file/stream.

      type Enum_Type is (Red, Yellow, Green, Blue, Indigo);
      type Fixed_Type is delta 0.125 range 0.0 .. 255.0;
      type Float_Type is digits 7 range 0.0 .. 1.0E5;
      type Modular_Type is mod 256;
      subtype Str_Type is String (1 .. 4);

      type Char_Array_Type is array (1 .. Tc_Items) of Character;
      type Enum_Array_Type is array (1 .. Tc_Items) of Enum_Type;
      type Fixed_Array_Type is array (1 .. Tc_Items) of Fixed_Type;
      type Float_Array_Type is array (1 .. Tc_Items) of Float_Type;
      type Int_Array_Type is array (1 .. Tc_Items) of Integer;
      type Mod_Array_Type is array (1 .. Tc_Items) of Modular_Type;
      type Str_Array_Type is array (1 .. Tc_Items) of Str_Type;
      type Unb_Str_Array_Type is array (1 .. Tc_Items) of Unbounded_String;

      Char_Array : Char_Array_Type := ('A', 'z', Yen_Sign);
      Tc_Char_Array_1, Tc_Char_Array_2 : Char_Array_Type := (others => Space);

      Enum_Array : Enum_Array_Type := (Blue, Yellow, Indigo);
      Tc_Enum_Array_1, Tc_Enum_Array_2 : Enum_Array_Type := (others => Red);

      Fix_Array : Fixed_Array_Type := (0.125, 123.5, 250.750);
      Tc_Fix_Array_1, Tc_Fix_Array_2 : Fixed_Array_Type := (others => 0.0);

      Flt_Array : Float_Array_Type := (1.0, 150.0, 1_500.0);
      Tc_Flt_Array_1, Tc_Flt_Array_2 : Float_Array_Type := (others => 0.0);

      Int_Array                      : Int_Array_Type := (124, 2_349, -24_001);
      Tc_Int_Array_1, Tc_Int_Array_2 : Int_Array_Type := (others => -99);

      Mod_Array                      : Mod_Array_Type := (10, 127, 255);
      Tc_Mod_Array_1, Tc_Mod_Array_2 : Mod_Array_Type := (others => 0);

      Str_Array : Str_Array_Type := ("abcd", "klmn", "wxyz");
      Tc_Str_Array_1, Tc_Str_Array_2 : Str_Array_Type := (others => "    ");

      Ustr_Array : Unb_Str_Array_Type :=
        (To_Unbounded_String ("cat"),
         To_Unbounded_String ("testing"),
         To_Unbounded_String ("ACVC"));
      Tc_Ustr_Array_1,
      Tc_Ustr_Array_2 : Unb_Str_Array_Type :=
        (others => Null_Unbounded_String);

      -- Create a stream access object pointing to the data file.

      Data_Stream : Ada.Text_Io.Text_Streams.Stream_Access :=
        Ada.Text_Io.Text_Streams.Stream (File => Data_File);

   begin

      -- Use the 'Write attribute to enter the three sets of data items
      -- into the data stream.
      -- Note that the data will be mixed within the text file.

      for I in 1 .. Tc_Items loop
         Character'Write (Data_Stream, Char_Array (I));
         Enum_Type'Write (Data_Stream, Enum_Array (I));
         Fixed_Type'Write (Data_Stream, Fix_Array (I));
         Float_Type'Write (Data_Stream, Flt_Array (I));
         Integer'Write (Data_Stream, Int_Array (I));
         Modular_Type'Write (Data_Stream, Mod_Array (I));
         Str_Type'Write (Data_Stream, Str_Array (I));
         Unbounded_String'Write (Data_Stream, Ustr_Array (I));
      end loop;

      -- Use the 'Output attribute to enter the entire arrays of each
      -- type of data items into the data stream.
      -- Note that the array bounds will be written to the stream as part
      -- of the action of the 'Output attribute.

      Char_Array_Type'Output (Data_Stream, Char_Array);
      Enum_Array_Type'Output (Data_Stream, Enum_Array);
      Fixed_Array_Type'Output (Data_Stream, Fix_Array);
      Float_Array_Type'Output (Data_Stream, Flt_Array);
      Int_Array_Type'Output (Data_Stream, Int_Array);
      Mod_Array_Type'Output (Data_Stream, Mod_Array);
      Str_Array_Type'Output (Data_Stream, Str_Array);
      Unb_Str_Array_Type'Output (Data_Stream, Ustr_Array);

      -- Reset the data file to mode In_File.  The data file will now serve
      -- as the source of data which will be compared to the original data
      -- written to the file above.
      Reset1 : begin
         Ada.Text_Io.Reset (File => Data_File, Mode => Ada.Text_Io.In_File);
      exception
         when Ada.Text_Io.Use_Error =>
            Report.Not_Applicable
              ("Reset to In_File not supported for Text_IO");
            raise Incomplete;
      end Reset1;

      -- Extract and validate all the single data items from the stream.

      for I in 1 .. Tc_Items loop
         Character'Read (Data_Stream, Tc_Char_Array_1 (I));
         Enum_Type'Read (Data_Stream, Tc_Enum_Array_1 (I));
         Fixed_Type'Read (Data_Stream, Tc_Fix_Array_1 (I));
         Float_Type'Read (Data_Stream, Tc_Flt_Array_1 (I));
         Integer'Read (Data_Stream, Tc_Int_Array_1 (I));
         Modular_Type'Read (Data_Stream, Tc_Mod_Array_1 (I));
         Str_Type'Read (Data_Stream, Tc_Str_Array_1 (I));
         Unbounded_String'Read (Data_Stream, Tc_Ustr_Array_1 (I));
      end loop;

      if Tc_Char_Array_1 /= Char_Array then
         Report.Failed ("Character values do not match");
      end if;
      if Tc_Enum_Array_1 /= Enum_Array then
         Report.Failed ("Enumeration values do not match");
      end if;
      if Tc_Fix_Array_1 /= Fix_Array then
         Report.Failed ("Fixed point values do not match");
      end if;
      if Tc_Flt_Array_1 /= Flt_Array then
         Report.Failed ("Floating point values do not match");
      end if;
      if Tc_Int_Array_1 /= Int_Array then
         Report.Failed ("Integer values do not match");
      end if;
      if Tc_Mod_Array_1 /= Mod_Array then
         Report.Failed ("Modular values do not match");
      end if;
      if Tc_Str_Array_1 /= Str_Array then
         Report.Failed ("String values do not match");
      end if;
      if Tc_Ustr_Array_1 /= Ustr_Array then
         Report.Failed ("Unbounded_String values do not match");
      end if;

      -- Extract and validate all data arrays from the data stream.
      -- Note that the 'Input attribute denotes a function, whereas the
      -- other stream oriented attributes in this test denote procedures.

      Tc_Char_Array_2 := Char_Array_Type'Input (Data_Stream);
      Tc_Enum_Array_2 := Enum_Array_Type'Input (Data_Stream);
      Tc_Fix_Array_2  := Fixed_Array_Type'Input (Data_Stream);
      Tc_Flt_Array_2  := Float_Array_Type'Input (Data_Stream);
      Tc_Int_Array_2  := Int_Array_Type'Input (Data_Stream);
      Tc_Mod_Array_2  := Mod_Array_Type'Input (Data_Stream);
      Tc_Str_Array_2  := Str_Array_Type'Input (Data_Stream);
      Tc_Ustr_Array_2 := Unb_Str_Array_Type'Input (Data_Stream);

      if Tc_Char_Array_2 /= Char_Array then
         Report.Failed ("Character array values do not match");
      end if;
      if Tc_Enum_Array_2 /= Enum_Array then
         Report.Failed ("Enumeration array values do not match");
      end if;
      if Tc_Fix_Array_2 /= Fix_Array then
         Report.Failed ("Fixed point array values do not match");
      end if;
      if Tc_Flt_Array_2 /= Flt_Array then
         Report.Failed ("Floating point array values do not match");
      end if;
      if Tc_Int_Array_2 /= Int_Array then
         Report.Failed ("Integer array values do not match");
      end if;
      if Tc_Mod_Array_2 /= Mod_Array then
         Report.Failed ("Modular array values do not match");
      end if;
      if Tc_Str_Array_2 /= Str_Array then
         Report.Failed ("String array values do not match");
      end if;
      if Tc_Ustr_Array_2 /= Ustr_Array then
         Report.Failed ("Unbounded_String array values do not match");
      end if;

   exception
      when Incomplete =>
         raise;
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Deletion : begin
      -- Delete the data file.
      if not Ada.Text_Io.Is_Open (Data_File) then
         Ada.Text_Io.Open (Data_File, Ada.Text_Io.In_File, Data_Filename);
      end if;
      Ada.Text_Io.Delete (Data_File);

   exception
      when others =>
         Report.Failed ("Delete not properly implemented for Text_IO");

   end Deletion;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others =>
      Report.Failed ("Unexpected exception");
      Report.Result;

end Cxac004;
