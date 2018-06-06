-- CXACB01.A
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
--      Check that the default attributes 'Input and 'Output work properly when
--      used with objects of a variety of types, including two-dimensional
--      arrays and records without default discriminants.
--
-- TEST DESCRIPTION:
--      This test simulates utility company service record storage, using
--      Stream_IO to allow the storage of heterogeneous data in a single
--      stream file.
--
--      Three types of data are written to the stream file for each utility
--      service customer.
--      First, the general information on the customer is written.
--      This is an object of a discriminated (without default) record
--      type.  This is followed by an integer object containing a count of
--      the number of service months for the customer. Finally, a
--      two-dimensional array object with monthly consumption information for
--      the customer is written to the stream.
--
--      Objects of record types with discriminants without defaults should
--      have their discriminants included in the stream when using 'Output.
--      Likewise, discriminants should be extracted
--      from the stream when using 'Input. Similarly, array bounds are written
--      to and read from the stream when using 'Output and 'Input with array
--      objects.
--
-- APPLICABILITY CRITERIA:
--      Applicable to all implementations that support external
--      Stream_IO files.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Fxacb00;
with Ada.Streams.Stream_Io;
with Report;

procedure Cxacb01 is
begin

   Report.Test
     ("CXACB01",
      "Check that the default attributes 'Input and " &
      "'Output work properly when used with objects " &
      "of record, natural, and array types");

   Test_For_Stream_Io_Support :
   declare

      Util_File                : Ada.Streams.Stream_Io.File_Type;
      Util_Stream              : Ada.Streams.Stream_Io.Stream_Access;
      Utility_Service_Filename : constant String := Report.Legal_File_Name;

   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on
      -- calls to various Stream_IO operations. This block statement encloses a
      -- call to Create, which should produce an exception in a non-supportive
      -- environment. These exceptions will be handled to produce a
      -- Not_Applicable result.

      Ada.Streams.Stream_Io.Create
        (Util_File, Ada.Streams.Stream_Io.Out_File, Utility_Service_Filename);

      Operational_Test_Block :
      declare

         -- The following procedure will store all of the customer specific
         -- information into the stream.

         procedure Store_Data_In_Stream
           (Customer : in Fxacb00.Service_Type;
            Months   : in Fxacb00.Months_In_Service_Type;
            History  : in Fxacb00.Service_History_Type)
         is
         begin
            Fxacb00.Service_Type'Output (Util_Stream, Customer);
            Fxacb00.Months_In_Service_Type'Output (Util_Stream, Months);
            Fxacb00.Service_History_Type'Output (Util_Stream, History);
         end Store_Data_In_Stream;

         -- The following procedure will remove from the stream all of the
         -- customer related information.

         procedure Retrieve_Data_From_Stream
           (Customer : out Fxacb00.Service_Type;
            Months   : out Fxacb00.Months_In_Service_Type;
            History  : out Fxacb00.Service_History_Type)
         is
         begin
            Customer := Fxacb00.Service_Type'Input (Util_Stream);
            Months   := Fxacb00.Months_In_Service_Type'Input (Util_Stream);
            History  := Fxacb00.Service_History_Type'Input (Util_Stream);
         end Retrieve_Data_From_Stream;

      begin

         Util_Stream := Ada.Streams.Stream_Io.Stream (Util_File);

         -- Write all of the customer service information (record, numeric, and
         -- array objects) defined in package FXACB00 into the stream.

         Data_Storage_Block :
         begin

            Store_Data_In_Stream
              (Customer => Fxacb00.Customer1, Months => Fxacb00.C1_Months,
               History  => Fxacb00.C1_Service_History);

            Store_Data_In_Stream
              (Fxacb00.Customer2, Fxacb00.C2_Months,
               History => Fxacb00.C2_Service_History);

            Store_Data_In_Stream
              (Months   => Fxacb00.C3_Months,
               History  => Fxacb00.C3_Service_History,
               Customer => Fxacb00.Customer3);
         end Data_Storage_Block;

         Data_Verification_Block :
         declare

            Tc_Residence  : Fxacb00.Service_Type (Fxacb00.Residence);
            Tc_Apartment  : Fxacb00.Service_Type (Fxacb00.Apartment);
            Tc_Commercial : Fxacb00.Service_Type (Fxacb00.Commercial);

            Tc_Months1, Tc_Months2,
            Tc_Months3 : Fxacb00.Months_In_Service_Type :=
              Fxacb00.Months_In_Service_Type'First;

            Tc_History1 : Fxacb00.Service_History_Type
              (Fxacb00.Quarterly_Period_Type, Fxacb00.Month_In_Quarter_Type) :=
              (others => (others => Fxacb00.Electric_Usage_Type'Last));

            Tc_History2 : Fxacb00.Service_History_Type
              (Fxacb00
                 .Quarterly_Period_Type range Fxacb00.Spring .. Fxacb00.Summer,
               Fxacb00.Month_In_Quarter_Type) :=
              (others => (others => Fxacb00.Electric_Usage_Type'Last));

            Tc_History3 : Fxacb00.Service_History_Type
              (Fxacb00.Quarterly_Period_Type, Fxacb00.Month_In_Quarter_Type) :=
              (others => (others => Fxacb00.Electric_Usage_Type'Last));

         begin

            Ada.Streams.Stream_Io.Reset
              (Util_File, Ada.Streams.Stream_Io.In_File);

            -- Input all of the data that is contained in the stream. Compare
            -- all data with the original data in package FXACB00 that was
            -- written to the stream.

            Retrieve_Data_From_Stream (Tc_Residence, Tc_Months1, Tc_History1);
            Retrieve_Data_From_Stream (Tc_Apartment, Tc_Months2, Tc_History2);
            Retrieve_Data_From_Stream
              (Customer => Tc_Commercial, Months => Tc_Months3,
               History  => Tc_History3);

            -- After all the data has been correctly extracted, the file should
            -- be empty.

            if not Ada.Streams.Stream_Io.End_Of_File (Util_File) then
               Report.Failed ("Stream file not empty");
            end if;

            -- Verify that the data values read from the stream are the same as
            -- those written to the stream.

            if
              ((Fxacb00."/=" (Fxacb00.Customer1, Tc_Residence))
               or else (Fxacb00."/=" (Fxacb00.Customer2, Tc_Apartment))
               or else (Fxacb00."/=" (Fxacb00.Customer3, Tc_Commercial)))
            then
               Report.Failed ("Customer information incorrect");
            end if;

            if
              ((Fxacb00."/=" (Fxacb00.C1_Months, Tc_Months1)) or
               (Fxacb00."/=" (Fxacb00.C2_Months, Tc_Months2)) or
               (Fxacb00."/=" (Fxacb00.C3_Months, Tc_Months3)))
            then
               Report.Failed ("Number of Months information incorrect");
            end if;

            if not
              ((Fxacb00."=" (Fxacb00.C1_Service_History, Tc_History1)) and
               (Fxacb00."=" (Fxacb00.C2_Service_History, Tc_History2)) and
               (Fxacb00."=" (Fxacb00.C3_Service_History, Tc_History3)))
            then
               Report.Failed ("Service history information incorrect");
            end if;

         end Data_Verification_Block;

      exception

         when others =>
            Report.Failed ("Exception raised in Operational Test Block");

      end Operational_Test_Block;

      -- Delete the file.
      if Ada.Streams.Stream_Io.Is_Open (Util_File) then
         Ada.Streams.Stream_Io.Delete (Util_File);
      else
         Ada.Streams.Stream_Io.Open
           (Util_File, Ada.Streams.Stream_Io.Out_File,
            Utility_Service_Filename);
         Ada.Streams.Stream_Io.Delete (Util_File);
      end if;

   exception

      -- Since Use_Error or Name_Error can be raised if, for the specified
      -- mode, the environment does not support Stream_IO operations, the
      -- following handlers are included:

      when Ada.Streams.Stream_Io.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Stream IO Create");

      when Ada.Streams.Stream_Io.Use_Error =>
         Report.Not_Applicable ("Use_Error raised on Stream IO Create");

      when others =>
         Report.Failed ("Unexpected exception raised");

   end Test_For_Stream_Io_Support;

   Report.Result;

end Cxacb01;
