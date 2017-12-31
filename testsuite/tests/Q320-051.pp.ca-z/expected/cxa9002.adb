---

with Report;
with Ada.Storage_Io;
with Ada.Direct_Io;
with Ada.Tags;
with Cxa9002_0;

procedure Cxa9002 is
   package Dir_Io is new Ada.Direct_Io (Integer);
   Test_File : Dir_Io.File_Type;
   Incomplete : exception;
begin

   Report.Test
     ("CXA9002",
      "Check that the operations defined in the " &
      "generic package Ada.Storage_IO provide the " &
      "ability to store and retrieve objects of " &
      "tagged types from in-memory buffers");

   Test_For_Direct_Io_Support :
   begin

      -- The following Create does not have any bearing on the test scenario,
      -- but is included to check that the implementation supports Direct_IO
      -- files. An exception on this Create statement will raise a Name_Error
      -- or Use_Error, which will be handled to produce a Not_Applicable
      -- result. If created, the file is immediately deleted, as it is not
      -- needed for the program scenario.

      Dir_Io.Create (Test_File, Dir_Io.Out_File, Report.Legal_File_Name (1));
   exception

      when Dir_Io.Use_Error | Dir_Io.Name_Error =>
         Report.Not_Applicable
           ("Files not supported - Create as Out_File for Direct_IO");
         raise Incomplete;

   end Test_For_Direct_Io_Support;

   Deletion :
   begin
      Dir_Io.Delete (Test_File);
   exception
      when others =>
         Report.Failed ("Delete not properly implemented for Direct_IO");
   end Deletion;

   Test_Block :
   declare

      use Cxa9002_0;

      Acct_Filename : constant String := Report.Legal_File_Name (1);
      Cash_Filename : constant String := Report.Legal_File_Name (2);
      Inv_Filename  : constant String := Report.Legal_File_Name (3);
      Chk_Filename  : constant String := Report.Legal_File_Name (4);
      Sav_Filename  : constant String := Report.Legal_File_Name (5);

      type Tag_Pointer_Type is access String;

      Tc_Account_Type_Tag, Tc_Cash_Account_Type_Tag,
      Tc_Investment_Account_Type_Tag, Tc_Checking_Account_Type_Tag,
      Tc_Savings_Account_Type_Tag : Tag_Pointer_Type;

      Tc_Account : Account_Type := (Num => "123");

      Tc_Cash_Account : Cash_Account_Type :=
        (Num => "234", Years_As_Customer => 3);

      Tc_Investment_Account : Investment_Account_Type :=
        (Num => "456", Investment_Vehicle => Bonds);

      Tc_Checking_Account : Checking_Account_Type :=
        (Num => "567", Years_As_Customer => 2, Checks_Per_Year => 300,
         Interest_Bearing => True);

      Tc_Savings_Account : Savings_Account_Type :=
        (Num => "789", Years_As_Customer => 14, Kind => Business);

      procedure Buffer_Data is

         Account            : Account_Type            := Tc_Account;
         Cash_Account       : Cash_Account_Type       := Tc_Cash_Account;
         Investment_Account : Investment_Account_Type := Tc_Investment_Account;
         Checking_Account   : Checking_Account_Type   := Tc_Checking_Account;
         Savings_Account    : Savings_Account_Type    := Tc_Savings_Account;

         -- The instantiations below are a central point in this test.
         -- Storage_IO is instantiated for each of the specific tagged type.
         -- These instantiated packages will be used to compress tagged objects
         -- of these various types into buffers that will be written to the
         -- Direct_IO files declared below.

         package Acct_Sio is new Ada.Storage_Io (Account_Type);
         package Cash_Sio is new Ada.Storage_Io (Cash_Account_Type);
         package Inv_Sio is new Ada.Storage_Io (Investment_Account_Type);
         package Chk_Sio is new Ada.Storage_Io (Checking_Account_Type);
         package Sav_Sio is new Ada.Storage_Io (Savings_Account_Type);

         -- Direct_IO is instantiated for the buffer types defined in the
         -- instantiated Storage_IO packages.

         package Acct_Dio is new Ada.Direct_Io (Acct_Sio.Buffer_Type);
         package Cash_Dio is new Ada.Direct_Io (Cash_Sio.Buffer_Type);
         package Inv_Dio is new Ada.Direct_Io (Inv_Sio.Buffer_Type);
         package Chk_Dio is new Ada.Direct_Io (Chk_Sio.Buffer_Type);
         package Sav_Dio is new Ada.Direct_Io (Sav_Sio.Buffer_Type);

         Acct_Buffer : Acct_Sio.Buffer_Type;
         Cash_Buffer : Cash_Sio.Buffer_Type;
         Inv_Buffer  : Inv_Sio.Buffer_Type;
         Chk_Buffer  : Chk_Sio.Buffer_Type;
         Sav_Buffer  : Sav_Sio.Buffer_Type;

         Acct_File : Acct_Dio.File_Type;
         Cash_File : Cash_Dio.File_Type;
         Inv_File  : Inv_Dio.File_Type;
         Chk_File  : Chk_Dio.File_Type;
         Sav_File  : Sav_Dio.File_Type;

      begin

         Acct_Dio.Create (Acct_File, Acct_Dio.Out_File, Acct_Filename);
         Cash_Dio.Create (Cash_File, Cash_Dio.Out_File, Cash_Filename);
         Inv_Dio.Create (Inv_File, Inv_Dio.Out_File, Inv_Filename);
         Chk_Dio.Create (Chk_File, Chk_Dio.Out_File, Chk_Filename);
         Sav_Dio.Create (Sav_File, Sav_Dio.Out_File, Sav_Filename);

         -- Store the tag values of the objects declared above for comparison
         -- with tag values of objects following processing.

         Tc_Account_Type_Tag :=
           new String'(Ada.Tags.External_Tag (Account_Type'Tag));

         Tc_Cash_Account_Type_Tag :=
           new String'(Ada.Tags.External_Tag (Cash_Account_Type'Tag));

         Tc_Investment_Account_Type_Tag :=
           new String'(Ada.Tags.External_Tag (Investment_Account_Type'Tag));

         Tc_Checking_Account_Type_Tag :=
           new String'(Ada.Tags.External_Tag (Checking_Account_Type'Tag));

         Tc_Savings_Account_Type_Tag :=
           new String'(Ada.Tags.External_Tag (Savings_Account_Type'Tag));

         -- Prepare tagged data for writing to the Direct_IO files using
         -- Storage_IO procedure to place data in buffers.

         Acct_Sio.Write (Buffer => Acct_Buffer, Item => Account);
         Cash_Sio.Write (Cash_Buffer, Cash_Account);
         Inv_Sio.Write (Inv_Buffer, Item => Investment_Account);
         Chk_Sio.Write (Buffer => Chk_Buffer, Item => Checking_Account);
         Sav_Sio.Write (Sav_Buffer, Savings_Account);

         -- At this point, the data and associated tag values have been
         -- buffered by the Storage_IO procedure, and the buffered data can
         -- be written to the appropriate Direct_IO file.

         Acct_Dio.Write (File => Acct_File, Item => Acct_Buffer);
         Cash_Dio.Write (Cash_File, Cash_Buffer);
         Inv_Dio.Write (Inv_File, Item => Inv_Buffer);
         Chk_Dio.Write (File => Chk_File, Item => Chk_Buffer);
         Sav_Dio.Write (Sav_File, Sav_Buffer);

         -- Close all Direct_IO files.

         Acct_Dio.Close (Acct_File);
         Cash_Dio.Close (Cash_File);
         Inv_Dio.Close (Inv_File);
         Chk_Dio.Close (Chk_File);
         Sav_Dio.Close (Sav_File);

      exception
         when others =>
            Report.Failed ("Exception raised in Buffer_Data");
      end Buffer_Data;

      procedure Read_Data is

         Account            : Account_Type;
         Cash_Account       : Cash_Account_Type;
         Investment_Account : Investment_Account_Type;
         Checking_Account   : Checking_Account_Type;
         Savings_Account    : Savings_Account_Type;

         -- Storage_IO is instantiated for each of the specific tagged type.

         package Acct_Sio is new Ada.Storage_Io (Account_Type);
         package Cash_Sio is new Ada.Storage_Io (Cash_Account_Type);
         package Inv_Sio is new Ada.Storage_Io (Investment_Account_Type);
         package Chk_Sio is new Ada.Storage_Io (Checking_Account_Type);
         package Sav_Sio is new Ada.Storage_Io (Savings_Account_Type);

         -- Direct_IO is instantiated for the buffer types defined in the
         -- instantiated Storage_IO packages.

         package Acct_Dio is new Ada.Direct_Io (Acct_Sio.Buffer_Type);
         package Cash_Dio is new Ada.Direct_Io (Cash_Sio.Buffer_Type);
         package Inv_Dio is new Ada.Direct_Io (Inv_Sio.Buffer_Type);
         package Chk_Dio is new Ada.Direct_Io (Chk_Sio.Buffer_Type);
         package Sav_Dio is new Ada.Direct_Io (Sav_Sio.Buffer_Type);

         Acct_Buffer : Acct_Sio.Buffer_Type;
         Cash_Buffer : Cash_Sio.Buffer_Type;
         Inv_Buffer  : Inv_Sio.Buffer_Type;
         Chk_Buffer  : Chk_Sio.Buffer_Type;
         Sav_Buffer  : Sav_Sio.Buffer_Type;

         Acct_File : Acct_Dio.File_Type;
         Cash_File : Cash_Dio.File_Type;
         Inv_File  : Inv_Dio.File_Type;
         Chk_File  : Chk_Dio.File_Type;
         Sav_File  : Sav_Dio.File_Type;

      begin

         -- Open the Direct_IO files.

         Acct_Dio.Open (Acct_File, Acct_Dio.In_File, Acct_Filename);
         Cash_Dio.Open (Cash_File, Cash_Dio.In_File, Cash_Filename);
         Inv_Dio.Open (Inv_File, Inv_Dio.In_File, Inv_Filename);
         Chk_Dio.Open (Chk_File, Chk_Dio.In_File, Chk_Filename);
         Sav_Dio.Open (Sav_File, Sav_Dio.In_File, Sav_Filename);

         -- Read the buffer data from the files using Direct_IO.

         Acct_Dio.Read (File => Acct_File, Item => Acct_Buffer);
         Cash_Dio.Read (Cash_File, Cash_Buffer);
         Inv_Dio.Read (Inv_File, Item => Inv_Buffer);
         Chk_Dio.Read (File => Chk_File, Item => Chk_Buffer);
         Sav_Dio.Read (Sav_File, Sav_Buffer);

         -- At this point, the data and associated tag values are stored in
         -- buffers. Use the Storage_IO procedure Read to recreate the tagged
         -- objects from the buffers.

         Acct_Sio.Read (Buffer => Acct_Buffer, Item => Account);
         Cash_Sio.Read (Cash_Buffer, Cash_Account);
         Inv_Sio.Read (Inv_Buffer, Item => Investment_Account);
         Chk_Sio.Read (Buffer => Chk_Buffer, Item => Checking_Account);
         Sav_Sio.Read (Sav_Buffer, Savings_Account);

         -- Delete all Direct_IO files.

         Acct_Dio.Delete (Acct_File);
         Cash_Dio.Delete (Cash_File);
         Inv_Dio.Delete (Inv_File);
         Chk_Dio.Delete (Chk_File);
         Sav_Dio.Delete (Sav_File);

         Data_Verification_Block :
         begin

            if Account /= Tc_Account then
               Report.Failed ("Incorrect Account object reconstructed");
            end if;

            if Cash_Account /= Tc_Cash_Account then
               Report.Failed ("Incorrect Cash_Account object reconstructed");
            end if;

            if Investment_Account /= Tc_Investment_Account then
               Report.Failed
                 ("Incorrect Investment_Account object reconstructed");
            end if;

            if Checking_Account /= Tc_Checking_Account then
               Report.Failed
                 ("Incorrect Checking_Account object reconstructed");
            end if;

            if Savings_Account /= Tc_Savings_Account then
               Report.Failed
                 ("Incorrect Savings_Account object reconstructed");
            end if;

         exception
            when others =>
               Report.Failed
                 ("Exception raised during Data_Verification Block");
         end Data_Verification_Block;

         -- To ensure that the tags of the values reconstructed by Storage_IO
         -- were properly preserved, object tag values following object
         -- reconstruction are compared with tag values of objects stored
         -- prior to processing.

         Tag_Verification_Block :
         begin

            if Tc_Account_Type_Tag.all /=
              Ada.Tags.External_Tag (Account_Type'Class (Account)'Tag) then
               Report.Failed ("Incorrect Account tag");
            end if;

            if Tc_Cash_Account_Type_Tag.all /=
              Ada.Tags.External_Tag
                (Cash_Account_Type'Class (Cash_Account)'Tag)
            then
               Report.Failed ("Incorrect Cash_Account tag");
            end if;

            if Tc_Investment_Account_Type_Tag.all /=
              Ada.Tags.External_Tag
                (Investment_Account_Type'Class (Investment_Account)'Tag)
            then
               Report.Failed ("Incorrect Investment_Account tag");
            end if;

            if Tc_Checking_Account_Type_Tag.all /=
              Ada.Tags.External_Tag
                (Checking_Account_Type'Class (Checking_Account)'Tag)
            then
               Report.Failed ("Incorrect Checking_Account tag");
            end if;

            if Tc_Savings_Account_Type_Tag.all /=
              Ada.Tags.External_Tag
                (Savings_Account_Type'Class (Savings_Account)'Tag)
            then
               Report.Failed ("Incorrect Savings_Account tag");
            end if;

         exception
            when others =>
               Report.Failed ("Exception raised during tag evaluation");
         end Tag_Verification_Block;

      exception
         when others =>
            Report.Failed ("Exception in Read_Data");
      end Read_Data;

   begin  -- Test_Block

      -- Enter the data into the appropriate files.
      Buffer_Data;

      -- Reconstruct the data from files, and verify the results.
      Read_Data;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others =>
      Report.Failed ("Unexpected exception");
      Report.Result;

end Cxa9002;
