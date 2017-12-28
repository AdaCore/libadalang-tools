--==================================================================--

with Cxac007_Logger;
with Cxac007_Test;
with Ada.Streams.Stream_Io;
with Report;
procedure Cxac007 is
   Log_File_Name : constant String :=
     Report.Legal_File_Name (Nam => "CXAC007");
-- A file name that will be used for the log file. This file will be written.
begin
   Report.Test ("CXAC007", "Check that Stream_IO is preelaborated");

   Test_For_Stream_Io_Support :
   declare
      Test_File : Ada.Streams.Stream_Io.File_Type;
   begin
      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on
      -- calls to various Stream_IO operations. This block statement encloses a
      -- call to Create, which should produce an exception in a non-supportive
      -- environment. These exceptions will be handled to produce a
      -- Not_Applicable result.

      Ada.Streams.Stream_Io.Create
        (Test_File,
         Ada.Streams.Stream_Io.Out_File,
         Log_File_Name);

      Ada.Streams.Stream_Io.Close (Test_File);

   exception

      when Ada.Streams.Stream_Io.Use_Error |
        Ada.Streams.Stream_Io.Name_Error   =>
         Report.Not_Applicable
           ("Files not supported - Create as Out_File for Stream_IO");
         Report.Comment ("File_Name is " & Log_File_Name);
         goto Not_Applicable;

   end Test_For_Stream_Io_Support;

   -- Generate a few log items:
   Cxac007_Logger.Log_Item (Log_File_Name, "Start");

   Cxac007_Test.Do_Something (Log_File_Name);

   Cxac007_Logger.Log_Item (Log_File_Name, "End");

   -- Read the log file and verify correct contents:
   declare
      Log_File : Ada.Streams.Stream_Io.File_Type;

      procedure Check (Data : in String) is
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 12);
         Last   : Ada.Streams.Stream_Element_Offset;
         use type Ada.Streams.Stream_Element_Offset;
         use type Ada.Streams.Stream_Element;
      begin
         Ada.Streams.Stream_Io.Read
           (Log_File,
            Buffer (1 .. Data'Length + 2),
            Last);
         if Last /= Data'Length + 2 then
            Report.Failed
              ("Wrong length read, premature end of file for " & Data);
         end if;
         if Buffer (Data'Length + 1) /= 13 then
            Report.Failed ("Missing CR for " & Data);
         end if;
         if Buffer (Data'Length + 2) /= 10 then
            Report.Failed ("Missing LF for " & Data);
         end if;
         for I in Data'Range loop
            if Buffer
                (Ada.Streams.Stream_Element_Offset (I - Data'First) + 1) /=
              Ada.Streams.Stream_Element (Character'Pos (Data (I)))
            then
               Report.Failed ("Wrong character for " & Data);
               exit;
            end if;
         end loop;

      end Check;

   begin
      Ada.Streams.Stream_Io.Open
        (Log_File,
         Ada.Streams.Stream_Io.In_File,
         Log_File_Name);

      Check ("Start");

      Check ("Doing");

      Check ("Do more");

      Check ("End");

      Ada.Streams.Stream_Io.Delete (Log_File); -- Clean up after ourselves.
   end;

   <<Not_Applicable>>
   Report.Result;
end Cxac007;
