with Ada.Directories;             use Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces;                  use Interfaces;
with My_File;                     use My_File;
with My_File.TGen_Support;        use My_File.TGen_Support;
with TGen.JSON;
with TGen.TGen_Support;           use TGen.TGen_Support;
with TGen.Marshalling_Lib;

procedure Example_Gen is

   Arr : My_Arr (1 .. 10, Mon .. Mon) :=
     (for I in 1 .. 10 => (Mon => 1));

   Arr_2 : My_Arr (1 .. 1, Mon .. Sun) :=
     (1 => (for Day in Weekday => 1));

   Filename : constant String := "scratchpad.bin";
   File : Ada.Streams.Stream_IO.File_Type;
   S : Stream_Access;
begin
   Create (File, Out_File, Filename);
   S := Stream (File);
   TGen_Marshalling_My_File_My_Arr_Output (S, Arr);
   Close (File);
   Open (File, In_File, Filename);
   S := Stream (File);
   begin
      declare
         Arr_In : My_Arr := TGen_Marshalling_My_File_My_Arr_Input (S);
      begin
         Put_Line ("did not raise an exception when reading array");
      end;
   exception
      when TGen.Marshalling_Lib.Invalid_Value => null;
   end;
   Close (File);
   Open (File, Out_File, Filename);
   S := Stream (File);
   TGen_Marshalling_My_File_My_Arr_Output (S, Arr_2);
   Close (File);
   Open (File, In_File, Filename);
   S := Stream (File);
   begin
      declare
         Arr_2_In : My_Arr := TGen_Marshalling_My_File_My_Arr_Input (S);
      begin
         Put_Line ("did not raise an exception when reading array");
      end;
   exception
      when TGen.Marshalling_Lib.Invalid_Value => null;
   end;
   Close (File);
end;
