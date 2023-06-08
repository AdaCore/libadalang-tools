with Ada.Streams;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Text_IO;                 use Ada.Text_IO;
with Interfaces;                  use Interfaces;
with Foo;                         use Foo;
with Foo.TGen_Support;            use Foo.TGen_Support;

procedure Example_Gen is

   F         : Ada.Streams.Stream_IO.File_Type;
   S         : Stream_Access;
   File_Name : constant String := "scratch_pad.bin";
   Rec_1, Rec_2 : Larger_Record_Type;


begin
   Create (F, Out_File, File_Name);
   S := Stream (F);

   Rec_1 :=
     (A => 1,
      B => 1.0,
      C => True,
      D => 'R',
      E => "123456789012345678901234567");

   TGen_Marshalling_foo_larger_record_type_output (S, Rec_1);

   Close (F);

   Open (F, In_File, File_Name);
   S := Stream (F);

   Rec_2 := TGen_Marshalling_foo_larger_record_type_input(S);

   Close (F);

   if Rec_1 /= Rec_2 then
      Put_Line ("Error comparing Larger_Record_Type");
   end if;

end Example_Gen;
