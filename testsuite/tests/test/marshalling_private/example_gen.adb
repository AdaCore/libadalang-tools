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
   Bar_1, Bar_2 : Bar;
   Baz_1, Baz_2 : Baz;


begin
   Create (F, Out_File, File_Name);
   S := Stream (F);

   TGen_Marshalling_foo_bar_output (S, Bar_1);
   TGen_Marshalling_foo_baz_output (S, Baz_2);

   Close (F);

   Open (F, In_File, File_Name);

   Bar_2 := TGen_Marshalling_foo_bar_input(S);
   Baz_2 := TGen_Marshalling_foo_baz_input(S);

   Close (F);

   if Bar_1 /= Bar_2 then
      Put_Line ("Error comparing Bar");
   elsif Baz_1 /= Baz_2 then
      Put_Line ("Error comparing Baz");
   end if;

end Example_Gen;
