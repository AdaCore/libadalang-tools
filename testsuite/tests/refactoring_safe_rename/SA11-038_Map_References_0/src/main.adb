with Ada.Text_IO; use Ada.Text_IO;
with Foo; use Foo;
with Foo.Bar;

procedure Main is
   Quux  : Natural := 7;
   Corge : Natural := 8;
begin
   Put_Line (Baz'Image);
   Put_Line (Baz'Image);
   Put_Line (Quux'Image);
   Put_Line (Quux'Image);
   Foo.Bar;
   declare
      Baz : Natural := 9;
   begin
      Put_Line (Baz'Image);
      Put_Line (Baz'Image);
      Put_Line (Quux'Image);
      Put_Line (Quux'Image);
      Put_Line (Corge'Image);
      Put_Line (Corge'Image);
   end;
end Main;
