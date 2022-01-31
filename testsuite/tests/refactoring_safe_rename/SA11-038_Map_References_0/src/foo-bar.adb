with Ada.Text_IO; use Ada.Text_IO;

procedure Foo.Bar is
   Qux : Natural := 6;
begin
   Put_Line (Baz'Image);
   Put_Line (Baz'Image); Put_Line (Baz'Image);
   Put_Line (Qux'Image);
   Put_Line (Qux'Image); Put_Line (Qux'Image);
end Foo.Bar;
