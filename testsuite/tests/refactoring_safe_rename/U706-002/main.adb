with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type Cargo is (Bar, Baz, Qux);
begin
   Ada.Text_IO.Put_Line (Bar'Image);
end Main;
