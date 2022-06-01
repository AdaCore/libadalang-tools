with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   declare
      Foo, Another_Foo : constant Integer := 1;
   begin
      Put_Line (Another_Foo'Image);
      declare
         Bar : Integer := Foo;
      begin
         Put_Line (Bar'Image);
         declare
            Baz : Integer;
         begin
            Baz := Foo;
            Ada.Text_IO.Put_Line (Foo'Image);
         end;
      exception
         when others => null;
      end;
      null;
   end;

end Main;
