with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   declare
      Foo : constant Integer := 1;
   begin
      Put_Line ("Some actions.");
      declare
         Bar : Integer;
      begin
         Put_Line (Foo'Image);
      exception
         when others => null;
      end;
      null;
   end;

end Main;
