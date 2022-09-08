with Ada.Text_IO;
procedure Main is
begin
   declare
      S, R : constant Integer := 1;
      T, V : constant Integer := 1;
      X, Y : constant Integer := 1;
      W, Z : constant Integer := 1;

      function Bar return Integer is (Z + R);

      procedure Foo
        (A : Integer;
         B : Integer);

      procedure Foo
        (A : Integer;
         B : Integer) is
      begin
         Ada.Text_IO.Put_Line (R'Image);
         Ada.Text_IO.Put_Line (V'Image);
         Ada.Text_IO.Put_Line (X'Image);
         Ada.Text_IO.Put_Line (Y'Image);
         Ada.Text_IO.Put_Line (W'Image);
         Ada.Text_IO.Put_Line (Bar'Image);
      end Foo;
   begin
      Foo (Y, Y);
   end;
end Main;
