with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Foo (A : Integer; B : Integer);
   procedure Foo (A : Integer; B : Integer) is

      procedure Extracted;

      procedure Extracted is
      begin
         Put_Line (A'Image);
         Put_Line (B'Image);
      end Extracted;

   begin
      Extracted;
   end Foo;
begin
   null;
end Main;
