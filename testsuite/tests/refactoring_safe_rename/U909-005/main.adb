with Ada.Text_IO; use Ada.Text_IO;
with Foo; use Foo;
procedure Main is
   procedure Qux (R : Boolean);
   procedure Qux (R : Boolean) is
   begin
      Put_Line (R'Image);
   end Qux;
   My_B : B;
begin
   My_B.Bar (Qux'Access);
end Main;
