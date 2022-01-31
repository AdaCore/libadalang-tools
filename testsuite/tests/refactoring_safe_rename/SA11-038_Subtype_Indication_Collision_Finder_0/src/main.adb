with Ada.Text_IO; use Ada.Text_IO;
with A; use A;

procedure Main is
   B : Bar := Bar1;
   procedure Another_Foo (My_Bar : A.Bar);
   procedure Another_Foo (My_Bar : A.Bar) is
   begin
      Ada.Text_IO.Put_Line (My_Bar'Image);
   end Another_Foo;
begin
   Foo (B);
   Another_Foo (B);
end Main;
