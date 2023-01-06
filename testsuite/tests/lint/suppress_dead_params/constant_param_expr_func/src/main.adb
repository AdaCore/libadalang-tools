with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   A : Boolean := True;
   C : Boolean;
   function Foo (B : Boolean) return Boolean is (B);
   -- We always call it without input;
begin
   C := Foo (False);
   if Foo(False) then
      Put_Line ("True");
   else
      Put_Line ("False");
   end if;
end Main;
