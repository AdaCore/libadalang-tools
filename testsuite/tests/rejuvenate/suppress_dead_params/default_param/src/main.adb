with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   A : Boolean := True;
   C : Boolean;
   function Foo (D : Boolean := True) return Boolean is (D);
   -- We always call it without input;
begin
   C := Foo (True);
   if Foo then
      Put_Line ("True");
   else
      Put_Line ("False");
   end if;
end Main;
