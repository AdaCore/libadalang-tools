with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Add_One (N : in out Integer; B : Boolean := True) is
   begin
      N := N + 1;
   end Add_One;
   F : Integer := 1;
begin
   Add_One (F, True);
   Add_One (F);
end Main;
