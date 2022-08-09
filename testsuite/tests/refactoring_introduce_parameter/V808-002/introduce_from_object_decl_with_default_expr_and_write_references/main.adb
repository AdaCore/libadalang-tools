with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Local_Func;
   procedure Local_Func is
      A : Integer := 2;
   begin
      A := 3;
      Put_Line (A'Img);
   end Local_Func;
begin
   null;
end Main;
