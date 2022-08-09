with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Local_Func;
   procedure Local_Func is
      A : Integer;
   begin
      Put_Line (A'Img);
      A := 1;
   end Local_Func;
begin
   null;
end Main;
