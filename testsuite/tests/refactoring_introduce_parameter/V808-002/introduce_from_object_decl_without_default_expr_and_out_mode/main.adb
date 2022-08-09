with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Local_Func;
   procedure Local_Func is
      A : Integer;
   begin
      A := 1;
      Put_Line (A'Img);
   end Local_Func;
begin
   null;
end Main;
