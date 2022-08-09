with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   procedure Local_Func (Foo, Introduced_Parameter, Bar : Integer; Introduced_Parameter_1 : Float);
   procedure Local_Func (Foo, Introduced_Parameter, Bar : Integer; Introduced_Parameter_1 : Float) is
      A : constant Integer := 1;
   begin
      Put_Line (A'Image);
   end Local_Func;
begin
   null;
end Main;
