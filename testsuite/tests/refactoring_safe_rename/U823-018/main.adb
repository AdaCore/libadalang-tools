with Ada.Text_IO;
with Gen;
procedure Main is
   package Gen_Int is new Gen (1);
begin
   Ada.Text_IO.Put_Line (Gen_Int.Const'Image);
end Main;
