procedure Main is
   package Foo is
      A, B, C : Integer := 0;
      D :Integer := C + 1;
   private
      E :Integer := A + 1;
   end Foo;

   package body Foo is
      procedure Add_B is
      begin
         B := B + 1;
      end Add_B;
   end Foo;

begin
   null;
end Main;
