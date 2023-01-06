procedure Main is
   package Foo is
      A, B: Integer := 0;
      procedure Add_One_To_B;
      package Foo_Baz is
      private
         C : Integer := A + 1;
      end Foo_Baz;
   end Foo;

   package body Foo is
      procedure Add_One_To_B is
      begin
         B := B +1;
      end Add_One_To_B;
   end Foo;

begin
   Foo.B := 3;
   Foo.Add_One_To_B;
end Main;
