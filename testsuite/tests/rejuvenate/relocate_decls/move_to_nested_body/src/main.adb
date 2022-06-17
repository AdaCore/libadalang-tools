procedure Main is
   package Foo is
      A: Integer := 0;
      package Foo_Baz is
         procedure Add_One;
      end Foo_Baz;
   private

   end Foo;

   package body Foo is
      package body Foo_Baz is
         procedure Add_One is
         begin
            A := A + 1;
         end Add_One;
      end Foo_Baz;
   end Foo;

begin
   null;
end Main;
