package body Pkg is

   function Foo (X : Integer) return Ada_Variable is
   begin
      return Ada_Variable'(Bar => X);
   end Foo;

end Pkg;
