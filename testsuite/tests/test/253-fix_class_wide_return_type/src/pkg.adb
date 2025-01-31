package body Pkg is
   function Create_Foo (Bar : Integer) return Foo'Class is
   begin
      return Foo'(Baz => Bar);
   end Create_Foo;
end Pkg;
