package Pkg is
   type Foo is tagged private;

private
   type Foo is tagged record
      Baz : Integer;
   end record;

   function Create_Foo (Bar : Integer) return Foo'Class;
end Pkg;
