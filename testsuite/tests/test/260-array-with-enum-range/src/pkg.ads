package Pkg is
   type Foo_Index is (Foo, Bar);

   type Foo_Array is array (Foo_Index'Range) of Integer;

   function Is_Two (Arr : Foo_Array) return Boolean is (Arr (Foo) = 2);
end Pkg;
