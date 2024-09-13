package Foo is

   subtype Foo is Integer range 1 .. 100;

   type Test is record
      X, Y : Foo;
   end record;

   function "=" (L, R : Test) return Boolean is (False);

end Foo;
