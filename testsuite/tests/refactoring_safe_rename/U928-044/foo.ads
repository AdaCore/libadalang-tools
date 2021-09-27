package Foo is
   generic
      type A is private;
   package Bar is
      procedure Baz (B : A);
   end Bar;
end Foo;

