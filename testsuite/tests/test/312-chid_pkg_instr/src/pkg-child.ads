package Pkg.Child is

   function Foo (X : T) return T is (X);

   package Inner_1 is

      function Bar (X : T) return T is (X);

   end Inner_1;

   package Inner_2 is

      function Baz (X : T) return T is (X);

   end Inner_2;

   function Qux (X : T) return T is (X);

end Pkg.Child;
