package body Foo is
   package body Bar is
      function Ident_Nested (X : T) return T is (X);
   end Bar;
end Foo;
