package Foo is
   package Bar is
      type T is private;
      function Ident_Nested (X : T) return T;
   private
      type T is new Integer;
   end Bar;
   function Ident (X : T) return T is (Ident_Nested (X));
end Foo;
