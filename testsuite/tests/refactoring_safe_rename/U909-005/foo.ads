package Foo is
   type A is interface;
   procedure Bar (Self : A; Baz : access procedure (P : Boolean)) is abstract;
   type B is new A with null record;
   overriding procedure Bar (Self : B; Baz : access procedure (Q : Boolean));
end Foo;
