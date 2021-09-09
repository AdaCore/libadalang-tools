package body Foo is
   overriding procedure Bar (Self : B; Baz : access procedure (Q : Boolean))
   is
      This_Q : Boolean := False;
   begin
      Baz (Q => This_Q);
   end Bar;
end Foo;
