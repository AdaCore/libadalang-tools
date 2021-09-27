with Foo; use Foo;

procedure Main is
   package My_Bar is new Bar (Integer);
begin
   My_Bar.Baz (1);
end Main;

