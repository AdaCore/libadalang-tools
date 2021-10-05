with Foo; use Foo;
with Foo.Baz;
with Foo.Qux; use Foo.Qux;
with Foo.Qux.Garply;
procedure Main is
begin
   Foo.Bar;
   Foo.Baz;
   Foo.Qux.Corge;
   Foo.Qux.Garply;
end Main;

