procedure Main is
   procedure Foo
     (Bar : not null access procedure (Baz  : Boolean)) is null;
begin
   null;
end Main;
