procedure Main is
   procedure Foo is
      procedure Bar is null;

   begin
      Bar;
   end Foo;
begin
   null;
end Main;
