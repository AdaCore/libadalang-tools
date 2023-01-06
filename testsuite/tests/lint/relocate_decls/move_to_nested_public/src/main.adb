procedure Main is
   package Foo is
      type Bar is null record;
      package Foo_Baz is
         type Fred is new Foo.Bar;
      private
      end Foo_Baz;
      A : Foo_Baz.Fred;
   end Foo;

begin
   --  Insert code here.
   null;
end Main;
