package body Main_Package is

   procedure Foo is null;

   procedure Foo (Bar : in out Integer) is null;

   procedure Foo
     (Bar : in out Integer;
      Baz : in out Integer;
      Qux : in out Integer) is null;

end Main_Package;
