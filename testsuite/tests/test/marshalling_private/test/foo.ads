package Foo is
   type Bar is private;
   subtype Baz is Bar;
   procedure Qux (F : Bar; B : Baz);
private
   type Bar is null record;
end Foo;