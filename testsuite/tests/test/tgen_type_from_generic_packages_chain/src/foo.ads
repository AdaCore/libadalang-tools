with Bar;
generic
   type Element_Type is private;
package Foo is

   package Foo_Bar is new Bar (Element_Type);

end Foo;
