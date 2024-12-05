with Bar;
generic
   type Element_Type is private;
package Foo is

   type Foo_Range is range 1 .. 10;
   type Foo_Array is array (Foo_Range) of Element_Type;

   package Foo_Bar is new Bar (Foo_Array);

end Foo;
