with Foo;
package Qux is
   function Qux_Add (I : Integer) return Integer is (2 + I);
   type Grault_Type is
      record
         G : Integer;
      end record;
   package Corge is new Foo (Integer);
   package Waldo is
      package Fred is new Foo (Natural);
      package Grault is new Foo (Grault_Type);
   end Waldo;
private
   package Garply is new Foo (Float);
end Qux;
