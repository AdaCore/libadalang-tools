with Bob;
generic
   type Foo_Type is private;
package Foo is
   procedure Swap (A, B : in out Foo_Type);
   procedure Sneaky (A : Integer);
   package Bob_Foo is new Bob (Foo_Type);
end Foo;
