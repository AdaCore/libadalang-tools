package Foos is
   type Foo_1 is interface;
   function Get (F : Foo_1) return Integer is abstract;
   type Foo_2 is interface;
   function Get (F : Foo_2) return Integer is abstract;
end Foos;
