package body Bar is
   overriding
   function Get (F : Both_Foos) return Integer is (12);
   overriding
   function Get (F : Only_Foo_1) return Integer is (1);
   overriding
   function Get (F : Only_Foo_2) return Integer is (2);
end Bar;
