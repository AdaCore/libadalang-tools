with Foos; use Foos;
package Bar is
   type Both_Foos is new Foo_1 and Foo_2 with null record;
   overriding
   function Get (F : Both_Foos) return Integer;
   type Only_Foo_1 is new Foo_1 with null record;
   overriding
   function Get (F : Only_Foo_1) return Integer;
   type Only_Foo_2 is new Foo_2 with null record;
   overriding
   function Get (F : Only_Foo_2) return Integer;
end Bar;
