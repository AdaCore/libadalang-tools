package Pkg is
   type Variable is tagged null record;
   type Ada_Variable is private;

   function Foo (X : Integer) return Ada_Variable;

private
   type Ada_Variable is new Variable with record
      Bar : Integer;
   end record;
end Pkg;
