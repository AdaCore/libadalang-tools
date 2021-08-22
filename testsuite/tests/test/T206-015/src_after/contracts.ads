package Contracts is

   function Sqrt (X : Float) return Float;

   type T is tagged null record;

   function Self (X : T) return T is (X);
end Contracts;
