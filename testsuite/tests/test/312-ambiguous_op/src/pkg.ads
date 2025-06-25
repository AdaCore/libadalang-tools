package Pkg is

   function "+" (X : Float; Y : Integer) return Integer is (Y);

   function "+" (Y : Integer; X : Float) return Integer is (Y);

   --  Gnattest used to generate ambiguous calls for these operators as it used
   --  a named parameter association:

   --  Pkg."+" (X => 3.14, Y => 42);

   --  The only way to disambiguate this is to use the actual operator notation
   --  3.14 + 42 or 42 + 3.14.

end Pkg;
