package Pkg is
   type Bool_Int is range 0 .. 1;
   --  Reverse mapping is only available with Ada 2022
   function To_Bool (X : Bool_Int) return Boolean is (Boolean'Val (X));
end Pkg;
