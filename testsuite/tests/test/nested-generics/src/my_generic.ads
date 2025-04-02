generic
package My_Generic is
   package Nested is
         function F (X : Integer) return Integer is (X);
   end Nested;

   function G (X : Integer) return Integer is (Nested.F (X));
end My_Generic;
