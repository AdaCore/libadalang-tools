package Q is
   function Inc (X : Integer) return Integer is (X + 1);

   package Nesting_1 is
      function F1 (X : Integer) return Integer is (X + 3);
   end Nesting_1;

   package Nesting_2 is
      package Nesting_2_1 is
         function F5 (X : Integer) return Integer is (X * 7);
      end Nesting_2_1;
   end Nesting_2;
end Q;
