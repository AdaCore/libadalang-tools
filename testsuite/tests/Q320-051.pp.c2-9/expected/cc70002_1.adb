     --==================================================================--

package body Cc70002_1 is

   function "+" (A, B : Matrix_2d) return Matrix_2d is
      C : Matrix_2d;
   begin
      for I in Abscissa loop
         for J in Ordinate loop
            C (I, J) := A (I, J) + B (I, J);
         end loop;
      end loop;
      return C;
   end "+";

end Cc70002_1;
