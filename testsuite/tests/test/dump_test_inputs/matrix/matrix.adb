with Number.Operations; use Number.Operations;

package body Matrix is

   function Add_Matrix (L, R : Matrix_Type) return Matrix_Type
   is
      Res : Matrix_Type;
   begin
      for I in 1 .. 4 loop
         for J in 1 .. 4 loop
            Res (I, J) := Add (L (I, J), R (I, J));
         end loop;
      end loop;

      return Res;

   end Add_Matrix;

   function Multiply_Matrix (L, R : Matrix_Type) return Matrix_Type
   is
      Res : Matrix_Type;
   begin
      for I in 1 .. 4 loop
         for J in 1 .. 4 loop
            Res (I, J) := 0;
            for K in 1 .. 4 loop
               Res (I, J) := Add (Res (I, J),  Multiply (L (I, K), R (K, J)));
            end loop;
         end loop;
      end loop;

      return Res;

   end Multiply_Matrix;

end Matrix;
