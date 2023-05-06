with Number; use Number;

package Matrix is

   type Matrix_Type is
     array (1 .. 4, 1 .. 4) of Number_Type;

   function Add_Matrix (L, R : Matrix_Type) return Matrix_Type;

   function Multiply_Matrix (L, R : Matrix_Type) return Matrix_Type;

end Matrix;
