     --==================================================================--

package body Cc70002_2 is

   function Add_Matrices_With_Coefficients (A : Matrix_Ops.Matrix_2d;
      Ca : Integer; B : Matrix_Ops.Matrix_2d;
      Cb : Integer) return Matrix_Ops.Matrix_2d
   is
      Left, Right : Matrix_Ops.Matrix_2d;
   begin
      Left  := Math_Sig.Power (A, Ca);      -- Multiply 1st array by its coeff.
      Right := Math_Sig.Power (B, Cb);      -- Multiply 2nd array by its coeff.
      return (Matrix_Ops."+" (Left, Right));-- Add these two arrays.
   end Add_Matrices_With_Coefficients;

end Cc70002_2;
