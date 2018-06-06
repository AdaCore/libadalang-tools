     --==================================================================--

with Cc70002_0;  -- Mathematical group signature.
with Cc70002_1;  -- 2D matrix abstraction.

generic          -- Mathematical 2D matrix addition group.

   with package Matrix_Ops is new Cc70002_1 (<>);

   -- Although the restriction of the formal package below to signatures
   -- describing addition groups, and then only for 2D matrices, is rather
   -- artificial in the context of this "application," the passing of types,
   -- objects, and subprograms as actuals to a formal package is not.

   with package Math_Sig is new Cc70002_0 (Group_Type => Matrix_Ops.Matrix_2d,
      Identity => Matrix_Ops.Add_Ident, Operation => Matrix_Ops."+");

package Cc70002_2 is

   -- Add two matrices that are to be multiplied by coefficients: [ ] = CA*[ ]
   -- + CB*[ ].

   function Add_Matrices_With_Coefficients
     (A  : Matrix_Ops.Matrix_2d; Ca : Integer; B : Matrix_Ops.Matrix_2d;
      Cb : Integer) return Matrix_Ops.Matrix_2d;

   -- ...Other operations.

end Cc70002_2;
