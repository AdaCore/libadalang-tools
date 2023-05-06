with Number; use Number;

package body Matrix_Data is

   procedure Reset_Matrixes is
   begin
      Matrix_1 :=
        ((1, 0, 0, -1),
         (0, 1, 0, 0),
         (0, 0, 1, 0),
         (-1, 0, 0, 1));

      Matrix_1 :=
        ((1, 2, 0, 4),
         (-2, 2, 1, 1),
         (7, 3, 0, -5),
         (3, -4, 5, 2));
   end Reset_Matrixes;

   procedure Perform_Calculations (L, R : Matrix_Type) is
   begin
      Matrix_Result := Multiply_Matrix (L, Add_Matrix (L, R));
   end Perform_Calculations;

end Matrix_Data;
