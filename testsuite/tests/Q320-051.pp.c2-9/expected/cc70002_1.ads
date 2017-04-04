     --==================================================================--

generic          -- 2D matrix abstraction.
   type Element_Type is range <>;

   type Abscissa is range <>;
   type Ordinate is range <>;

   type Matrix_2d is array (Abscissa, Ordinate) of Element_Type;
package Cc70002_1 is

   Add_Ident : constant Matrix_2d := (Abscissa => (others => 1));
   -- Artificial for
   -- testing purposes.
   -- ... Other identity matrices.

   function "+" (A, B : Matrix_2d) return Matrix_2d;

   -- ... Other operations.

end Cc70002_1;
