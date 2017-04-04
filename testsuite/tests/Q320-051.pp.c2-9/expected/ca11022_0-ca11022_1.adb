--==================================================================--

package body Ca11022_0.Ca11022_1 is

   procedure Draw_Square
     (At_Col : in Column;
      At_Row : in Row;
      Len    : in Length)
   is
   begin
      -- use square drawing algorithm call
      Put_Dot (At_Col + Column (Len), At_Row + Row (Len));
      -- as needed in the algorithm.
      Tc_Draw_Square := True;
   end Draw_Square;

   -------------------------------------------------------
   procedure Draw_Circle
     (At_Col : in Column;
      At_Row : in Row;
      Rad    : in Radius)
   is
   begin
      -- use circle drawing algorithm call
      for I in 1 .. Rad loop
         Put_Dot (At_Col + Column (I), At_Row + Row (I));
      end loop;
      -- as needed in the algorithm.
      Tc_Draw_Circle := True;
   end Draw_Circle;

end Ca11022_0.Ca11022_1;
