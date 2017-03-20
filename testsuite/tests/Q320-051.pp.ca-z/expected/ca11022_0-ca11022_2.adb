--==================================================================--

package body Ca11022_0.Ca11022_2 is

   procedure Vt100_Graphic is
      procedure Vt100_Putdot (X : in Column; Y : in Row) is
      begin
         -- Light a pixel at location (X, Y);
         Tc_Screen (Y, X) := True;
      end Vt100_Putdot;

      ------------------------------------

      -- Declare instance of the generic sibling package to draw a circle,
      -- a square, or an ellipse customized for the VT100.
      package Vt100_Graphic is new Ca11022_0.Ca11022_1 (Vt100_Putdot);

   begin
      Vt100_Graphic.Draw_Circle (X, Y, R);
      Vt100_Graphic.Draw_Square (X, Y, L);
   end Vt100_Graphic;

end Ca11022_0.Ca11022_2;
