--==================================================================--

package body Ca11022_0.Ca11022_3 is

   procedure Ibm3270_Graphic is
      procedure Ibm3270_Putdot (X : in Column; Y : in Row) is
      begin
         -- Light a pixel at location (X + 2, Y);
         Tc_Screen (Y, X + Column (2)) := True;
      end Ibm3270_Putdot;

      ------------------------------------

      -- Declare instance of the generic sibling package to draw a circle,
      -- a square, or an ellipse customized for the IBM3270.
      package Ibm3270_Graphic is new Ca11022_0.Ca11022_1 (Ibm3270_Putdot);

   begin
      Ibm3270_Graphic.Draw_Circle (X, Y, R);
      Ibm3270_Graphic.Draw_Square (X, Y, L);
   end Ibm3270_Graphic;

end Ca11022_0.Ca11022_3;
