--==================================================================--

with Ca11022_0.Ca11022_1;                -- Generic sibling.

-- Child package to provide customized graphic functions for the IBM3270.
package Ca11022_0.Ca11022_3 is           -- IBM3270 Graphic.

   X : Column := 39;
   Y : Row    := 11;
   R : Radius := 3;
   L : Length := 7;

   procedure Ibm3270_Graphic;

end Ca11022_0.Ca11022_3;
