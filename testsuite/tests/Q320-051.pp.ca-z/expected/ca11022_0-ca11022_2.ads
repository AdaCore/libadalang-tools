--==================================================================--

with Ca11022_0.Ca11022_1;                -- Generic sibling.

-- Child package to provide customized graphic functions for the
-- VT100.
package Ca11022_0.Ca11022_2 is           -- VT100 Graphic.

   X : Column := 8;
   Y : Row    := 3;
   R : Radius := 2;
   L : Length := 6;

   procedure Vt100_Graphic;

end Ca11022_0.Ca11022_2;
