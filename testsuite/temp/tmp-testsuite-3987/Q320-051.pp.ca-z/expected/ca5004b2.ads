----------------------------------------------------------

with Header; use Header;
with Ca5004b0, Ca5004b1;
use Ca5004b0, Ca5004b1;
pragma Elaborate (Header, Ca5004b0, Ca5004b1);
package Ca5004b2 is

   K1 : Integer := Ca5004b0.I;
   K2 : Integer := Ca5004b1.J;

   procedure Require_Body;

end Ca5004b2;
