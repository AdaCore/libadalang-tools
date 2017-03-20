--==================================================================--

package body Ca11021_0.Ca11021_1 is

   function "+" (Left, Right : Complex_Type) return Complex_Type is

   begin
      return ((Left.Real + Right.Real, Left.Imag + Right.Imag));
   end "+";

   --------------------------------------------------

   function "-" (Right : Complex_Type) return Complex_Type is
   begin
      return (-Right.Real, -Right.Imag);
   end "-";

end Ca11021_0.Ca11021_1;
