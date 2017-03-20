--=======================================================================--

package body Fa11d00 is  -- Complex_Definition_Pkg
   function Complex (Real, Imag : Int_Type) return Complex_Type is
   begin
      return (Real, Imag);
   end Complex;

end Fa11d00;     -- Complex_Definition_Pkg
