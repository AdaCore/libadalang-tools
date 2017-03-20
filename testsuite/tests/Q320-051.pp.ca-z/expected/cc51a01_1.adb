     --==================================================================--

package body Cc51a01_1 is

   -- This body should never be called.
   --
   -- The test sends the function Numerator a fraction with a negative
   -- numerator, and expects this negative numerator to be returned. This
   -- version of the function returns the absolute value of the numerator.
   -- Thus, a call to this version is detectable by examining the sign
   -- of the return value.

   function Numerator (Frac : Pos_Fraction) return Integer is
      Converted_Frac : Fc51a00.Fraction_Type := Fc51a00.Fraction_Type (Frac);
      Orig_Numerator : Integer := Fc51a00.Numerator (Converted_Frac);
   begin
      return abs (Orig_Numerator);
   end Numerator;

end Cc51a01_1;
