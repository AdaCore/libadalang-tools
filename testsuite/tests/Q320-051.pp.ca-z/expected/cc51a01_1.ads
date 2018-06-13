     --==================================================================--

with Fc51a00;         -- Fraction type abstraction.
package Cc51a01_1 is

   -- Derive directly from the root type of the class and override one of the
   -- primitive subprograms.

   type Pos_Fraction is new Fc51a00.Fraction_Type;     -- Derived directly from
   -- root type of class.
   -- Inherits "/" from root type. Inherits "-" from root type. Inherits
   -- Numerator from root type. Inherits Denominator from root type.

   -- Return absolute value of numerator as integer.
   function Numerator
     (Frac : Pos_Fraction)            -- Overrides parent's
      return Integer;                                   -- operation.

end Cc51a01_1;