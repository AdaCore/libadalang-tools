     --==================================================================--

package body Cc51a01_0 is

   function Put (Item : in Fraction) return String is
      Num : constant String :=              -- Fraction's primitive subprograms
      Integer'Image (Numerator (Item));   -- are inherited from its parent
      Den : constant String :=              -- (FC51A00.Fraction_Type) and NOT
      Integer'Image (Denominator (Item)); -- from the actual type.
   begin
      return (Num & '/' & Den);
   end Put;

end Cc51a01_0;
