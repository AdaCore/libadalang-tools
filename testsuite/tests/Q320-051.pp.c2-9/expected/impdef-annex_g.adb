     --==================================================================--

package body Impdef.Annex_G is

   -- NOTE: These are example bodies. It is expected that implementors
   --       will write their own versions of these routines.

--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--

   --  This function must return a negative zero value for implementations for
   --  which Float'Signed_Zeros is True.

   --  We generate the smallest normalized negative number, and divide by a few
   --  powers of two to obtain a number whose absolute value equals zero but
   --  whose sign is negative.

   function Negative_Zero return Float is
      Negz : Float :=
        -1.0 *
        Float
            (Float'Machine_Radix)**
          (Float'Machine_Emin - Float'Machine_Mantissa);
   begin
      return Negz / 8.0;
   end Negative_Zero;

--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--

end Impdef.Annex_G;
