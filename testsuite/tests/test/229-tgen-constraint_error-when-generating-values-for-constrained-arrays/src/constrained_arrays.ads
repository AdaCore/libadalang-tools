with Interfaces; use Interfaces;

package Constrained_Arrays is
   subtype Index_512 is Unsigned_32 range 0 .. 511;
   type Unsigned_32_Array_512 is array (Index_512) of Unsigned_32;
   function Func (X : Unsigned_32_Array_512) return Integer is (X'Length);
end Constrained_Arrays;
