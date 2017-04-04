     --==================================================================--

package body C460a01_0 is
   function Convert (P : Operand_Type) return Target_Type is
   begin
      return Target_Type (P); -- Never fails.
   end Convert;
end C460a01_0;
