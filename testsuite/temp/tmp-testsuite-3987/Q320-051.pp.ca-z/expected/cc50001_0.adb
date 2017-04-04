     --===================================================================--

package body Cc50001_0 is

   function "=" (Left, Right : Count_Type) return Boolean is
   begin
      return False;   -- Return FALSE even if Left = Right.
   end "=";

   function "=" (Left, Right : Person_Type) return Boolean is
   begin
      return False;   -- Return FALSE even if Left = Right.
   end "=";

end Cc50001_0;
