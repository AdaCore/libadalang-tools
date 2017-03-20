     --=================================================================--

with Fb40a00.Cb40a020_0.Cb40a020_1;   -- "with" of  private child subprogram
-- Text_Parser.Processing.Process_Text
package body Fb40a00.Cb40a020_0 is

   function Count_Alphanumerics (Text : in String) return Natural is
   begin
      Fb40a00.Cb40a020_0.Cb40a020_1 (Text);  -- Call prvt child proc.
      return (Alphanumeric_Count);           -- Global maintained in parent.
      -- No exception handler here, exception propagates.
   end Count_Alphanumerics;

end Fb40a00.Cb40a020_0;
