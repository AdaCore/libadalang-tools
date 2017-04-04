     --=================================================================--

with Fb40a00.Cb40a030_1;  -- private sibling package Text_Parser.Processing;

package body Fb40a00.Cb40a030_0 is

   function Count_Alphanumerics (Text : in String) return Natural is
   begin
      Fb40a00.Cb40a030_1.Process_Text (Text);     -- Call proc in prvt child
      -- package that is a
      -- sibling of this package.
      return (Alphanumeric_Count);
      -- No exception handler here, exception propagates.
   end Count_Alphanumerics;

end Fb40a00.Cb40a030_0;
