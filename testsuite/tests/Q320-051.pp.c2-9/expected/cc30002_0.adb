     --==================================================================--

package body Cc30002_0 is

   procedure Focus (C : in out Camera) is
   begin
      -- Artificial for testing purposes.
      C.Tc_Focus_Called := Body_Of_Ancestor;
   end Focus;

end Cc30002_0;
