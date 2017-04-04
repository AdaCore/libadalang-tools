--=================================================================--

package body Fa11c00_0.Fa11c00_1 is    -- Package body Animal.Mammal

   function Image (M : Mammal) return String is
   begin
      return ("Mammal Species:  " & M.Common_Name);
   end Image;

end Fa11c00_0.Fa11c00_1;               -- Package body Animal.Mammal
