--=================================================================--

package body Fa11c00_0 is    -- Package body Animal

   function Image (A : Animal) return String is
   begin
      return ("Animal Species:  " & A.Common_Name);
   end Image;

end Fa11c00_0;               -- Package body Animal
