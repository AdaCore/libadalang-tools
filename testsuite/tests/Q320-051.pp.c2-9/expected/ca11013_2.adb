--==================================================================--

package body Ca11013_2 is

   -- Not a real random number generator.
   function Random_Complex (Seed : My_Float) return My_Float is
   begin
      return (Seed + 3.0);
   end Random_Complex;

end Ca11013_2;
