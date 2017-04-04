--==================================================================--

package Ca11013_2 is

   -- To be used as actual parameters for random number generator in the parent
   -- package.

   type My_Float is digits 6 range -10.0 .. 100.0;

   function Random_Complex (Seed : My_Float) return My_Float;

end Ca11013_2;
