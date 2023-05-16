with Ada.Numerics.Elementary_Functions;
package body Contracts is

   function Sqrt (X : Float) return Float
   is
   begin
      if X < 0.0 then
         return -1.0;
      else
         return Ada.Numerics.Elementary_Functions.Sqrt (X);
      end if;
   end Sqrt;

end Contracts;
