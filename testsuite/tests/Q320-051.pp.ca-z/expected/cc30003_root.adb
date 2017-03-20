with Ada.Numerics.Elementary_Functions;
package body Cc30003_Root is
   function Distance (Obj : Object) return Float is
   begin
      return Ada.Numerics.Elementary_Functions.Sqrt
          (Obj.X_Coord**2 + Obj.Y_Coord**2);
   end Distance;
end Cc30003_Root;
