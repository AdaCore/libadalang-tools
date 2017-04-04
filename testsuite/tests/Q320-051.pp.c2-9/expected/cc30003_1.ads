with Cc30003_Root;
package Cc30003_1 is
   type Rectangle is new Cc30003_Root.Object with record
      Height, Width : Float;
   end record;

   function Area (R : Rectangle) return Float;

end Cc30003_1;
