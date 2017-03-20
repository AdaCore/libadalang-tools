with Cc30003_Root;
package Cc30003_2 is
   type Right_Triangle is new Cc30003_Root.Object with record
      A, B : Float;    -- Lengths of sides
   end record;

   function Area (T : Right_Triangle) return Float;

   function Hypotenuse (T : Right_Triangle) return Float;

   procedure Clear (T : in out Right_Triangle);

end Cc30003_2;
