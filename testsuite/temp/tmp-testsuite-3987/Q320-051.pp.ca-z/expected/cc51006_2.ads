     --==================================================================--

with Cc51006_0;       -- Weight class.
use Cc51006_0;
package Cc51006_2 is  -- Extensions to weight class.

   type Grams is new Weight_Type;                         -- Unconstrained
   -- derivative.

   function Weight_To_String (Wt : Grams) return String;  -- Overrides root
   -- type's operation.

   subtype Milligrams is Grams                            -- Constrained
   range 0.0 .. 0.999;                                  -- subtype (of der.).

   type Pounds is new Weight_Type                         -- Constrained
   range 0.0 .. 500.0;                                  -- derivative.

   function Weight_To_String (Wt : Pounds) return String; -- Overrides root
   -- type's operation.

end Cc51006_2;
