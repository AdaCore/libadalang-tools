     --===================================================================--

with Cc51007_1;
package Cc51007_2 is

   type Person is (Ood, Co, Cinc);

   type Medium_Alert is new Cc51007_1.Low_Alert with record
      Action_Officer : Person := Ood;
   end record;

   procedure Handle (A : in out Medium_Alert);        -- Overrides parent's
   -- implementation.
   Med : Medium_Alert;

end Cc51007_2;
