--=================================================================--

package Fa11c00_0.Fa11c00_1.Fa11c00_2 is -- Package Animal.Mammal.Primate

   type Habitat_Type is (Arboreal, Terrestrial);

   type Primate is new Mammal with record
      Habitat : Habitat_Type;
   end record;

   function Image (P : Primate) return String;

end Fa11c00_0.Fa11c00_1.Fa11c00_2;       -- Package Animal.Mammal.Primate
