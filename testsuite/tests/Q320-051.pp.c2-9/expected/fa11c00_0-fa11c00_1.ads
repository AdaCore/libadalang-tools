--=================================================================--

package Fa11c00_0.Fa11c00_1 is         -- Package Animal.Mammal

   type Hair_Color_Type is (Black, Brown, Blonde, Grey, White, Red);

   type Mammal is new Animal with record
      Hair_Color : Hair_Color_Type;
   end record;

   function Image (M : Mammal) return String;

end Fa11c00_0.Fa11c00_1;               -- Package Animal.Mammal
