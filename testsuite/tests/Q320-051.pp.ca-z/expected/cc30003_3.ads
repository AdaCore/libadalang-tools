with Cc30003_0;
with Cc30003_1;
package Cc30003_3 is

   package Counted_Rectangle_Inst is new Cc30003_0 (Cc30003_1.Rectangle);

   type Counted_Square is new Counted_Rectangle_Inst.Counted_Type with
   null record;

   procedure Make_Square (S : in out Counted_Square; Side : in Float);

   function Area (S : Counted_Square) return Float;

   procedure Bump (S : in out Counted_Square);

end Cc30003_3;
