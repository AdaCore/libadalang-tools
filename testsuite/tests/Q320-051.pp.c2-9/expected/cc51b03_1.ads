     --==================================================================--

with Fc51b00;
package Cc51b03_1 is

   subtype Spin_Type is Natural range 0 .. 3;

   type Extended_Vector
     (Spin : Spin_Type)
   is   -- Tagged type with
   new Fc51b00.Vector with
   null record;       -- discriminant (indefinite).

end Cc51b03_1;
