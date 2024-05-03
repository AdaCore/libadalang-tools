with Interfaces;
with System.Arith_128;
with System.Image_F;

package System_Under_Test is
   subtype Int128 is Interfaces.Integer_128;
   subtype Uns128 is Interfaces.Unsigned_128;

   package Impl is new System.Image_F (Int128, Uns128, System.Arith_128.Scaled_Divide128);
end System_Under_Test; 
