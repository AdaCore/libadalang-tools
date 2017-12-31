with Interfaces.C;
package body Cxb30171 is

   procedure Ada_Doubler (Inout1 : in out Interfaces.C.Int;
      Inout2 : in out Interfaces.C.Short; Inout3 : in out Interfaces.C.C_Float;
      Inout4                     : in out Interfaces.C.Double)
   is
      -- Double the value of each of the parameters.
      use type Interfaces.C.Int;
      use type Interfaces.C.Short;
      use type Interfaces.C.C_Float;
      use type Interfaces.C.Double;
   begin
      Inout1 := Inout1 * 2;
      Inout2 := Inout2 * 2;
      Inout3 := Inout3 * 2.0;
      Inout4 := Inout4 * 2.0;
   end Ada_Doubler;

end Cxb30171;
