with F393a00_0;
package body F393a00_3 is
   function Create return Pump is
      Sump : Pump;
   begin
      F393a00_0.Tc_Touch ('h');
      return Sump;
   end Create;

   procedure Set_Rate (A_Pump : in out Pump; To_Rate : Gallons_Per_Revolution)
   is
   begin
      F393a00_0.Tc_Touch ('i');
      A_Pump.Gprpm := To_Rate;
   end Set_Rate;

   function Rate (Of_Pump : Pump) return Gallons_Per_Revolution is
   begin
      F393a00_0.Tc_Touch ('j');
      return Of_Pump.Gprpm;
   end Rate;
end F393a00_3;
