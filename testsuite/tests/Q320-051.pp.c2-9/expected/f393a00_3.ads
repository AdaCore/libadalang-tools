----------------------------------------------------------------------

with F393a00_2;
package F393a00_3 is
   type Pump is new F393a00_2.Windmill with private;
   function Create return Pump;

   type Gallons_Per_Revolution is digits 3;
   procedure Set_Rate (A_Pump : in out Pump; To_Rate : Gallons_Per_Revolution);
   function Rate (Of_Pump : Pump) return Gallons_Per_Revolution;
private
   type Pump is new F393a00_2.Windmill with record
      Gprpm : Gallons_Per_Revolution := 0.0; -- Gallons/RPM
   end record;
end F393a00_3;
