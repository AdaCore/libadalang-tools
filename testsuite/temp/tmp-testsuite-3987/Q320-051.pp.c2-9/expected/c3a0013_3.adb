with Report;
package body C3a0013_3 is

   procedure Tc_Validate (It : Truck; Tc_Id : Character) is
   begin
      if Tc_Id /= 'T' then
         Report.Failed ("Dispatched to Truck");
      end if;
      if Wheels (It) /= 3 then
         Report.Failed ("Not a Truck");
      end if;
   end Tc_Validate;

   function Gear_Factor (It : Truck) return Natural is
   begin
      return C3a0013_1.Gear_Factor (C3a0013_1.Vehicle (It)) * 3;
   end Gear_Factor;

end C3a0013_3;
