with Report;
package body C3a0013_2 is

   procedure Tc_Validate (It : Car; Tc_Id : Character) is
   begin
      if Tc_Id /= 'C' then
         Report.Failed ("Dispatched to Car");
      end if;
      if Wheels (It) /= 4 then
         Report.Failed ("Not a Car");
      end if;
   end Tc_Validate;

   function Gear_Factor (It : Car) return Natural is
   begin
      return C3a0013_1.Gear_Factor (C3a0013_1.Vehicle (It)) * 2;
   end Gear_Factor;

end C3a0013_2;
