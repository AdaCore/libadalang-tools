-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body Cc40001_0.Cc40001_2 is

   procedure Tc_Verify_State is
   begin
      if Tc_Check_Object.Tc_Current_State /= Adjusted then
         Report.Failed ("CC40001_2 : Formal Object not adjusted");
      end if;
   end Tc_Verify_State;

end Cc40001_0.Cc40001_2;
