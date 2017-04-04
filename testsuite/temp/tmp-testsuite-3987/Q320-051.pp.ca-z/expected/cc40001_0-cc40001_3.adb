-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body Cc40001_0.Cc40001_3 is

   procedure Tc_Verify_State is
   begin
      if Bad_Status (Tc_Check_Object) then
         Report.Failed ("CC40001_3 : Formal Object not adjusted");
      end if;
   end Tc_Verify_State;

end Cc40001_0.Cc40001_3;
