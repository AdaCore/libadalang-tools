-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body C760009_0 is -- Check_1

   procedure Tc_Check_1 (Apf : in Private_Formal) is
      Local : Private_Formal;
   begin
      Local := Apf;
      Tc_Validate (Local);
   end Tc_Check_1;

   procedure Tc_Check_2 (Apf : out Private_Formal) is
      Local : Private_Formal;  -- initialized by virtue of actual being
      -- Controlled
   begin
      Apf := Local;
      Tc_Validate (Apf);
   end Tc_Check_2;

   procedure Tc_Check_3 (Apf : in out Private_Formal) is
      Local : Private_Formal;
   begin
      Local := Apf;
      Tc_Validate (Local);
   end Tc_Check_3;

end C760009_0;
