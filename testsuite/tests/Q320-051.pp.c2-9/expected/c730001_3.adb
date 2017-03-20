--==================================================================--

package body C730001_3 is

   function Tc_Get_Phone_Status (P : Phone'Class) return Capability_Type is
   begin
      return P.Status;
   end Tc_Get_Phone_Status;

   function Tc_Get_Indicator (P : Phone'Class) return Indicator_Type is
   begin
      return P.Indicator;
   end Tc_Get_Indicator;

end C730001_3;
