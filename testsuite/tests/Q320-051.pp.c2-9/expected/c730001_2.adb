     --==================================================================--

package body C730001_2 is

   procedure Answer
     (Cp : in out Conference_Room_Phone; Ci : in Indicator_Type := Modem)
   is
   begin
      Cp.Status    := Conference;
      Cp.Indicator := Ci;
   end Answer;

   function Tc_Get_Display (C : Alarm_Clock) return Display_Kind is
   begin
      return C.Display;
   end Tc_Get_Display;

   function Tc_Get_Display_Illumination
     (C : Alarm_Clock) return Illumination_Type
   is
   begin
      return C.Illumination;
   end Tc_Get_Display_Illumination;

end C730001_2;
