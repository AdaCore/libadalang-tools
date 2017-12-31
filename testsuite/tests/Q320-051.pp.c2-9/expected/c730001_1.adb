     --==================================================================--

package body C730001_1 is

   -- Note: This body is the one that should be executed in the test block
   --       below, not the version of the body corresponding to type Clock.

   procedure Set_Display (Wc : in out Wall_Clock;
      D                      : in     Display_Kind := Analog)
   is
   begin
      Wc.Display      := D;
      Wc.Illumination := Phosphorescence;
   end Set_Display;

   procedure Answer (Op : in out Office_Phone;
      Oi                : in     Indicator_Type := Buzzer)
   is
   begin
      Op.Status    := Call_Waiting;
      Op.Indicator := Oi;
   end Answer;

end C730001_1;
