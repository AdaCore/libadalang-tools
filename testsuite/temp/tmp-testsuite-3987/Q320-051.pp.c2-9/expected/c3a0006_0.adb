-----------------------------------------------------------------------------

package body C3a0006_0 is

   function Sine (Angle : in Float) return Float is
   begin
      Tc_Sine_Call := Tc_Sine_Call + 1;
      Sine_Value   := Sine_Value + Angle;
      return Sine_Value;
   end Sine;

   function Cos (Angle : in Float) return Float is
   begin
      Tc_Cos_Call := Tc_Cos_Call + 1;
      Cos_Value   := Cos_Value - Angle;
      return Cos_Value;
   end Cos;

   function Tan (Angle : in Float) return Float is
   begin
      Tc_Tan_Call := Tc_Tan_Call + 1;
      Tan_Value   := (Tan_Value + (Tan_Value * Angle));
      return Tan_Value;
   end Tan;

end C3a0006_0;
