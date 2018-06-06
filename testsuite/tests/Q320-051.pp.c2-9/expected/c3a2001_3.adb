----------------------------------------------------------------------------

with Tctouch;
package body C3a2001_3 is

   function Construct
     (Voltage : C3a2001_2.Voltages; Amperage : C3a2001_2.Amps)
      return Ground_Fault
   is
   begin
      Tctouch.Touch ('g'); --------------------------------------------- g
      return (C3a2001_2.Construct (Voltage, Amperage) with Capacitance => 0);
   end Construct;

   procedure Set_Trip
     (The_Breaker : in out Ground_Fault; Capacitance : in Integer)
   is
   begin
      Tctouch.Touch ('h'); --------------------------------------------- h
      The_Breaker.Capacitance := Capacitance;
   end Set_Trip;

end C3a2001_3;
