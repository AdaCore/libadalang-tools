----------------------------------------------------------------------------
with Report;
with Tctouch;
with C3a2001_1, C3a2001_2, C3a2001_3, C3a2001_4;

pragma Elaborate_All
  (Report,
   Tctouch,
   C3a2001_1,
   C3a2001_2,
   C3a2001_3,
   C3a2001_4);

package body C3a2001_5 is

   function Neww (Breaker : in C3a2001_1.Breaker'Class) return Component is
   begin
      return new C3a2001_1.Breaker'Class'(Breaker);
   end Neww;

   procedure Add (Gadget : in Component; To_Circuit : in out Connection) is
   begin
      To_Circuit := new Circuit'(Gadget, To_Circuit);
   end Add;

   procedure Flipper (The_Circuit : Connection) is
      Probe : Connection := The_Circuit;
   begin
      while Probe /= null loop
         C3a2001_1.Flip (Probe.The_Gadget.all);
         Probe := Probe.Next;
      end loop;
   end Flipper;

   procedure Tripper (The_Circuit : Connection) is
      Probe : Connection := The_Circuit;
   begin
      while Probe /= null loop
         C3a2001_1.Trip (Probe.The_Gadget.all);
         Probe := Probe.Next;
      end loop;
   end Tripper;

   procedure Restore (The_Circuit : Connection) is
      Probe : Connection := The_Circuit;
   begin
      while Probe /= null loop
         C3a2001_1.Reset (Probe.The_Gadget.all);
         Probe := Probe.Next;
      end loop;
   end Restore;

   procedure Failure (The_Circuit : Connection) is
      Probe : Connection := The_Circuit;
   begin
      while Probe /= null loop
         C3a2001_1.Fail (Probe.The_Gadget.all);
         Probe := Probe.Next;
      end loop;
   end Failure;

begin
   Add (Neww (C3a2001_2.Construct (C3a2001_2.V440, C3a2001_2.A5)), Short);
   Add (Neww (C3a2001_3.Construct (C3a2001_2.V110, C3a2001_2.A1)), Short);
   Add (Neww (C3a2001_4.Construct (C3a2001_2.V12, C3a2001_2.A100)), Short);
end C3a2001_5;
