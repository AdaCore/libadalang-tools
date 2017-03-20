with Tctouch;
package body C393001_1 is
   procedure Fail (The_Breaker : in out Breaker) is ------------------- a
   begin
      Tctouch.Touch ('a');
      The_Breaker.State := Failed;
   end Fail;

   procedure Set (The_Breaker : in out Breaker'Class; To_State : Status) is
   begin
      The_Breaker.State := To_State;
   end Set;

   function Status_Of (The_Breaker : Breaker) return Status is ------- b
   begin
      Tctouch.Touch ('b');
      return The_Breaker.State;
   end Status_Of;
end C393001_1;
