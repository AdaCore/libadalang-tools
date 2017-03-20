--=======================================================================--

package body F650a00.P is

   procedure Handle (Pa : in out Practice_Alert) is
   begin
      Pa.Status  := Real;
      Pa.Urgency := Medium;
   end Handle;

   function Make_Alert_For_Time (Time : in Duration) return Practice_Alert is
   begin
      return (Time => Time, Status => <>, Urgency => <>);
   end Make_Alert_For_Time;

end F650a00.P;
