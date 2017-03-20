--=======================================================================--

package body F393b00 is

   procedure Handle (Pa : in out Practice_Alert) is
   begin
      Pa.Status  := Real;
      Pa.Urgency := Medium;
   end Handle;

end F393b00;
