-- Alert_Foundation.Public_Child;

--=======================================================================--

package body F393b00.C393b13_0 is
   -- Alert_Foundation.Public_Child

   procedure Handle (Ca : in out Child_Alert) is
   begin
      Ca.Msg (1 .. Message'Length) := Message;
      Ca.Times_Handled             := Ca.Times_Handled + 1;
   end Handle;

end F393b00.C393b13_0;
