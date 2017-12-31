-----------------------------------------------------------------------

with Report; use Report;
pragma Elaborate (Report);
package body Ca5006a0 is
   Raised : Boolean := False;

   function P_E_Raised return Boolean is
   begin
      return Raised;
   end P_E_Raised;

   procedure Show_Pe_Raised is
   begin
      Raised := True;
   end Show_Pe_Raised;

begin
   Test
     ("CA5006A",
      "CHECK THAT A PROGRAM IS NOT REJECTED JUST " &
      "BECAUSE THERE IS NO WAY TO ELABORATE " &
      "SECONDARY UNITS SO PROGRAM_ERROR WILL BE " & "AVOIDED");

end Ca5006a0;
