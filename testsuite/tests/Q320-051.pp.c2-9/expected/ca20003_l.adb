with Ca20003_Tc;
package body Ca20003_L is

   procedure Display (Dbg : in Debug_Info) is
   begin
      Ca20003_Tc.Tc_Debug_Subsystem := Ca20003_Tc.Bad_Version;
   end Display;

begin
   Ca20003_Tc.Tc_Debug_Subsystem := Ca20003_Tc.First_Version;
end Ca20003_L;
