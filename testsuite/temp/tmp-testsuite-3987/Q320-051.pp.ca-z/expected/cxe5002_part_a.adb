-----------------------------------------------------------------------------

with Report;
package body Cxe5002_Part_A is
   -- This procedure should not be directly called because it has a pragma
   -- All_Calls_Remote applied to it.  Since the System.RPC body does not
   -- forward any calls, this procedure would only be called if the pragma
   -- were to be ignored.
   procedure Local_Remote_Call is
   begin
      Report.Failed ("pragma All_Calls_Remote was ignored.");
   end Local_Remote_Call;
end Cxe5002_Part_A;
