-----------------------------------------------------------------------------

with Report;
package body Cxe5002_Part_B is
   -- the procedures in this package are never actually called.  If a call
   -- actually occurs then probably the wrong body of System.RPC was used.

   procedure Remote_Normal (X : Integer) is
   begin
      Report.Failed ("Remote_Normal called - check version of System.RPC");
   end Remote_Normal;

   procedure Remote_Async (X : Integer) is
   begin
      Report.Failed ("Remote_Async called - check version of System.RPC");
   end Remote_Async;

end Cxe5002_Part_B;
