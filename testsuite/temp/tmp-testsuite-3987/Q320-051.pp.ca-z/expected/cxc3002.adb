     --==================================================================--

with Cxc3002_0;
with Cxc3002_1;

with Ada.Interrupts;

with Report;
procedure Cxc3002 is

begin -- CXC3002.

   Report.Test
     ("CXC3002",
      "Check that Program_Error is raised if the " &
      "interrupt corresponding to that specified by the " &
      "expression in pragma Attach_Handler is reserved");

   if Cxc3002_1.Reserved_Interrupt_Found then

      begin
         declare
            Int_Handler : Cxc3002_0.Handler_Type (Cxc3002_1.Id);
         begin
            Report.Failed ("Program_Error was not raised");
         end;
      exception
         when Program_Error => -- Expected result.
            null;
         when others =>
            Report.Failed ("Unexpected exception raised");
      end;

   else
      Report.Not_Applicable ("No reserved interrupts found");
   end if;

   Report.Result;

end Cxc3002;
