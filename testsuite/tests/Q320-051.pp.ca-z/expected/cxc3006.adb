     --==================================================================--

with Cxc3006_0;
with Cxc3006_1;

with Ada.Interrupts;

with Impdef.Annex_C;
with Report;
procedure Cxc3006 is
begin

   Report.Test
     ("CXC3006",
      "Check that Program_Error is raised if " &
      "Attach_Handler, Detach_Handler, or Exchange_Handler " &
      "is used to attempt to detach a handler that was " &
      "attached with a pragma Attach_Handler");

   Impdef.Annex_C.Enable_Interrupts;     -- Enable interrupts, if necessary.

-- ==============
-- Attach_Handler:
-- ==============

   begin
      Ada.Interrupts.Attach_Handler
        (Cxc3006_0.Dynamic.Handler'Access,
         Impdef.Annex_C.Interrupt_To_Generate);
      Report.Failed ("Attach_Handler: Program_Error was not raised");
   exception
      when Program_Error => -- Expected result.
         null;
      when others =>
         Report.Failed ("Attach_Handler: Unexpected exception raised");
   end;

   Impdef.Annex_C.Generate_Interrupt;
   delay Impdef.Annex_C.Wait_For_Interrupt;

   if Cxc3006_0.Dynamic.Handled then
      Report.Failed ("1st interrupt: Dynamic.Handler was called");
   end if;

   if not Cxc3006_1.Static.Handled then
      Report.Failed ("1st interrupt: Static.Handler not called");
   end if;

   Cxc3006_0.Dynamic.Reset;
   Cxc3006_1.Static.Reset;
   Report.Comment ("Done with 1st interrupt");

-- ==============
-- Detach_Handler:
-- ==============

   begin
      Ada.Interrupts.Detach_Handler (Impdef.Annex_C.Interrupt_To_Generate);
      Report.Failed ("Detach_Handler: Program_Error was not raised");
   exception
      when Program_Error => -- Expected result.
         null;
      when others =>
         Report.Failed ("Detach_Handler: Unexpected exception raised");
   end;

   Impdef.Annex_C.Generate_Interrupt;
   delay Impdef.Annex_C.Wait_For_Interrupt;

   if Cxc3006_0.Dynamic.Handled then
      Report.Failed ("2nd interrupt: Dynamic.Handler was called");
   end if;

   if not Cxc3006_1.Static.Handled then
      Report.Failed ("2nd interrupt: Static.Handler not called");
   end if;

   Cxc3006_0.Dynamic.Reset;
   Cxc3006_1.Static.Reset;
   Report.Comment ("Done with 2nd interrupt");

-- ================
-- Exchange_Handler:
-- ================

   declare
      Old_Handler : Ada.Interrupts.Parameterless_Handler;
   begin
      Ada.Interrupts.Exchange_Handler
        (Old_Handler, Cxc3006_0.Dynamic.Handler'Access,
         Impdef.Annex_C.Interrupt_To_Generate);
      Report.Failed ("Exchange_Handler: Program_Error was not raised");
   exception
      when Program_Error => -- Expected result.
         null;
      when others =>
         Report.Failed ("Exchange_Handler: Unexpected exception raised");
   end;

   Impdef.Annex_C.Generate_Interrupt;
   delay Impdef.Annex_C.Wait_For_Interrupt;

   if Cxc3006_0.Dynamic.Handled then
      Report.Failed ("3rd interrupt: Dynamic.Handler was called");
   end if;

   if not Cxc3006_1.Static.Handled then
      Report.Failed ("3rd interrupt: Static.Handler not called");
   end if;

   Report.Comment ("Done with 3rd interrupt");

   Report.Result;

end Cxc3006;
