     --==================================================================--

with Cxc3003_0;
with Cxc3003_1;

with Ada.Interrupts;

with Impdef.Annex_C;
with Report;
procedure Cxc3003 is
   package Ai renames Ada.Interrupts;
begin -- CXC3003.

   Report.Test
     ("CXC3003",
      "Check that when a protected object is " &
      "finalized, for any of its procedures that are " &
      "attached to interrupts, the handler is detached. " &
      "Check that if the handler was attached by a pragma " &
      "Attach_Handler, the previous handler is restored");

   Impdef.Annex_C.Enable_Interrupts;  -- Enable interrupts, if necessary.

-- (1) Attach Dynamic_Handler.Handle_Interrupt to the interrupt
--     identified by Interrupt_To_Generate:

   Ai.Attach_Handler
     (Cxc3003_0.Dynamic_Handler.Handle_Interrupt'Access,
      Impdef.Annex_C.Interrupt_To_Generate);

   declare

-- (2) Create a protected object of type Handler_Type, constraining it
--     with Interrupt_To_Generate. The pragma Attach_Handler within the
--     protected object (Static_Handler) should now attach
--     Static_Handler.Handle_Interrupt to Interrupt_To_Generate, overriding
--     the previous handler:

      Static_Handler : Cxc3003_1.Handler_Type
        (Impdef.Annex_C.Interrupt_To_Generate);

   begin

-- (3) Verify that Static_Handler.Handle_Interrupt has not yet been called:

      if Static_Handler.Handled then
         Report.Failed ("Interrupt handled prematurely by Static_Handler");
      else

-- (4) Generate the interrupt:

         Impdef.Annex_C.Generate_Interrupt;
         delay Impdef.Annex_C.Wait_For_Interrupt;

-- (5) Verify that Static_Handler.Handle_Interrupt was called:

         if not Static_Handler.Handled then
            Report.Failed ("Interrupt not handled by Static_Handler");
         end if;
      end if;

-- (6) Leave block, causing Static_Handler to be finalized. Since
--     Static_Handler.Handle_Interrupt was attached to Interrupt_To_Generate
--     by a pragma Attach_Handler, the previous handler (i.e.,
--     Dynamic_Handler.Handle_Interrupt) should be restored, rather than
--     the default treatment.

   end;

   Report.Comment ("Done with 1st interrupt");

-- (7) Verify that Dynamic_Handler.Handle_Interrupt has not yet been called:

   if Cxc3003_0.Dynamic_Handler.Handled then
      Report.Failed ("Interrupt handled prematurely by Dynamic_Handler");
   else

-- (8) Generate the interrupt:

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

-- (9) Verify that Dynamic_Handler.Handle_Interrupt was called:

      if not Cxc3003_0.Dynamic_Handler.Handled then
         Report.Failed ("Interrupt not handled by Dynamic_Handler");
      end if;
   end if;

   Report.Comment ("Done with 2nd interrupt");

   Report.Result;

end Cxc3003;
