     --==================================================================--

with Cxc3008_0;

with Ada.Interrupts;

with Impdef.Annex_C;
with Report;
procedure Cxc3008 is
begin

   Report.Test
     ("CXC3008",
      "Check Current_Handler, Attach_Handler, and " &
      "Exchange_Handler when user-specified handlers are " &
      "attached. Check Detach_Handler");

   Impdef.Annex_C.Enable_Interrupts;    -- Enable interrupts, if necessary.

   declare
      package Ai renames Ada.Interrupts;

      Old_Handler : Ai.Parameterless_Handler;
   begin

      -- Attach a user handler:

      Ai.Attach_Handler
        (Cxc3008_0.Dynamic1.Handler'Access,
         Impdef.Annex_C.Interrupt_To_Generate);

      -- Attach a different user handler:

      Ai.Attach_Handler
        (Cxc3008_0.Dynamic2.Handler'Access,
         Impdef.Annex_C.Interrupt_To_Generate);

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

      if Cxc3008_0.Dynamic1.Handle_Count /= 0 or
        Cxc3008_0.Dynamic2.Handle_Count /= 1 then
         Report.Failed ("Wrong handler counts after 1st interrupt");
      end if;

      Report.Comment ("Done with 1st interrupt");

      -- Exchange handlers:

      Ai.Exchange_Handler
        (Old_Handler, Cxc3008_0.Dynamic1.Handler'Access,
         Impdef.Annex_C.Interrupt_To_Generate);

      -- Old_Handler now designates Dynamic2.Handler.

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

      if Cxc3008_0.Dynamic1.Handle_Count /= 1 or
        Cxc3008_0.Dynamic2.Handle_Count /= 1 then
         Report.Failed ("Wrong handler counts after 2nd interrupt");
      end if;

      Report.Comment ("Done with 2nd interrupt");

      -- Current handler is Dynamic1.Handler.

      -- Attach a handler with pragma Attach_Handler (via object creation):

      declare
         Static : Cxc3008_0.Handler_Type;
      begin

         Impdef.Annex_C.Generate_Interrupt;
         delay Impdef.Annex_C.Wait_For_Interrupt;

         if Static.Handle_Count /= 1 or Cxc3008_0.Dynamic1.Handle_Count /= 1 or
           Cxc3008_0.Dynamic2.Handle_Count /= 1 then
            Report.Failed ("Wrong handler counts after 3rd interrupt");
         end if;

         Report.Comment ("Done with 3rd interrupt");

      end;

      -- Protected object Static has been finalized, and previous handler
      -- (Dynamic1.Handler) should be restored.

      -- Exchange handlers:

      Ai.Exchange_Handler
        (Old_Handler,
         Old_Handler,  -- Designates Dynamic2.Handler.
         Impdef.Annex_C.Interrupt_To_Generate);

      -- Old_Handler now designates Dynamic1.Handler.

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

      if Cxc3008_0.Dynamic1.Handle_Count /= 1 or
        Cxc3008_0.Dynamic2.Handle_Count /= 2 then
         Report.Failed ("Wrong handler counts after 4th interrupt");
      end if;

      Report.Comment ("Done with 4th interrupt");

      -- Attach a different handler:

      Ai.Attach_Handler
        (Old_Handler, -- Designates Dynamic1.Handler.
         Impdef.Annex_C.Interrupt_To_Generate);

      -- Attach yet a different user handler:

      -- In practice, calling Current_Handler within Attach_Handler for
      -- the same interrupt will probably never occur, but calling it for
      -- a different interrupt will. That's what is simulated below:

      Ai.Attach_Handler
        (Ai.Current_Handler (Impdef.Annex_C.Interrupt_To_Generate),
         Impdef.Annex_C.Interrupt_To_Generate);

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

      if Cxc3008_0.Dynamic1.Handle_Count /= 2 or
        Cxc3008_0.Dynamic2.Handle_Count /= 2 then
         Report.Failed ("Wrong handler counts after 5th interrupt");
      end if;

      Report.Comment ("Done with 5th interrupt");

      -- Exchange handlers:

      -- In practice, calling Current_Handler within Exchange_Handler for
      -- the same interrupt will probably never occur, but calling it for
      -- a different interrupt will. That's what is simulated below:

      Ai.Exchange_Handler
        (Old_Handler,
         Ai.Current_Handler (Impdef.Annex_C.Interrupt_To_Generate),
         Impdef.Annex_C.Interrupt_To_Generate);

      -- Old_Handler now designates Dynamic1.Handler.

      Impdef.Annex_C.Generate_Interrupt;
      delay Impdef.Annex_C.Wait_For_Interrupt;

      if Cxc3008_0.Dynamic1.Handle_Count /= 3 or
        Cxc3008_0.Dynamic2.Handle_Count /= 2 then
         Report.Failed ("Wrong handler counts after 6th interrupt");
      end if;

      Report.Comment ("Done with 6th interrupt");

      -- Test Detach_Handler:

      if not Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
         Report.Failed ("No user-specified handler is attached");
      else

         -- Detach handler:

         Ai.Detach_Handler (Impdef.Annex_C.Interrupt_To_Generate);

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed ("Default treatment not restored by Detach_Handler");
         end if;

      end if;

   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error raised");
   end;

   Report.Result;

end Cxc3008;
