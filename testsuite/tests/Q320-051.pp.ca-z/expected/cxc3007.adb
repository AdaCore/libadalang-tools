     --==================================================================--

with Cxc3007_0;

with Ada.Interrupts;

with Impdef.Annex_C;
with Report;
procedure Cxc3007 is
begin

   Report.Test
     ("CXC3007",
      "Attach_Handler and Exchange_Handler: Check " &
      "that default treatment is restored if either is called " &
      "with value null or with result of Current_Handler when " &
      "no user-specified handler is attached");

   declare
      package Ai renames Ada.Interrupts;

      Default     : Ai.Parameterless_Handler;
      Old_Handler : Ai.Parameterless_Handler;
   begin

      if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
         Report.Failed ("User-specified handler is attached at start-up");
      else

         -- Identify and record default treatment:
         Default := Ai.Current_Handler (Impdef.Annex_C.Interrupt_To_Generate);

         -- ==============
         -- Attach_Handler:
         -- ==============

         -- Attach a user handler:

         Ai.Attach_Handler
           (Cxc3007_0.Dynamic.Handler1'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         if not Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed ("Attach_Handler: nothing attached after 1 call");
         end if;

         -- Restore default treatment (using null):

         Ai.Attach_Handler (null, Impdef.Annex_C.Interrupt_To_Generate);

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Attach_Handler: default treatment not restored " &
               "after call with null");
         end if;

         -- Attach a user handler twice:

         Ai.Attach_Handler
           (Cxc3007_0.Dynamic.Handler1'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         Ai.Attach_Handler
           (Cxc3007_0.Dynamic.Handler2'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         if not Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed ("Attach_Handler: nothing attached after 2 calls");
         end if;

         -- Restore default treatment (using value obtained at start-up):

         Ai.Attach_Handler (Default, Impdef.Annex_C.Interrupt_To_Generate);

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Attach_Handler: default treatment not restored " &
               "after call with start-up value");
         end if;

         -- Restore default treatment (using call to Current_Handler):

         Ai.Attach_Handler
           (Ai.Current_Handler (Impdef.Annex_C.Interrupt_To_Generate),
            Impdef.Annex_C.Interrupt_To_Generate);

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Attach_Handler: default treatment not restored " &
               "after call with Current_Handler");
         end if;

         -- ================
         -- Exchange_Handler:
         -- ================

         -- Default treatment is in effect at this point.

         -- Exchange for a user handler twice:

         Ai.Exchange_Handler
           (Old_Handler, Cxc3007_0.Dynamic.Handler1'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to default treatment.

         Ai.Exchange_Handler
           (Old_Handler, Cxc3007_0.Dynamic.Handler2'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to Handler1.

         if not Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed ("Exchange_Handler: nothing attached after 2 calls");
         end if;

         -- Restore default treatment (using null):

         Ai.Exchange_Handler
           (Old_Handler, null, Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to Handler2.

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Exchange_Handler: default treatment not " &
               "restored after call with null");
         end if;

         -- Default treatment is again in effect at this point.

         -- Exchange for a user handler:

         Ai.Exchange_Handler
           (Old_Handler, Cxc3007_0.Dynamic.Handler1'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to default treatment.

         if not Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed ("Exchange_Handler: nothing attached after 1 call");
         end if;

         -- Restore default treatment (using current value of Old_Handler):

         Ai.Exchange_Handler
           (Old_Handler, Old_Handler, Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to Handler1.

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Exchange_Handler: default treatment not " &
               "restored after call with value of Old_Handler");
         end if;

         -- Default treatment is again in effect at this point.

         -- Restore default treatment (using call to Current_Handler):

         Ai.Exchange_Handler
           (Old_Handler,
            Ai.Current_Handler (Impdef.Annex_C.Interrupt_To_Generate),
            Impdef.Annex_C.Interrupt_To_Generate);

         -- Old_Handler now points to default treatment.

         if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
            Report.Failed
              ("Exchange_Handler: default treatment not " &
               "restored after call with Current_Handler");
         end if;

      end if;

   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error raised");
   end;

   Report.Result;

end Cxc3007;
