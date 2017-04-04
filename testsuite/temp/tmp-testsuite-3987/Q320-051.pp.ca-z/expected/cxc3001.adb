     --==================================================================--

with Ada.Interrupts;
with Cxc3001_0;

with Impdef.Annex_C;
with Report;

procedure Cxc3001 is

   package Ai renames Ada.Interrupts;

   type Interrupt_Array is array (Ai.Interrupt_Id) of Boolean;

   Reserved_Interrupt : Interrupt_Array := (others => False);
   Verbose            : Boolean         := True;

                 --===--===--===--===--===--===--===--===--===--

   procedure Map_Reserved_Interrupts (Isreserved : in out Interrupt_Array) is
   begin
      for Interrupt in Isreserved'Range loop
         if Ai.Is_Reserved (Interrupt) then
            Isreserved (Interrupt) := True;
         else
            Isreserved (Interrupt) := False;
         end if;
      end loop;
   end Map_Reserved_Interrupts;

                 --===--===--===--===--===--===--===--===--===--

   procedure Check_Attachments
     (Isreserved     : in Interrupt_Array;
      Expected_Value : in Boolean)
   is
      Fail_Count : Natural := 0;
   begin
      for Interrupt in Isreserved'Range loop
         if not Isreserved (Interrupt)
           and then Ai.Is_Attached (Interrupt) /= Expected_Value
         then
            Fail_Count := Fail_Count + 1;
         end if;
      end loop;

      if Fail_Count /= 0 then
         Report.Failed
           ("Wrong result from Is_Attached for" &
            Natural'Image (Fail_Count) &
            " of" &
            Natural'Image (Isreserved'Length) &
            " interrupt IDs");
      end if;
   end Check_Attachments;

                 --===--===--===--===--===--===--===--===--===--

   procedure Attach_Handlers (Isreserved : in Interrupt_Array) is
   begin
      for Interrupt in Isreserved'Range loop
         if not Isreserved (Interrupt) then
            Ai.Attach_Handler (Cxc3001_0.Good.Handler'Access, Interrupt);
         end if;
      end loop;
   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error in Attach_Handlers");
      when others =>
         Report.Failed ("Unexpected exception in Attach_Handlers");
   end Attach_Handlers;

                 --===--===--===--===--===--===--===--===--===--

   procedure Detach_Handlers (Isreserved : in Interrupt_Array) is
   begin
      for Interrupt in Isreserved'Range loop
         if not Isreserved (Interrupt) then
            Ai.Detach_Handler (Interrupt);
         end if;
      end loop;
   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error in Detach_Handlers");
      when others =>
         Report.Failed ("Unexpected exception in Detach_Handlers");
   end Detach_Handlers;

                 --===--===--===--===--===--===--===--===--===--

   procedure Test_Attach_Handler is
   begin
      if Cxc3001_0.Good.Count /= 0 then
         Report.Failed ("Bad handle count before Attach_Handler");
      else
         Ai.Attach_Handler
           (Cxc3001_0.Good.Handler'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         Impdef.Annex_C.Generate_Interrupt;
         delay Impdef.Annex_C.Wait_For_Interrupt;

         if Cxc3001_0.Good.Count /= 1 then
            Report.Failed ("Handler not attached by Attach_Handler");
         else
            begin
               Ai.Attach_Handler
                 (Cxc3001_0.Bad.Handler'Access,
                  Impdef.Annex_C.Interrupt_To_Generate);
               Report.Failed ("Program_Error not raised by Attach_Handler");
            exception
               when Program_Error => -- Expected result.
                  null;
               when others =>
                  Report.Failed ("Unexpected exception from Attach_Handler");
            end;

            -- Don't generate interrupt if no user-handler attached at
            -- this point (could cause unpredictable results):
            if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
               Impdef.Annex_C.Generate_Interrupt;
               delay Impdef.Annex_C.Wait_For_Interrupt;

               if Cxc3001_0.Good.Count /= 2 then
                  Report.Failed ("Treatment not retained by Attach_Handler");
               end if;
            else
               Report.Failed ("Attach_Handler: no handler attached");
            end if;

            Cxc3001_0.Good.Reset;  -- Reset CXC3001_0.Good.Count to 0.

         end if;

      end if;
   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error in Test_Attach_Handler");
      when others =>
         Report.Failed ("Unexpected exception in Test_Attach_Handler");
   end Test_Attach_Handler;

                 --===--===--===--===--===--===--===--===--===--

   procedure Test_Exchange_Handler is
      Old : Ai.Parameterless_Handler;
   begin
      if Cxc3001_0.Good.Count /= 0 then
         Report.Failed ("Bad handle count before Exchange_Handler");
      else
         Ai.Exchange_Handler
           (Old,
            Cxc3001_0.Good.Handler'Access,
            Impdef.Annex_C.Interrupt_To_Generate);

         Impdef.Annex_C.Generate_Interrupt;
         delay Impdef.Annex_C.Wait_For_Interrupt;

         if Cxc3001_0.Good.Count /= 1 then
            Report.Failed ("Handler not attached by Exchange_Handler");
         else
            begin
               Ai.Exchange_Handler
                 (Old,
                  Cxc3001_0.Bad.Handler'Access,
                  Impdef.Annex_C.Interrupt_To_Generate);
               Report.Failed ("Program_Error not raised by Exchange_Handler");
            exception
               when Program_Error => -- Expected result.
                  null;
               when others =>
                  Report.Failed ("Unexpected exception from Exchange_Handler");
            end;

            -- Don't generate interrupt if no user-handler attached at
            -- this point (could cause unpredictable results):
            if Ai.Is_Attached (Impdef.Annex_C.Interrupt_To_Generate) then
               Impdef.Annex_C.Generate_Interrupt;
               delay Impdef.Annex_C.Wait_For_Interrupt;

               if Cxc3001_0.Good.Count /= 2 then
                  Report.Failed ("Treatment not retained by Exchange_Handler");
               end if;
            else
               Report.Failed ("Exchange_Handler: no handler attached");
            end if;

         end if;

      end if;
   exception
      when Program_Error =>
         Report.Failed ("Unexpected Program_Error in Test_Exchange_Handler");
      when others =>
         Report.Failed ("Unexpected exception in Test_Exchange_Handler");
   end Test_Exchange_Handler;

                 --===--===--===--===--===--===--===--===--===--

begin

   Report.Test
     ("CXC3001",
      "Check operation of Is_Attached. Check that " &
      "Program_Error is raised if the handler used by " &
      "Attach_Handler or Exchange_Handler does not " &
      "have an associated pragma Interrupt_Handler");

-- Enable interrupts, if necessary:
   Impdef.Annex_C.Enable_Interrupts;

-- Identify all reserved interrupts:
   Map_Reserved_Interrupts (Reserved_Interrupt);

-- Verify that Is_Attached is False for all non-reserved interrupts:

   if Verbose then
      Report.Comment ("Before attachments..");
   end if;

   Check_Attachments
     (Isreserved     => Reserved_Interrupt,
      Expected_Value => False);

-- Attach a user-defined handler to each non-reserved interrupt:
   Attach_Handlers (Reserved_Interrupt);

-- Verify that Is_Attached is True for all non-reserved interrupts:

   if Verbose then
      Report.Comment ("After attachments..");
   end if;

   Check_Attachments
     (Isreserved     => Reserved_Interrupt,
      Expected_Value => True);

-- Detach user-defined handlers from each non-reserved interrupt:
   Detach_Handlers (Reserved_Interrupt);

-- Verify that Is_Attached is False for all non-reserved interrupts:

   if Verbose then
      Report.Comment ("After detachments..");
   end if;

   Check_Attachments
     (Isreserved     => Reserved_Interrupt,
      Expected_Value => False);

-- Verify that P_E is raised if Attach_Handler uses an invalid handler:
   Test_Attach_Handler;

-- Verify that P_E is raised if Exchange_Handler uses an invalid handler:
   Test_Exchange_Handler;

   Report.Result;

end Cxc3001;
