------------------------------------------------------------------

with Ada.Synchronous_Task_Control;
with Ada.Interrupts;
with System;
with Report;
with Impdef;
with Impdef.Annex_C;
with Cxda004_1;
procedure Cxda004 is
   Verbose       : constant Boolean := Cxda004_1.Verbose;
   In_Suspension : Boolean          := False;
   pragma Volatile (In_Suspension);
   package Stc renames Ada.Synchronous_Task_Control;
   Check_So : Stc.Suspension_Object;

   procedure Check_For_Ref (S : in out Stc.Suspension_Object) is
   -- this procedure is called with Check_SO as a parameter
   begin
      Stc.Set_True (S);
      if not Stc.Current_State (Check_So) then
         Report.Failed ("Suspension_Object is not a by-reference type (1)");
      end if;
      Stc.Set_False (S);
      if Stc.Current_State (Check_So) then
         Report.Failed ("Suspension_Object is not a by-reference type (2)");
      end if;
   end Check_For_Ref;

begin

   Report.Test
     ("CXDA004",
      "Check that a suspension_object can be changed" &
      " by an interrupt handler");

   declare -- encapsulate the test
      task Does_Suspension is
         entry Do_Pe_Test;
         entry Do_Int_Test;
      end Does_Suspension;

      task body Does_Suspension is
      begin
         accept Do_Pe_Test;
         Stc.Suspend_Until_True (Check_So);

         accept Do_Int_Test;
         -- time to wait
         In_Suspension := True;
         Stc.Suspend_Until_True (Cxda004_1.So);
         In_Suspension := False;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Does_Suspension");
      end Does_Suspension;

   begin
      -- the test for suspension objects being passed by reference
      Check_For_Ref (Check_So);

      -- check for program_error if a suspend_until_true is done on a
      -- suspension object that already has a task waiting on it.
      Stc.Set_False (Check_So);
      Does_Suspension.Do_Pe_Test;
      delay Impdef.Switch_To_New_Task;   -- let Does_Suspension suspend
      begin
         Stc.Suspend_Until_True (Check_So);
         Report.Failed ("no exception on double suspend");
      exception
         when Program_Error =>
            if Verbose then
               Report.Comment ("exception properly raised");
            end if;
         when others =>
            Report.Failed ("wrong exception on double suspend");
      end;
      Stc.Set_True (Check_So);  -- let Does_Suspension continue

      -- interrupt test
      Stc.Set_False (Cxda004_1.So);
      Does_Suspension.Do_Int_Test;
      delay Impdef.Switch_To_New_Task;  -- to allow Does_Suspension to block
      Impdef.Annex_C.Enable_Interrupts;
      Impdef.Annex_C.Generate_Interrupt;

      delay Impdef.Annex_C.Wait_For_Interrupt;

      if In_Suspension then
         Report.Failed ("Task was not unblocked");
         abort Does_Suspension;  -- so test will terminate
      end if;

      if Cxda004_1.Interrupt_Count = 0 then
         Report.Failed ("interrupt never occurred");
      else
         if Verbose then
            Report.Comment
              ("interrupt count is" &
               Integer'Image (Cxda004_1.Interrupt_Count));
         end if;
      end if;

   exception
      when others =>
         Report.Failed ("Unexpected Exception in main procedure");
   end;  -- encapsulation

   Report.Result;

end Cxda004;
