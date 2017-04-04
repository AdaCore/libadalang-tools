with System;
with Report;
with Cxd3003_1; use Cxd3003_1;
procedure Cxd3003 is
   Priority_Top : constant System.Priority           := System.Priority'Last;
   Priority_Int : constant System.Interrupt_Priority :=
     System.Interrupt_Priority'First;
begin

   Report.Test
     ("CXD3003",
      "Ceiling_Locking: default priority ceiling" &
      " for an interrupt handler protected object");

   declare -- encapsulate the test

      task Task_Of_Top_Priority is
         pragma Priority (Priority_Top);
      end Task_Of_Top_Priority;

      task Task_Of_Int_Priority is
         pragma Interrupt_Priority (Priority_Int);
      end Task_Of_Int_Priority;

      -- These tasks call a protected object whose ceiling should be higher
      -- than the task's priority
      --

      task body Task_Of_Top_Priority is
         Numb : Task_Number := 1;
      begin
         Protected_Object.For_Ceiling_Check (Numb);  -- Should be O.K.
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Task_of_Top_Priority");
      end Task_Of_Top_Priority;

      task body Task_Of_Int_Priority is
         Numb : Task_Number := 2;
      begin
         Protected_Object.For_Ceiling_Check (Numb);  -- Should be O.K.
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Task_of_Int_Priority");
      end Task_Of_Int_Priority;

   begin
      null;
   end;  -- encapsulation

   -- Now check that the calls were actually made and not optimized away
   --
   if not Protected_Object.Verify_Calls then
      Report.Failed ("For_Ceiling_Check not called correctly");
   end if;

   Report.Result;

end Cxd3003;
