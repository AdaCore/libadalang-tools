

with System;
with Report;
with CXD3003_1; use CXD3003_1;
procedure CXD3003 is
   Priority_Top : constant System.Priority := System.Priority'Last;
   Priority_Int : constant System.Interrupt_Priority := 
                                             System.Interrupt_Priority'First; 
begin

   Report.Test ("CXD3003", "Ceiling_Locking: default priority ceiling" &
                           " for an interrupt handler protected object");
   
   declare -- encapsulate the test


      task Task_of_Top_Priority is  
         pragma priority ( Priority_Top );
      end Task_of_Top_Priority;  

      task Task_of_Int_Priority is  
         pragma interrupt_priority ( Priority_Int );
      end Task_of_Int_Priority;  


      -- These tasks call a protected object whose ceiling should be
      -- higher than the task's priority
      --   

      task body Task_of_Top_Priority is 
         Numb : Task_Number := 1;
      begin
         Protected_Object.For_Ceiling_Check ( Numb );  -- Should be O.K.
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_of_Top_Priority");
      end Task_of_Top_Priority; 

       
      task body Task_of_Int_Priority is 
         Numb : Task_Number := 2;
      begin
         Protected_Object.For_Ceiling_Check ( Numb );  -- Should be O.K.
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_of_Int_Priority");
      end Task_of_Int_Priority; 


   begin
      null;
   end;  -- encapsulation

   -- Now check that the calls were actually made and not optimized away
   --  
   if not Protected_Object.Verify_Calls then 
      Report.Failed ("For_Ceiling_Check not called correctly");
   end if;

   Report.Result;

end CXD3003;
