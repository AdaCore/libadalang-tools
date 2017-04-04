package body C93005c_Pk1 is

-- THIS TASK IS CALLED IF AN UNACTIVATED TASK IS EVER INCORRECTLY ACTIVATED. IT
-- REPORTS FAILURE.

   task T is
      entry E;
   end T;

   -- *********************************************** START OF DEFINITIONS FOR
   -- MUST NOT TERMINATE TASKS ***********************************************

-- COUNT OF TASKS THAT MUST NOT BE TERMINATED AND ARE STILL ACTIVE.

   Mnt_Count : Integer := 0;

-- TASK TO SYNCHRONIZE THE MNT_COUNT VARIABLE

   task Mnt_Counter is
      entry Incr;
      entry Decr;
   end Mnt_Counter;

-- SYNCHRONIZING TASK

   task body Mnt_Counter is
   begin
      loop
         select
            accept Incr do
               Mnt_Count := Mnt_Count + 1;
            end Incr;

         or
            accept Decr do
               Mnt_Count := Mnt_Count - 1;
            end Decr;

         or
            terminate;

         end select;
      end loop;
   end Mnt_Counter;

-- INCREMENT THE MNT_COUNT WHEN A TASK IS CREATED
--
   function F return Integer is
   begin
      Mnt_Counter.Incr;
      return 0;
   end F;

-- CHECK THAT THE MUST NOT BE TERMINATED TASKS ARE NOT YET TERMINATED AND THAT
-- THE SYNCRHONIZING TASK ITSELF IS NOT TERMINATED.
--
   procedure Check is
   begin
      if Mnt_Count /= 0 or Mnt_Counter'Terminated then
         Failed
           ("SOME MUST-NOT-TERMINATE TASK WAS PREMATURELY " & "TERMINATED");
      end if;
-- RESET THE COUNT FOR THE NEXT SUBTEST:
      Mnt_Count := 0;
   end Check;

-- A MUST NOT BE TERMINATED TASK. DELAY LONG ENOUGH TO BE THE LAST TASK OF A
-- SCOPE TO TERMINATE. THEN DECREMENT THE COUNTER.
--
   task body Mnt_Task is
   begin
      delay 5.0;
      Mnt_Counter.Decr;
   end Mnt_Task;

   -- *********************************************** END OF DEFINITIONS FOR
   -- MUST NOT TERMINATE TASKS ***********************************************

   task body T is
   begin
      loop
         select
            accept E do
               Failed ("SOME TYPE U TASK WAS ACTIVATED");
            end E;

         or
            terminate;
         end select;
      end loop;
   end T;

   -- TASKS OF TYPE UNACTIVATED MUST NEVER BE ACTIVATED.
   --
   task body Unactivated is
   begin
      T.E;
   end Unactivated;
end C93005c_Pk1;
