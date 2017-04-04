----------------------------------------------------------

package body Cxd6001_1 is

   task body Killer_Task is
      Victim : Tid.Task_Id;
   begin
      loop
         select
            accept Task_To_Abort (T : in Tid.Task_Id) do
               Victim := T;
            end Task_To_Abort;
         or
            terminate;
         end select;

         Stc.Suspend_Until_True (Kill_Now);
         Tid.Abort_Task (Victim);
      end loop;
   end Killer_Task;

end Cxd6001_1;
