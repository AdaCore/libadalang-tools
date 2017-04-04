--------------------------------------------------------------------------

with Report;
package body Cxc7001_0 is
   use type Ada.Task_Identification.Task_Id;

   task body Task_Type is
      Who_I_Am : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task;
   begin
      loop
         select
            accept Check_Caller (Id : in Ada.Task_Identification.Task_Id) do
               if Id /= Check_Caller'Caller then
                  Report.Failed ("The caller is not the caller");
               end if;
            end Check_Caller;

         or
            accept My_Id (Id : out Ada.Task_Identification.Task_Id) do
               Id := Who_I_Am;
            end My_Id;

         or
            accept Shutdown;
            exit;

         or
            terminate;  -- don't hold up the test
         end select;
      end loop;
   end Task_Type;

end Cxc7001_0;
