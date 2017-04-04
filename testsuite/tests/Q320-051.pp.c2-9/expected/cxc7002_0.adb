package body Cxc7002_0 is
   task body Lib_Tasks is
   begin
      accept Get_Id (Id : out Ada.Task_Identification.Task_Id) do
         Id := Ada.Task_Identification.Current_Task;
      end Get_Id;
      accept Ok_To_Terminate;
   end Lib_Tasks;
end Cxc7002_0;
