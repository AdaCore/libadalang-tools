package Cxe4001_Partition_B is
   pragma Remote_Call_Interface;

   procedure Raise_Program_Error;
   procedure Raise_Visible_Exception;
   procedure Raise_Invisible_Exception;
   procedure Call_A_Raise_Invisible_1;
   procedure Call_A_Raise_Invisible_2;

   procedure Finished;
end Cxe4001_Partition_B;
