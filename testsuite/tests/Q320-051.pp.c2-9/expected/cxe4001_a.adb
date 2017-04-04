-----------------------------------------------------------------------------

with Cxe4001_Partition_A;
with Cxe4001_Partition_B;
with Report;
with System.Rpc;
procedure Cxe4001_A is
begin
   Report.Test ("CXE4001_A", "Exception handling across partitions");

   -- make sure partitioning is performed
   if Cxe4001_Partition_A'Partition_Id = Cxe4001_Partition_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE4001_Partition_A and " &
         "CXE4001_Partition_B are in the same partition.");
   end if;

   -- now do the tests
   Cxe4001_Partition_A.Predefined_Simple;
   Cxe4001_Partition_A.Userdefined_Simple;
   Cxe4001_Partition_A.Invisible_Simple;
   Cxe4001_Partition_A.Invisible_Complex_1;
   Cxe4001_Partition_A.Invisible_Complex_2;

   -- all done
   Cxe4001_Partition_B.Finished;
   Report.Result;
end Cxe4001_A;
