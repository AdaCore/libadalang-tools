with Cxe4001_Partition_B;
with Cxe4001_Partition_A;
with Report;
with System.Rpc;
procedure Cxe4001_B is
begin
   Report.Test ("CXE4001_B", "Server partition of exception handling test");
   if Cxe4001_Partition_A'Partition_Id = Cxe4001_Partition_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - 1 and Part_B are in the" & " same partition.");
   end if;
   -- Report.Result is called in the body of CXE4001_Partition_B.
end Cxe4001_B;
