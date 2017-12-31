------------------------------------------------------------------------

with Report;
with Cxe1001_P;      -- a procedure who's ID is to be checked
with Cxe1001_Q;      -- a procedure who's ID is to be checked
with System;

procedure Cxe1001_A is

   type Hold_Partition_Id is range System.Min_Int .. System.Max_Int;
   P_Id    : Hold_Partition_Id;
   Q_Id    : Hold_Partition_Id;
   Main_Id : Hold_Partition_Id := Cxe1001_A'Partition_Id;

begin

   Report.Test
     ("CXE1001_A", "Check Partition IDs. " & "-- This is the FIRST PARTITION");

   Cxe1001_P;
   Cxe1001_Q;
   P_Id := Cxe1001_P'Partition_Id;
   Q_Id := Cxe1001_Q'Partition_Id;
   if P_Id /= Q_Id then
      Report.Failed
        ("Partition IDs of the procedures in this " &
         "partition are not the same");
   end if;
   if P_Id /= Main_Id then
      Report.Failed
        ("Partition ID of main not same as procedure " & "in partition" &
         Hold_Partition_Id'Image (Main_Id) & Hold_Partition_Id'Image (P_Id));
   end if;
   Report.Special_Action
     ("Partition ID of FIRST Partition is: " & Hold_Partition_Id'Image (P_Id) &
      ".  Check that this is different from that " &
      "of the SECOND partition");

   Report.Result;

end Cxe1001_A;
