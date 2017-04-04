-----------------------------------------------------------------------------
--
-- main procedure for partition A (and the test)
--

with Cxe5002_Part_A;
with Cxe5002_Part_B;
with Cxe5002_State;
with System.Rpc;
with Report;
procedure Cxe5002_A is
   use type System.Rpc.Partition_Id;
begin
   Report.Test ("CXE5002", "Partition Communication System usage");

   -- make sure partitioning is performed correctly
   if Cxe5002_Part_A'Partition_Id = Cxe5002_Part_B'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE5002_Part_A and CXE5002_Part_B" &
         " are in the same partition.");
   end if;
   if Cxe5002_Part_A'Partition_Id /= Cxe5002_A'Partition_Id then
      Report.Failed
        ("Partitioning Error - CXE5002_Part_A and CXE5002_A" &
         " are not in the same partition.");
   end if;

   -- test calls to the other partition
   Cxe5002_State.Apc_Expected         := False;
   Cxe5002_State.Remote_Call_Occurred := False;
   Cxe5002_State.Calls_To_Partition_B := True;
   begin
      Cxe5002_Part_B.Remote_Normal (1);
      Report.Failed ("Expected exception not raised");
   exception
      when System.Rpc.Communication_Error =>
         null;
      when others =>
         Report.Failed ("Unexpected exception raised");
   end;
   if Cxe5002_State.Remote_Call_Occurred then
      null;
   -- Report.Comment ("Remote call went through System.RPC");
   else
      Report.Failed ("Remote call did not go through System.RPC");
   end if;

   Cxe5002_State.Apc_Expected         := True;
   Cxe5002_State.Remote_Call_Occurred := False;
   Cxe5002_State.Calls_To_Partition_B := True;
   begin
      Cxe5002_Part_B.Remote_Async (2);
      Report.Failed ("Expected exception not raised");
   exception
      when System.Rpc.Communication_Error =>
         null;
      when others =>
         Report.Failed ("Unexpected exception raised");
   end;
   if Cxe5002_State.Remote_Call_Occurred then
      null;
   -- Report.Comment ("Remote async call went through System.RPC");
   else
      Report.Failed ("Remote async call did not go through System.RPC");
   end if;

   -- test calls to this partition

   Cxe5002_State.Apc_Expected         := False;
   Cxe5002_State.Remote_Call_Occurred := False;
   Cxe5002_State.Calls_To_Partition_B := False;
   begin
      Cxe5002_Part_A.Local_Remote_Call;
      Report.Failed ("Expected exception not raised");
   exception
      when System.Rpc.Communication_Error =>
         null;
      when others =>
         Report.Failed ("Unexpected exception raised");
   end;
   if Cxe5002_State.Remote_Call_Occurred then
      null;
   -- Report.Comment ("Local remote call went through System.RPC");
   else
      Report.Failed ("Local remote call did not go through System.RPC");
   end if;

   -- finish up
   Report.Result;
end Cxe5002_A;
