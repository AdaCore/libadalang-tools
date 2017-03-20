
-----------------------------------------------------------------------------
--
-- main procedure for partition A (and the test)
--

with CXE5002_Part_A;
with CXE5002_Part_B;
with CXE5002_State;
with System.RPC;
with Report;
procedure CXE5002_A is
  use type System.RPC.Partition_ID;
begin
  Report.Test ("CXE5002", "Partition Communication System usage");

  -- make sure partitioning is performed correctly
  if CXE5002_Part_A'Partition_ID = CXE5002_Part_B'Partition_ID then
    Report.Failed ("Partitioning Error - CXE5002_Part_A and CXE5002_Part_B" &
                   " are in the same partition.");
  end if;
  if CXE5002_Part_A'Partition_ID /= CXE5002_A'Partition_ID then
    Report.Failed ("Partitioning Error - CXE5002_Part_A and CXE5002_A" &
                   " are not in the same partition.");
  end if;

  -- test calls to the other partition
  CXE5002_State.APC_Expected         := False;
  CXE5002_State.Remote_Call_Occurred := False;
  CXE5002_State.Calls_To_Partition_B := True;
  begin
    CXE5002_Part_B.Remote_Normal (1);
    Report.Failed ("Expected exception not raised");
  exception
    when System.RPC.Communication_Error => null;
    when others => Report.Failed ("Unexpected exception raised");
  end;
  if CXE5002_State.Remote_Call_Occurred then
    null;
    -- Report.Comment ("Remote call went through System.RPC");
  else
    Report.Failed ("Remote call did not go through System.RPC");
  end if;

  CXE5002_State.APC_Expected         := True;
  CXE5002_State.Remote_Call_Occurred := False;
  CXE5002_State.Calls_To_Partition_B := True;
  begin
    CXE5002_Part_B.Remote_Async (2);
    Report.Failed ("Expected exception not raised");
  exception
    when System.RPC.Communication_Error => null;
    when others => Report.Failed ("Unexpected exception raised");
  end;
  if CXE5002_State.Remote_Call_Occurred then
    null;
    -- Report.Comment ("Remote async call went through System.RPC");
  else
    Report.Failed ("Remote async call did not go through System.RPC");
  end if;

  -- test calls to this partition

  CXE5002_State.APC_Expected         := False;
  CXE5002_State.Remote_Call_Occurred := False;
  CXE5002_State.Calls_To_Partition_B := False;
  begin
    CXE5002_Part_A.Local_Remote_Call;
    Report.Failed ("Expected exception not raised");
  exception
    when System.RPC.Communication_Error => null;
    when others => Report.Failed ("Unexpected exception raised");
  end;
  if CXE5002_State.Remote_Call_Occurred then
    null;
    -- Report.Comment ("Local remote call went through System.RPC");
  else
    Report.Failed ("Local remote call did not go through System.RPC");
  end if;

  -- finish up
  Report.Result;
end CXE5002_A;
