
-----------------------------------------------------------------------------
--
-- main procedure for partition A (and the test)
--

with CXE5003_Part_A1;
with CXE5003_Part_A2;
with CXE5003_Part_B;
with CXE5003_Check;
with CXE5003_Normal;
with Report;
procedure CXE5003_A is
begin
  CXE5003_Check.Main := True;

  -- Note that the expected behavior is for all the elaboration to take
  -- place first, System.RPC.Establish_RPC_Receiver to be called, and then
  -- this main procedure to be called.  Report.Test is called in the
  -- Establish_RPC_Receiver routine.

  if not CXE5003_Check.Establish then
    Report.Test ("CXE5003", "Partition Communication System elaboration " &
                            "and initialization");
    Report.Failed ("System.RPC.Establish_RPC_Receiver was not called");
  end if;

  -- finish up
  Report.Result;
end CXE5003_A;
