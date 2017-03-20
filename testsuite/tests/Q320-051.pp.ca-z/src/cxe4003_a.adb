
-----------------------------------------------------------------------------

with Report;
with CXE4003_Part_B1;
with CXE4003_Blocking_Test;
with CXE4003_Cancellation_Test;
with CXE4003_Call_Test;
procedure CXE4003_A is
begin
  Report.Test ("CXE4003_A", "Check the blocking properties of " &
                            "remote calls");
  -- Check that the task executing a remote subprogram call blocks
  -- until the subprogram in the called partition returns.
  CXE4003_Blocking_Test.Do_Test;

  -- Check that a remote procedure call can be aborted.
  CXE4003_Cancellation_Test.Do_Test;

  -- Check that remote subprogram calls are executed at most once.
  -- Check that potentially concurrent calls from multiple tasks
  -- can be handled by the PCS.
  CXE4003_Call_Test.Do_Test;

   -- All done.
  CXE4003_Part_B1.Test_Complete;
  Report.Result;
exception
  when others =>
     Report.Failed ("Unexpected exception in test");
     Report.Result;
end CXE4003_A;
