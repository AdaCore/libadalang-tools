-----------------------------------------------------------------------------

with Report;
with Cxe4003_Part_B1;
with Cxe4003_Blocking_Test;
with Cxe4003_Cancellation_Test;
with Cxe4003_Call_Test;
procedure Cxe4003_A is
begin
   Report.Test
     ("CXE4003_A",
      "Check the blocking properties of " & "remote calls");
   -- Check that the task executing a remote subprogram call blocks
   -- until the subprogram in the called partition returns.
   Cxe4003_Blocking_Test.Do_Test;

   -- Check that a remote procedure call can be aborted.
   Cxe4003_Cancellation_Test.Do_Test;

   -- Check that remote subprogram calls are executed at most once.
   -- Check that potentially concurrent calls from multiple tasks
   -- can be handled by the PCS.
   Cxe4003_Call_Test.Do_Test;

   -- All done.
   Cxe4003_Part_B1.Test_Complete;
   Report.Result;
exception
   when others =>
      Report.Failed ("Unexpected exception in test");
      Report.Result;
end Cxe4003_A;
