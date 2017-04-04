-----------------------------------------------------------------------------
--
-- main procedure for partition A (and the test)
--

with Cxe5003_Part_A1;
with Cxe5003_Part_A2;
with Cxe5003_Part_B;
with Cxe5003_Check;
with Cxe5003_Normal;
with Report;
procedure Cxe5003_A is
begin
   Cxe5003_Check.Main := True;

   -- Note that the expected behavior is for all the elaboration to take
   -- place first, System.RPC.Establish_RPC_Receiver to be called, and then
   -- this main procedure to be called.  Report.Test is called in the
   -- Establish_RPC_Receiver routine.

   if not Cxe5003_Check.Establish then
      Report.Test
        ("CXE5003",
         "Partition Communication System elaboration " & "and initialization");
      Report.Failed ("System.RPC.Establish_RPC_Receiver was not called");
   end if;

   -- finish up
   Report.Result;
end Cxe5003_A;
