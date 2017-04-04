with Report;
with Impdef.Annex_E;
with Cxe4003_Part_B1;
package body Cxe4003_Cancellation_Test is
   -- Check that a remote procedure call can be aborted.
   --
   -- This is first done using asynchronous transfer of control where the
   -- triggering_statement is a delay of a period that is shorter than the time
   -- required for the RPC to complete. The remote procedure contains a delay
   -- to insure that it takes longer than the timeout. The test is repeated
   -- several times to insure that once an RPC is aborted it is still possible
   -- to try again. Finally, the same remote procedure is called without a
   -- timeout to make sure it is possible to really call the procedure.

   procedure Atc_Test is
      Was_Aborted : Boolean;
   begin
      Cxe4003_Part_B1.Start_Cancellation_Test;
      for Several in 1 .. 10 loop
         Was_Aborted := False;

         select
            -- delay enough time to insure the remote procedure is executing
            delay Impdef.Annex_E.Max_Rpc_Call_Time;
            Was_Aborted := True;
         then abort
            -- make sure the remote procedure takes longer than the timeout
            Cxe4003_Part_B1.May_Be_Cancelled
              (Impdef.Annex_E.Max_Rpc_Call_Time +
               Impdef.Annex_E.Max_Rpc_Call_Time);
         end select;

         if not Was_Aborted then
            Report.Failed
              ("RPC was not aborted - iteration" & Integer'Image (Several));
         end if;
      end loop;

      -- this one shouldn't be cancelled
      Cxe4003_Part_B1.May_Be_Cancelled (0.0);

      -- tell the other partition that it can report its results for this test
      -- now.
      Cxe4003_Part_B1.End_Cancellation_Test ("atc");
   end Atc_Test;

   --
   -- This is done again using an abort statement. The remote procedure
   -- contains a delay to insure that it takes longer than the delay used here
   -- before the abort is performed. The test is repeated several times to
   -- insure that once an RPC is aborted it is still possible to try again.
   -- Finally, the same remote procedure is called without an abort to make
   -- sure it is possible to really call the procedure.

   procedure Abort_Test is
      Preemptive_Abort : Boolean := True;

      -- indication to the Agent_Task as to whether or not it will be aborted.
      Abort_Expected : Boolean := True;

      task type Agent_Task;
      task body Agent_Task is
      begin
         -- make sure the remote procedure takes longer than the timeout
         Cxe4003_Part_B1.May_Be_Cancelled
           (Impdef.Annex_E.Max_Rpc_Call_Time +
            Impdef.Annex_E.Max_Rpc_Call_Time);
         -- if preemptive aborts are supported then we should never get to the
         -- next statement
         Preemptive_Abort := False;

         -- in the absence of preemptive aborts, give the runtime a chance to
         -- do the abort
         delay 0.0;  -- abort completion point

         if Abort_Expected then
            -- if we get here then the task was not aborted. Could be because
            -- the RPC returned prematurely (an error). Alternatively, the
            -- value of ImpDef.Annex_E.Max_RPC_Call_Time may not be large
            -- enough. If this is the case, a new value should be picked.
            Report.Failed ("abort did not take place");
         end if;
      end Agent_Task;

   begin
      Cxe4003_Part_B1.Start_Cancellation_Test;
      for Several in 1 .. 10 loop

         declare
            The_Agent : Agent_Task;
         begin
            -- delay enough time to insure the remote procedure is executing
            -- but not so long that the call will complete.
            delay Impdef.Annex_E.Max_Rpc_Call_Time;
            abort The_Agent;
         end;

      end loop;

      -- state our findings about preemptive abort. Note that preemptive abort
      -- is only required if the Real-Time Annex is supported.
      if not Preemptive_Abort then
         if Impdef.Validating_Annex_D then
            Report.Failed ("preemptive abort is not supported");
         else
            Report.Comment ("preemptive abort is not supported");
         end if;
      end if;

      -- this one shouldn't be cancelled
      Abort_Expected := False;
      declare
         The_Agent : Agent_Task;
      begin
         null;  -- wait for the agent to finish
      end;

      -- tell the other partition that it can report its results for this test
      -- now.
      Cxe4003_Part_B1.End_Cancellation_Test ("abort");

   end Abort_Test;

   procedure Do_Test is
   begin
      Atc_Test;
      Abort_Test;
   end Do_Test;
end Cxe4003_Cancellation_Test;
