with Report;
with Cxe4003_Part_B1;
with Impdef;
package body Cxe4003_Blocking_Test is
   Rpc_In_Progress : Boolean := False;

   task Gets_Blocked is
      entry Go;
   end Gets_Blocked;

   task body Gets_Blocked is
   begin
      accept Go;
      Rpc_In_Progress := True;
      Cxe4003_Part_B1.Block_2;
      Rpc_In_Progress := False;
      -- Report.Comment ("Call to Block_2 is now complete");
   end Gets_Blocked;

   procedure Do_Test is
   begin
      Gets_Blocked.Go;
      -- Gets_Blocked is now free to make the RPC The following delay gives
      -- Gets_Blocked plenty of opportunity to run but isn't really necessary
      -- since we explicitly block when we call Release_1
      delay Impdef.Minimum_Task_Switch;
      -- At this point the call should be in progress
      Cxe4003_Part_B1.Release_1;
      -- at this point we know that task Gets_Blocked should be blocked in the
      -- call to Block_2.
      if not Rpc_In_Progress then
         Report.Failed ("task did not block during RPC");
      end if;

      -- allow the task to complete
      Cxe4003_Part_B1.Release_2;
   end Do_Test;
end Cxe4003_Blocking_Test;
