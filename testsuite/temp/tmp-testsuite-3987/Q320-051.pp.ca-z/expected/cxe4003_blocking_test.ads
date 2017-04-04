-----------------------------------------------------------------------------

package Cxe4003_Blocking_Test is

   -- Check that the task executing a remote subprogram call blocks
   -- until the subprogram in the called partition returns.
   --
   -- This is done by having a task make a call to partition B where
   -- the call is blocked until calls to Release_1 and Release_2 have
   -- been received.  The sequence of events is as follows:
   --
   --       main                        task
   --       ----                        ----
   --
   --       start task                 wait for start
   --                                  set flag
   --                                  call Block_2
   --       call Release_1
   --       check that flag is set
   --       call Release_2
   --                                  reset flag
   --
   procedure Do_Test;
end Cxe4003_Blocking_Test;
