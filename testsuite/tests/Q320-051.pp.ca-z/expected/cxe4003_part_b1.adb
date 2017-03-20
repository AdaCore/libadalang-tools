-----------------------------------------------------------------------------

with Report;
package body Cxe4003_Part_B1 is
   Cancellation_Test_Start_Count  : Integer;
   Cancellation_Test_Finish_Count : Integer;

   task Sequence is
      -- blocking test support
      entry Block_2;
      entry Release_1;
      entry Release_2;

      -- last thing to call
      entry Finished;
   end Sequence;

   task body Sequence is
   begin

      -- the protocol needed by the blocking test
      accept Block_2 do
         accept Release_1;
         accept Release_2;
      end Block_2;

      -- allow the task to complete which will then allow the partition
      -- to complete but only when told to do so.
      accept Finished;
      Report.Result;

   end Sequence;

   ------

   procedure Test_Complete is
   begin
      Sequence.Finished;
   end Test_Complete;

   ------

   procedure Block_2 is
   begin
      Sequence.Block_2;
   end Block_2;

   procedure Release_1 is
   begin
      Sequence.Release_1;
   end Release_1;

   procedure Release_2 is
   begin
      Sequence.Release_2;
   end Release_2;

   ------
   procedure Start_Cancellation_Test is
   begin
      Cancellation_Test_Start_Count  := 0;
      Cancellation_Test_Finish_Count := 0;
   end Start_Cancellation_Test;

   procedure May_Be_Cancelled (Delay_Time : Duration) is
   begin
      -- this first statement should always be executed
      Cancellation_Test_Start_Count := Cancellation_Test_Start_Count + 1;

      -- the cancellation should come in during this delay
      delay Delay_Time;

      -- this statement should only be executed if the remote call is
      -- not really cancelled.  This is legal but not especially desirable.
      Cancellation_Test_Finish_Count := Cancellation_Test_Finish_Count + 1;
   end May_Be_Cancelled;

   procedure End_Cancellation_Test (Name : String) is
   begin
      if Cancellation_Test_Start_Count /= 11 then
         Report.Failed
           ("Remote procedure in cancellation test " &
            "was called" &
            Integer'Image (Cancellation_Test_Start_Count) &
            " times instead of 11 times");
      else
         null;
         -- Report.Comment ("cancellation test calls ok");
      end if;

      case Cancellation_Test_Finish_Count is
         when 0 =>
            -- the call that wasn't aborted should have finished
            Report.Failed ("last RPC for cancellation test did not complete");
         when 1 =>
            -- this is the desired result
            null;
         -- Report.Comment ("all aborted remote calls were cancelled for " &
         --                 Name & " test");
         when 11 =>
            -- undesirable but not illegal.
            Report.Comment
              ("none of the aborted remote calls were " &
               "actually cancelled for " &
               Name &
               " test");
         when others =>
            -- some, but not all, of the calls were cancelled.
            -- this is undesirable but not illegal.
            Report.Comment
              (Integer'Image (11 - Cancellation_Test_Finish_Count) &
               " remote calls out of 10 were cancelled for " &
               Name &
               " test");
      end case;

   end End_Cancellation_Test;
end Cxe4003_Part_B1;
