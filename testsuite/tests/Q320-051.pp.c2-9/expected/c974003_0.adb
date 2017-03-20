     --==================================================================--

with Report;
with Impdef;

package body C974003_0 is

   procedure Listen_For_Input (Key : out Key_Enum) is
   begin
      -- Model the situation where the user waits a bit for the card to
      -- be validated, then presses cancel before it completes.

      -- Delay long enough to force queuing of Keyboard.Cancel_Pressed.
      delay Impdef.Minimum_Task_Switch;

      if Report.Equal (3, 3) then  -- Always true.
         Key := Cancel;
      end if;
   end Listen_For_Input;

   -- One of these gets created as "Keyboard" for each transaction
   --
   task body Atm_Keyboard_Task is
      Key_Pressed : Key_Enum := None;
   begin
      loop
         -- Force entry calls
         Listen_For_Input (Key_Pressed);                   -- to be queued,
         -- then set guard to
         -- true.
         select when (Key_Pressed = Cancel) =>                 -- Guard is now
            accept Cancel_Pressed do                    -- true, so accept
               Tc_Triggering_Statement_Completed := True;  -- queued entry
            end Cancel_Pressed;                         -- call.

            -- User has cancelled the transaction so we exit the
            -- loop and allow the task to terminate
            exit;
         else
            Key_Pressed := None;
         end select;

      end loop;
   exception
      when others =>
         Report.Failed ("Unexpected Exception in ATM_Keyboard_Task");
   end Atm_Keyboard_Task;

   procedure Read_Card (Card : in out Atm_Card_Type) is
   begin
      Card.Number := 9_999;
      Card.Pin    := 111;
   end Read_Card;

   procedure Validate_Card (Card : in Atm_Card_Type) is
   begin
      -- Simulate an exceedingly long validation activity.
      loop                                             -- Infinite loop.
         Tc_Count := (Tc_Count + 1) mod Integer (Card.Pin);
         -- Synch. point to allow transfer of control to Keyboard
         -- task during this simulation
         delay Impdef.Minimum_Task_Switch;
         exit when not Report.Equal (Tc_Count, Tc_Count);    -- Always false.
      end loop;
   end Validate_Card;

   procedure Perform_Transaction (Card : in Atm_Card_Type) is
   begin
      Report.Failed
        ("Triggering alternative sequence of statements " & "not executed");
      if not Tc_Triggering_Statement_Completed then
         Report.Failed ("Triggering statement did not complete");
      end if;
      if Tc_Count = 1_234 then
         -- Initial value is unchanged
         Report.Failed ("Abortable part did not execute");
      end if;
   end Perform_Transaction;

end C974003_0;
