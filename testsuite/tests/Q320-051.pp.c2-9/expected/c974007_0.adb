     --==================================================================--

with Report;
package body C974007_0 is

   protected body Atm_Keyboard_Protected is

      -- Barrier is false for the live of the test
      entry Cancel_Pressed when (Last_Key_Pressed = Cancel) is
      begin
         Triggering_Statement_Executed := True;   -- Test has failed
         -- (Note: cannot call Report.Failed in the protected entry body]
      end Cancel_Pressed;

      procedure Read_Key is                                     -- Never
      begin                                                     -- called.
         -- Simulate a procedure which reads user keyboard input, and which is
         -- called by some interrupt handler.
         Last_Key_Pressed := Cancel;
      end Read_Key;

   end Atm_Keyboard_Protected;

   procedure Read_Card (Card : in out Atm_Card_Type) is
   begin
      Card.Number := 9_999;
      Card.Pin    := 111;
   end Read_Card;

   procedure Validate_Card (Card : in Atm_Card_Type) is
   begin
      Abortable_Part_Executed := True;
   end Validate_Card;

   procedure Perform_Transaction (Card : in Atm_Card_Type) is
   begin
      Perform_Transaction_Executed := True;
   end Perform_Transaction;

end C974007_0;
