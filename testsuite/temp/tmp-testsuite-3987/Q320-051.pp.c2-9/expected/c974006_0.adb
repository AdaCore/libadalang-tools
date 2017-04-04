     --==================================================================--

with Report;
package body C974006_0 is

   protected body Atm_Keyboard_Protected is

      entry Cancel_Pressed when (Last_Key_Pressed = Cancel) is
      begin
         Entry_Body_Executed := True;
      end Cancel_Pressed;

      procedure Read_Key is
      begin
         -- Simulate a procedure which processes user keyboard input, and
         -- which is called by some interrupt handler.
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
      Report.Failed ("Abortable part executed");
   end Validate_Card;

   procedure Perform_Transaction (Card : in Atm_Card_Type) is
   begin
      Report.Failed
        ("Triggering alternative sequence of statements " &
         "not fully executed");
   end Perform_Transaction;

end C974006_0;
