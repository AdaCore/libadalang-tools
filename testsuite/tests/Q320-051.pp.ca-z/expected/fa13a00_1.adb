     --==================================================================--

package body Fa13a00_1 is

   -- Call the elevator.

   procedure Call (F : in Floor; C : in out Call_Waiting_Type) is
   begin
      C (F) := True;
   end Call;

   --------------------------------------------

   -- Clear all calls of the elevator.

   procedure Clear_Calls (C : in out Call_Waiting_Type) is
   begin
      C := (others => False);
   end Clear_Calls;

end Fa13a00_1;
