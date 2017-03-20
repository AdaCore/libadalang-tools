--==================================================================--

with Fa13a00_1.Fa13a00_2;                 -- Floor Calculation

package body Fa13a00_1.Fa13a00_3 is

   -- Going up or down depends on the current floor.

   procedure Move_Elevator (F : in Floor; C : in out Call_Waiting_Type) is
   begin
      if F > Current_Floor then
         Fa13a00_1.Fa13a00_2.Up (Floor'Pos (F) - Floor'Pos (Current_Floor));
         Fa13a00_1.Call (F, C);
      elsif F < Current_Floor then
         Fa13a00_1.Fa13a00_2.Down (Floor'Pos (Current_Floor) - Floor'Pos (F));
         Fa13a00_1.Call (F, C);
      end if;

   end Move_Elevator;

end Fa13a00_1.Fa13a00_3;
