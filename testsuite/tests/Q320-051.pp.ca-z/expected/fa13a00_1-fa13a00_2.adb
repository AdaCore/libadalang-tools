--==================================================================--

package body Fa13a00_1.Fa13a00_2 is

   -- Go up from the current floor.

   procedure Up (Howmany : in Floor_No) is
   begin
      Current_Floor := Floor'Val (Floor'Pos (Current_Floor) + Howmany);
   end Up;

   --------------------------------------------

   -- Go down from the current floor.

   procedure Down (Howmany : in Floor_No) is
   begin
      Current_Floor := Floor'Val (Floor'Pos (Current_Floor) - Howmany);
   end Down;

end Fa13a00_1.Fa13a00_2;
