-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with Tctouch;
package body C392011_0 is

   The_Character : Character := 'A';

   function Create return Level_0 is
      Created_Item_0 : constant Level_0 := (Ch_Item => The_Character);
   begin
      The_Character := Character'Succ (The_Character);
      Tctouch.Touch
        ('A'); -- --- ---- ----- ---- --- -- --- ---- ----- ---- -- A
      return Created_Item_0;
   end Create;

   procedure Check (Left, Right : in Level_0) is
   begin
      Tctouch.Touch
        ('B'); -- --- ---- ----- ---- --- -- --- ---- ----- ---- -- B
   end Check;

end C392011_0;
