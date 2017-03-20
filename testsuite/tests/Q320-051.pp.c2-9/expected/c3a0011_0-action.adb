-----------------------------------------------------------------------------

package body C3a0011_0.Action is

   procedure Rotate_Front is
   begin
      The_Heading := The_Heading + 5;
   end Rotate_Front;

   procedure Rotate_Back is
   begin
      The_Heading := The_Heading - 5;
   end Rotate_Back;

end C3a0011_0.Action;
