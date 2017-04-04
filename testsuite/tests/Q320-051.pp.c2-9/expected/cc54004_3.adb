     --===================================================================--

package body Cc54004_3 is

   procedure Tc_Create_Alert_Stack is
   begin
      Alert_Stacks.Push (Alert_List, new Cc54004_1.Low_Alert);
      Alert_Stacks.Push (Alert_List, new Cc54004_1.Medium_Alert);
   end Tc_Create_Alert_Stack;

end Cc54004_3;
