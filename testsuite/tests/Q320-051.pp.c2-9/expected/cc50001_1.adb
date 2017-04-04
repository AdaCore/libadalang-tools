     --===================================================================--

package body Cc50001_1 is

   -- For the sake of brevity, the implementation of Push is completely
   -- artificial; the goal is to model a call of the equality operator
   -- within the generic.
   --
   -- A real application might implement Push such that it does not add new
   -- items to the stack if they are identical to the top item; in that case,
   -- the equality operator would be called as part of an "if" condition.

   procedure Push (I : in Item; Tc_Check : out Boolean) is
   begin
      Tc_Check := not (I = I);              -- Call user-defined "="; should
      -- return FALSE. Negation of
      -- result makes TC_Check TRUE.
   end Push;

end Cc50001_1;
