     --==================================================================--

package body Cxc3003_0 is

   protected body Dynamic_Handler is
      procedure Handle_Interrupt is
      begin
         Was_Handled := True;
      end Handle_Interrupt;

      function Handled return Boolean is
      begin
         return Was_Handled;
      end Handled;
   end Dynamic_Handler;

end Cxc3003_0;
