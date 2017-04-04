     --==================================================================--

with Ada.Interrupts;
package Cxc3003_1 is

   protected type Handler_Type (Id : Ada.Interrupts.Interrupt_Id) is
      procedure Handle_Interrupt;
      pragma Attach_Handler (Handle_Interrupt, Id);

      function Handled return Boolean;
   private
      Was_Handled : Boolean := False;
   end Handler_Type;

end Cxc3003_1;
