     --==================================================================--

with Impdef.Annex_C;
package Cxc3006_1 is

   protected Static is
      procedure Handler;
      pragma Attach_Handler (Handler, Impdef.Annex_C.Interrupt_To_Generate);

      procedure Reset;
      function Handled return Boolean;
   private
      Was_Handled : Boolean := False;
   end Static;

end Cxc3006_1;
