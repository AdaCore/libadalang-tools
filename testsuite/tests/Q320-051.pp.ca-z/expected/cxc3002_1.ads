     --==================================================================--

with Ada.Interrupts;
pragma Elaborate (Ada.Interrupts);

package Cxc3002_1 is

   Reserved_Interrupt_Found : Boolean := False;
   Id                       : Ada.Interrupts.Interrupt_Id;

   procedure Find_Reserved_Interrupt (Found : out Boolean;
      Int_Id                                : out Ada.Interrupts.Interrupt_Id);

end Cxc3002_1;
