     --==================================================================--

with Ada.Interrupts;
pragma Elaborate (Ada.Interrupts);

package Cxc3005_1 is

   Dummy : Integer := 0;

   Reserved_Interrupt_Found : Boolean := False;
   Reserved_Interrupt       : Ada.Interrupts.Interrupt_Id;

   procedure Find_Reserved_Interrupt
     (Found : out Boolean; Id : out Ada.Interrupts.Interrupt_Id);

   procedure Avoid_Optimization (Id : in out Ada.Interrupts.Interrupt_Id);

   procedure Avoid_Optimization
     (Id : in out Ada.Interrupts.Parameterless_Handler);

end Cxc3005_1;
