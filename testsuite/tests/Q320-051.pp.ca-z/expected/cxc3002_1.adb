     --==================================================================--

package body Cxc3002_1 is

   procedure Find_Reserved_Interrupt (Found : out Boolean;
      Int_Id                                : out Ada.Interrupts.Interrupt_Id)
   is
   begin
      Found  := False;
      Int_Id := Ada.Interrupts.Interrupt_Id'First;
      for I in Ada.Interrupts.Interrupt_Id loop
         if Ada.Interrupts.Is_Reserved (I) then
            Int_Id := I;
            Found  := True;
            exit;
         end if;
      end loop;
   end Find_Reserved_Interrupt;

begin -- CXC3002_1.
   Find_Reserved_Interrupt (Reserved_Interrupt_Found, Id);
end Cxc3002_1;
