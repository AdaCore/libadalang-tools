     --==================================================================--

with Report;
package body Cxc3005_1 is

   procedure Find_Reserved_Interrupt                -- Check all values of
     (Found : out Boolean;                          -- type Interrupt_ID for
      Id    : out Ada.Interrupts.Interrupt_Id)
   is   -- a reserved interrupt.
   begin                                            -- If one is found, exit
      Found := False;                               -- the loop and return
      Id    := Ada.Interrupts.Interrupt_Id'First;   -- True for Found.
      for I in Ada.Interrupts.Interrupt_Id loop     -- Otherwise return False
         if Ada.Interrupts.Is_Reserved (I) then      -- for Found.
            Id    := I;
            Found := True;
            exit;
         end if;
      end loop;
   end Find_Reserved_Interrupt;

   procedure Avoid_Optimization (Id : in out Ada.Interrupts.Interrupt_Id) is
   begin
      -- Condition is always false:
      if not Report.Equal (Report.Ident_Int (Dummy), Dummy) then
         Id := Ada.Interrupts.Interrupt_Id'Last;         -- Never executed.
      end if;
   end Avoid_Optimization;

   procedure Avoid_Optimization
     (Id : in out Ada.Interrupts.Parameterless_Handler)
   is
   begin
      -- Condition is always false:
      if not Report.Equal (Report.Ident_Int (Dummy), Dummy) then
         Id := null;                                     -- Never executed.
      end if;
   end Avoid_Optimization;

begin -- CXC3005_1.
   Find_Reserved_Interrupt (Reserved_Interrupt_Found, Reserved_Interrupt);
end Cxc3005_1;
