     --===================================================================--

with Cc50001_0;  -- Tagged (actual) type declarations.
generic        -- Generic stack abstraction.

   type Item (<>) is tagged private;            -- Formal tagged private type.

package Cc50001_1 is

   -- Simulate a generic stack abstraction. In a real application, the
   -- second operand of Push might be of type Stack, and type Stack
   -- would have at least one component (pointing to the top stack item).

   type Stack is private;

   procedure Push (I : in Item; Tc_Check : out Boolean);

   -- ... Other stack operations.

private

   -- ... Stack and ancillary type declarations.

   type Stack is record                       -- Artificial.
      null;
   end record;

end Cc50001_1;
