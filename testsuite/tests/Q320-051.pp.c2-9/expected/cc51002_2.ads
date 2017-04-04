     --==================================================================--

with Cc51002_0;       -- Root message type and operations.
package Cc51002_2 is  -- Extended message type and operations.

   type Sender_Type is (Inside, Outside);

   type Who_Msg_Type is new Cc51002_0.Msg_Type with record   -- Derivative of
      From : Sender_Type;                                    -- root type of
   end record;                                               -- class.

   -- Note: this overriding version of Send has different parameter names from
   -- the root type's function. It also has no default expression.

   function Send (M : Who_Msg_Type;                          -- Overrides
   R                : Cc51002_0.Recipients) return Boolean;  -- root type's
   -- operation.
   -- ...Other extended message operations.

end Cc51002_2;
