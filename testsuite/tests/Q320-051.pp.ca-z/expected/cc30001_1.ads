     --==================================================================--

with Cc30001_0;  -- Root message type.
generic          -- Generic "mail" package.
   type Message is new Cc30001_0.Msg_Type with private;
package Cc30001_1 is

   type Mail_Type is new Message with record   -- Derived from formal type.
      To : String (1 .. 8);
   end record;

   procedure Send (M : in out Mail_Type);      -- For this test, this version
   -- of Send should be called in
   -- ... Other operations. -- all cases.

end Cc30001_1;
