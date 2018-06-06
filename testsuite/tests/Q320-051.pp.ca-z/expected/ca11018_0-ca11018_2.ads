--=================================================================--

-- Public generic child to copy words to the messages.

generic
   type Message_Type is new Message_Rec with private;
   -- Derived from parent's type.

package Ca11018_0.Ca11018_2 is

   procedure Copy
     (From_The_Word : in Message; To_The_Message : in out Message_Type);

end Ca11018_0.Ca11018_2;
