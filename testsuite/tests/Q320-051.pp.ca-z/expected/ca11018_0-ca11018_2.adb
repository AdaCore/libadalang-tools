--=================================================================--

package body Ca11018_0.Ca11018_2 is

   procedure Copy (From_The_Word : in     Message;
      To_The_Message             : in out Message_Type)
   is

   -- Copy words to the appropriate messages.

   begin
      To_The_Message.The_Content        -- Parent's private type.
        (1 .. From_The_Word'Length) := From_The_Word;

      To_The_Message.The_Length         -- Parent's private type.
      := From_The_Word'Length;
   end Copy;

end Ca11018_0.Ca11018_2;
