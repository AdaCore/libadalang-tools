--=================================================================--

package body Ca110050_0 is              -- Package body Messages.

   Message_Count : Integer := 0;

   function Next_Available_Message return Descriptor is
   begin
      Message_Count := Message_Count + 5;
      return (Descriptor (Message_Count));
   end Next_Available_Message;

end Ca110050_0;                         -- Package body Messages.
