     --===================================================================--

with Cc54001_0;      -- Generic stack of pointers.
pragma Elaborate (Cc54001_0);

package Cc54001_1 is

   subtype Message is String;
   type Message_Ptr is access constant Message;

   Message_Count : constant := 4;

   Message_0 : aliased constant Message := "Hello";
   Message_1 : aliased constant Message := "Doctor";
   Message_2 : aliased constant Message := "Name";
   Message_3 : aliased constant Message := "Continue";

   package Stack_Of_Messages is new Cc54001_0
     (Element_Type => Message,
      Element_Ptr  => Message_Ptr,
      Size         => Message_Count);

   Message_Stack : Stack_Of_Messages.Stack_Type;

   procedure Create_Message_Stack;

end Cc54001_1;
