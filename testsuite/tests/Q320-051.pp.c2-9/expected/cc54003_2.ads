     --===================================================================--

with Cc54003_0;      -- Generic stack of pointers.
pragma Elaborate (Cc54003_0);

with Cc54003_1;      -- Message abstraction.

package Cc54003_2 is

   type Operation_Ptr is access function
     (Msg_Ptr : Cc54003_1.Message_Ptr) return Cc54003_1.Message_Ptr;

   Maximum_Ops : constant := 4;         -- Arbitrary.

   package Stack_Of_Ops is new Cc54003_0
     (Item_Type    => Cc54003_1.Message,
      Item_Ptr     => Cc54003_1.Message_Ptr,
      Function_Ptr => Operation_Ptr,
      Size         => Maximum_Ops);

   Operation_Stack : Stack_Of_Ops.Stack_Type;

   procedure Create_Operation_Stack;

end Cc54003_2;
