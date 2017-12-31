     --===================================================================--

with Cc54001_1;

with Report;
procedure Cc54001 is

   package Messages renames Cc54001_1.Stack_Of_Messages;

   Msg0, Msg1, Msg2, Msg3 : Cc54001_1.Message_Ptr;

begin
   Report.Test
     ("CC54001",
      "Check that a general access-to-constant type " &
      "may be passed as an actual to a generic formal " &
      "access-to-constant type");

   Cc54001_1.Create_Message_Stack;

   Messages.Pop (Cc54001_1.Message_Stack, Msg3);  -- Pop items off stack in the
   Messages.Pop (Cc54001_1.Message_Stack, Msg2);  -- reverse order that they
   Messages.Pop (Cc54001_1.Message_Stack, Msg1);  -- were pushed.
   Messages.Pop (Cc54001_1.Message_Stack, Msg0);

   if Msg0.all /= Cc54001_1.Message_0 or else Msg1.all /= Cc54001_1.Message_1
     or else Msg2.all /= Cc54001_1.Message_2
     or else Msg3.all /= Cc54001_1.Message_3 then
      Report.Failed ("Items popped off of stack do not match those pushed");
   end if;

   Report.Result;
end Cc54001;
