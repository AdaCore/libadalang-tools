     --===================================================================--

with Cc54003_1;  -- Message abstraction.
with Cc54003_2;  -- Message-operation stack.

with Report;
procedure Cc54003 is

   package Msg_Ops renames Cc54003_2.Stack_Of_Ops;

   Msg      : Cc54003_1.Message_Ptr := new Cc54003_1.Message'("Hello there");
   Expected : Cc54003_1.Message     := "Dummy: Hello there (12:03pm)";

begin
   Report.Test
     ("CC54003",
      "Check that a general access-to-subprogram type " &
      "may be passed as an actual to a generic formal " &
      "access-to-subprogram type");

   Cc54003_2.Create_Operation_Stack;

   declare
      Actual : Cc54003_1.Message_Ptr :=
        Msg_Ops.Execute_Stack (Cc54003_2.Operation_Stack, Msg);
   begin
      if Actual.all /= Expected then
         Report.Failed ("Wrong result from dereferenced subprogram execution");
      end if;
   end;

   Report.Result;
end Cc54003;
