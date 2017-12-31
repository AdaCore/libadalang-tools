     --==================================================================--

with Cc51002_0;  -- Root message type and operations.
with Cc51002_1;  -- Message class function.
with Cc51002_2;  -- Extended message type and operations.

with Report;
procedure Cc51002 is

   function Send_Msg is new Cc51002_1 (Cc51002_0.Msg_Type);
   function Send_Wmsg is new Cc51002_1 (Cc51002_2.Who_Msg_Type);

   Mess  : Cc51002_0.Msg_Type     := (Text => "Greetings!");
   Wmess : Cc51002_2.Who_Msg_Type :=
     (Text => "Willkommen", From => Cc51002_2.Outside);

   Tc_Okay_Mstatus  : Boolean := False;
   Tc_Okay_Wmstatus : Boolean := False;

begin
   Report.Test
     ("CC51002",
      "Check that, for formal derived tagged types, " &
      "the formal parameter names and default expressions for " &
      "a primitive subprogram in an instance are determined by " &
      "the primitive subprogram of the ancestor type, but that " &
      "the primitive subprogram body executed is that of the" & "actual type");

   Tc_Okay_Mstatus := Send_Msg (Mess);
   if not Tc_Okay_Mstatus then
      Report.Failed ("Wrong result from call to root type's operation");
   end if;

   Tc_Okay_Wmstatus := Send_Wmsg (Wmess);
   if not Tc_Okay_Wmstatus then
      Report.Failed ("Wrong result from call to derived type's operation");
   end if;

   Report.Result;
end Cc51002;
