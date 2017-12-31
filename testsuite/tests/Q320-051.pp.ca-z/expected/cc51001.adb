     --==================================================================--

with Cc51001_0;  -- Root type for message class.
with Cc51001_1;  -- Extensions to message class.
with Cc51001_2;  -- I/O operations for message class.

with Report;
procedure Cc51001 is

   -- Instantiate for various types in the class:

   package Msgs is new Cc51001_2 (Cc51001_0.Msg_Type);         -- Definite.
   package Fmsgs is new Cc51001_2 (Cc51001_1.From_Msg_Type);    -- Indefinite.
   package Tfmsgs is new Cc51001_2 (Cc51001_1.To_From_Msg_Type); -- Indefinite.

   Msg  : Cc51001_0.Msg_Type      := (Text => "This is message #001");
   Fmsg : Cc51001_1.From_Msg_Type :=
     (Text => "This is message #002", Slen => 2, From => "Me");
   Tfmsg : Cc51001_1.To_From_Msg_Type :=
     (Text => "This is message #003", From => "You       ", Dlen => 4,
      To   => "Them");

   Expected_Msg   : constant String := "This is message #001";
   Expected_Fmsg  : constant String := "This is message #002";
   Expected_Tfmsg : constant String := "This is message #003";

begin
   Report.Test
     ("CC51001",
      "Check that the formal derived type may have " &
      "an unknown discriminant part. Check that the ancestor " &
      "type in a formal derived type definition may be a " &
      "tagged type, and that the actual parameter may be any " &
      "definite or indefinite descendant of the ancestor type");

   if (Msgs.Print_Message (Msg) /= Expected_Msg) then
      Report.Failed ("Wrong result for definite root type");
   end if;

   if (Fmsgs.Print_Message (Fmsg) /= Expected_Fmsg) then
      Report.Failed ("Wrong result for direct indefinite derivative");
   end if;

   if (Tfmsgs.Print_Message (Tfmsg) /= Expected_Tfmsg) then
      Report.Failed ("Wrong result for Indirect indefinite derivative");
   end if;

   Report.Result;
end Cc51001;
