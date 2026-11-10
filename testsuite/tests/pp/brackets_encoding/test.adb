procedure Test is
   -- Literal brackets: [" and ["123456789"] and ["03A9"]
   A : constant String := "[";
   B : constant String := "]";
   C : constant String := "[""a""]";
   D : constant String := "[""";
   E : constant String := "[1, 2]";
   F : constant Character := '[';
   G : constant Wide_String := "a["03A9"]["0041"]""b";
   H : constant Wide_Character := '["03A9"]';
   I : constant Wide_Wide_String := "["0001F600"]";
   J : constant String := "["E9"]["5B"]["22"]";
   K : constant Wide_String := "["0003a9"]";
   L : constant Wide_Wide_Character := '["0001F600"]';
   M : constant Character := '["E9"]';
   N : constant Wide_Character := '["0003A9"]';
   O : constant String := %[%;
   P : constant Wide_String := %["03A9"]%%[%;
   Q : constant String := "--%[";
   R : constant String := %--[%;
   ["E9"] : Integer := 1;
   ["03A9"] : Integer := 2;
   ["000416"] : Integer := 3;
   ["00010400"] : Integer := 4;
   X["03A9"] : Integer := 0;
begin
   ["E9"] := ["03A9"] + ["000416"] + ["00010400"];
   X["03A9"] := X["03A9"] + 1;
end Test; -- ["
