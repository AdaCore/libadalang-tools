with Ada.Text_IO; use Ada.Text_IO;

procedure Playground_1 is
   Buf : String := "Hello ...";

   Full_Name : String := "John Smith";
begin
   Buf (7 .. 9) := "Bob";
   --  Careful! This works because the string on the right side is the
   --  same length as the replaced slice!

   Put_Line (Buf);  --  Prints "Hello Bob"

   Put_Line ("Hi " & Full_Name (1 .. 4)); --  Prints "Hi John"
end Playground_1;

procedure Playground_1_X is
   Buf : String := "Hello ...";

   Full_Name : String := "John Smith";
begin
   Buf (7 .. 9) := "Bob";
   --  Careful! This works because the string on the right side is the
   --  same length as the replaced slice!

   Put_Line (Buf);  --  Prints "Hello Bob"

   Put_Line ("Hi " & Full_Name (1 .. 4)); --  Prints "Hi John"
end Playground_1_X;

procedure Playground_1_Y is
   Buf : String := "Hello ...";

   Full_Name : String := "John Smith";
begin
   Buf (7 .. 9) := "Bob";
   --  Careful! This works because the string on the right side is the
   --  same length as the replaced slice!

   Put_Line (Buf);  --  Prints "Hello Bob"

   Put_Line ("Hi " & Full_Name (1 .. 4)); --  Prints "Hi John"
end Playground_1_Y;



