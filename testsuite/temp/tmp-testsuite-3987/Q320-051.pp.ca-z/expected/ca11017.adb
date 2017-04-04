--=================================================================--

with Report;

with Ca11017_0.Ca11017_1;   -- Explicit with public child package,
-- implicit with parent package (CA11017_0).

procedure Ca11017 is

   package String_Pkg renames Ca11017_0;
   use String_Pkg;

begin

   Report.Test
     ("CA11017",
      "Check that body of the parent package can " &
      "depend on one of its own public children");

-- Both input strings have the same size. Replace the first string by the
-- second string.

   Replace_Subtest : declare
      The_First_String, The_Second_String : String_Rec (16);
      -- Parent's private type.
      The_Position : Positive := 1;
   begin
      Ca11017_1.Copy ("This is the time", To_The_String => The_First_String);

      Ca11017_1.Copy ("For all good men", The_Second_String);

      Replace (The_First_String, The_Position, The_Second_String);

      -- Compare results using function from public child since
      -- the type is private.

      if not Ca11017_1.Same_Content (The_First_String, The_Second_String) then
         Report.Failed ("Incorrect results");
      end if;

   end Replace_Subtest;

-- During processing, the application may erroneously attempt to replace
-- strings of different size. This would result in the raising of an
-- exception.

   Exception_Subtest : declare
      The_First_String : String_Rec (17);
      -- Parent's private type.
      The_Second_String : String_Rec (13);
      -- Parent's private type.
      The_Position : Positive := 2;
   begin
      Ca11017_1.Copy (" ACVC Version 2.0", The_First_String);

      Ca11017_1.Copy
        (From_The_Substring => "ACVC 9X Basic",
         To_The_String      => The_Second_String);

      Replace (The_First_String, The_Position, The_Second_String);

      Report.Failed ("Exception was not raised");

   exception
      when Ca11017_1.Position_Error =>
         Report.Comment ("Exception is raised as expected");

   end Exception_Subtest;

   Report.Result;

end Ca11017;
