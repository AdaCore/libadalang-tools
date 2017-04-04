-----------------------------------------------------------------------------

with Report;
with Tctouch;

with C3a0007_0;
with C3a0007_1;
with C3a0007_2;
procedure C3a0007 is

   Pink_Button  : C3a0007_0.Button;
   Green_Button : C3a0007_1.Priority_Button;

begin

   Report.Test
     ("C3A0007",
      "Check that a call to a subprogram via an " &
      "access-to-subprogram value stored in a data " &
      "structure will correctly dispatch according to " &
      "the tag of the class-wide parameter passed " &
      "via that call");

   -- Call inherited operation Push to set Default_Response value in the
   -- extension.

   C3a0007_1.Push (Green_Button);
   Tctouch.Validate ("PCd", "First Green Button Push");

   Tctouch.Assert_Not
     (C3a0007_0.Default_Call,
      "Incorrect Green Default_Response");

   C3a0007_0.Push (Pink_Button);
   Tctouch.Validate ("PCD", "First Pink Button Push");

   -- Call inherited operations Set_Response and Push to set Emergency value in
   -- the extension.
   C3a0007_1.Set_Response (Green_Button, C3a0007_2.Emergency'Access);
   C3a0007_1.Push (Green_Button);
   Tctouch.Validate ("SPE", "Second Green Button Push");

   Tctouch.Assert (C3a0007_2.Emergency_Call, "Incorrect Green Emergency");

   C3a0007_0.Set_Response (Pink_Button, C3a0007_2.Emergency'Access);
   C3a0007_0.Push (Pink_Button);
   Tctouch.Validate ("SPE", "Second Pink Button Push");

   -- Call primitive operation to set priority value in the extension.
   C3a0007_1.Set_Priority (Green_Button);
   Tctouch.Validate ("s", "Green Button Priority");

   Tctouch.Assert (Green_Button.Priority = 1, "Incorrect Set_Priority");

   Report.Result;

end C3a0007;
