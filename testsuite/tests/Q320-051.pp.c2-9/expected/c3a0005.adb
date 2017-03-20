-----------------------------------------------------------------------------

with Tctouch;
with Report;

with C3a0005_0;

procedure C3a0005 is

   Big_Red_Button : aliased C3a0005_0.Button;

begin

   Report.Test
     ("C3A0005",
      "Check that access to subprogram may be " &
      "stored within data structures, and that the " &
      "access to subprogram can subsequently be called");

   C3a0005_0.Push (Big_Red_Button'Access);
   Tctouch.Validate ("PD", "Using default value");
   Tctouch.Assert (C3a0005_0.Default_Call, "Default Call");

   -- set Emergency value in Button.Response
   C3a0005_0.Set_Response (Big_Red_Button'Access, C3a0005_0.Emergency'Access);

   C3a0005_0.Push (Big_Red_Button'Access);
   Tctouch.Validate ("SPE", "After set to Emergency value");
   Tctouch.Assert (C3a0005_0.Emergency_Call, "Emergency Call");

   Report.Result;

end C3a0005;
