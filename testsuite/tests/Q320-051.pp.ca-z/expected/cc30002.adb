     --==================================================================--

with Cc30002_0;
with Cc30002_1;
with Cc30002_2;
with Cc30002_3; -- Instance.

with Report;
procedure Cc30002 is

   package Speed_Cameras renames Cc30002_3;

   use Cc30002_0;

   Tc_Camera1 : Speed_Cameras.Speed_Camera;
   Tc_Camera2 : Speed_Cameras.Speed_Camera'Class := Tc_Camera1;
   Tc_Camera3 : Speed_Cameras.Speed_Camera;
   Tc_Camera4 : Speed_Cameras.Speed_Camera;

begin
   Report.Test
     ("CC30002",
      "Check that an explicit declaration in the " &
      "private part of an instance does not override an implicit " &
      "declaration in the instance, unless the corresponding " &
      "explicit declaration in the generic overrides a " &
      "corresponding implicit declaration in the generic. Check " &
      "for primitive subprograms of tagged types");

--
-- Check non-dispatching calls outside instance:
--

   -- Non-overriding primitive operation:

   Speed_Cameras.Set_Shutter_Speed (Tc_Camera1);
   if Tc_Camera1.Tc_Shutter_Called /= Body_Of_Actual then
      Report.Failed
        ("Wrong body executed: non-dispatching call to " &
         "Set_Shutter_Speed outside instance");
   end if;

   -- Overriding primitive operation:

   Speed_Cameras.Focus (Tc_Camera1);
   if Tc_Camera1.Tc_Focus_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: non-dispatching call to " &
         "Focus outside instance");
   end if;

--
-- Check dispatching calls outside instance:
--

   -- Non-overriding primitive operation:

   Speed_Cameras.Set_Shutter_Speed (Tc_Camera2);
   if Tc_Camera2.Tc_Shutter_Called /= Body_Of_Actual then
      Report.Failed
        ("Wrong body executed: dispatching call to " &
         "Set_Shutter_Speed outside instance");
   end if;

   -- Overriding primitive operation:

   Speed_Cameras.Focus (Tc_Camera2);
   if Tc_Camera2.Tc_Focus_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: dispatching call to " &
         "Focus outside instance");
   end if;

--
-- Check non-dispatching calls within instance:
--

   Speed_Cameras.Self_Test_Nondisp (Tc_Camera3);

   -- Non-overriding primitive operation:

   if Tc_Camera3.Tc_Shutter_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: non-dispatching call to " &
         "Set_Shutter_Speed inside instance");
   end if;

   -- Overriding primitive operation:

   if Tc_Camera3.Tc_Focus_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: non-dispatching call to " &
         "Focus inside instance");
   end if;

--
-- Check dispatching calls within instance:
--

   Speed_Cameras.Self_Test_Disp (Tc_Camera4);

   -- Non-overriding primitive operation:

   if Tc_Camera4.Tc_Shutter_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: dispatching call to " &
         "Set_Shutter_Speed inside instance");
   end if;

   -- Overriding primitive operation:

   if Tc_Camera4.Tc_Focus_Called /= Body_In_Instance then
      Report.Failed
        ("Wrong body executed: dispatching call to " &
         "Focus inside instance");
   end if;

   Report.Result;
end Cc30002;
