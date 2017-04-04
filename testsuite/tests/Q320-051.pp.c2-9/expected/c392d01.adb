     --==================================================================--

with F392d00;
with C392d01_0.C392d01_1;

with Report;

procedure C392d01 is
   Zooming_Camera : C392d01_0.Zoom_Camera;
   Auto_Camera1   : C392d01_0.C392d01_1.Auto_Speed;
   Auto_Camera2   : C392d01_0.C392d01_1.Auto_Speed;

   Tc_Expected_Zoom_Depth : constant F392d00.Depth_Of_Field := 83;
   Tc_Expected_Auto_Depth : constant F392d00.Depth_Of_Field := 83;
   Tc_Expected_Depth      : constant F392d00.Depth_Of_Field := 83;
   Tc_Expected_Zoom_Speed : constant F392d00.Shutter_Speed := F392d00.Thousand;
   Tc_Expected_Auto_Speed : constant F392d00.Shutter_Speed := F392d00.Thousand;
   Tc_Expected_Speed : constant F392d00.Shutter_Speed  := F392d00.Two_Fifty;

   use type F392d00.Depth_Of_Field;
   use type F392d00.Shutter_Speed;

begin
   Report.Test
     ("C392D01",
      "Dispatching for overridden and non-overridden " &
      "primitive subprograms: private extension declared in child " &
      "unit, parent is tagged private whose full view is derived  " &
      "type");

-- Call the class-wide operation (Self_Test) for Zoom_Camera'Class, which
-- itself calls the class-wide operation for Remote_Camera'Class, which in
-- turn makes dispatching calls to Focus and Set_Shutter_Speed:

   -- For an object of type Zoom_Camera, the dispatching call to Focus
   -- should dispatch to the body explicitly declared for Zoom_Camera. The
   -- dispatching to Set_Shutter_Speed should dispatch to the body declared
   -- for Remote_Camera:

   C392d01_0.Self_Test (Zooming_Camera);

   if not C392d01_0.Tc_Correct_Result
       (Zooming_Camera,
        Tc_Expected_Zoom_Depth,
        Tc_Expected_Zoom_Speed)
   then
      Report.Failed ("Calls dispatched incorrectly for tagged private type");
   end if;

   -- For an object of type Auto_Speed, the dispatching call to Focus should
   -- dispatch to the body explicitly declared for Zoom_Camera. The dispatching
   -- call to Set_Shutter_Speed should dispatch to the body explicitly declared
   -- for Remote_Camera:

   C392d01_0.Self_Test (Auto_Camera1);

   if not C392d01_0.C392d01_1.Tc_Correct_Result
       (Auto_Camera1,
        Tc_Expected_Auto_Depth,
        Tc_Expected_Auto_Speed)
   then
      Report.Failed ("Calls dispatched incorrectly for private extension");
   end if;

   -- Call to Self_Test from C392D01_0.C392D01_1 invokes the dispatching
   -- call to Focus which should dispatch to the body explicitly declared for
   -- Zoom_Camera. The dispatching call to Set_Shutter_Speed should dispatch
   -- to the body explicitly declared for Auto_Speed:

   C392d01_0.C392d01_1.Self_Test (Auto_Camera2);

   if not C392d01_0.C392d01_1.Tc_Correct_Result
       (Auto_Camera2,
        Tc_Expected_Depth,
        Tc_Expected_Speed)
   then
      Report.Failed ("Call to explicit subprogram executed the wrong body");
   end if;

   Report.Result;

end C392d01;
