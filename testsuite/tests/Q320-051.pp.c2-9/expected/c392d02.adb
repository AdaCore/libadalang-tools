     --==================================================================--

with F392d00;
with C392d02_0;

with Report;

procedure C392d02 is
   Basic_Camera : F392d00.Remote_Camera;
   Auto_Camera1 : C392d02_0.Auto_Speed;
   Auto_Camera2 : C392d02_0.Auto_Speed;

   Tc_Expected_Basic_Speed : constant F392d00.Shutter_Speed :=
     F392d00.Thousand;
   Tc_Expected_Speed : constant F392d00.Shutter_Speed := F392d00.Four_Hundred;

   use type F392d00.Shutter_Speed;

begin
   Report.Test
     ("C392D02",
      "Dispatching for non-overridden primitive " &
      "subprograms: record extension declared in non-child " &
      "package, parent is tagged record");

-- Call the class-wide operation for Remote_Camera'Class, which dispatches
-- to Set_Shutter_Speed:

   -- For an object of type Remote_Camera, the dispatching call should
   -- dispatch to the body declared for the root type:

   F392d00.Self_Test (Basic_Camera);

   if Basic_Camera.Shutter /= Tc_Expected_Basic_Speed then
      Report.Failed ("Call dispatched incorrectly for root type");
   end if;

   -- C392D02_0.Set_Shutter_Speed should never be called by F392D00.Self_Test,
   -- since C392D02_0.Set_Shutter_Speed does not override
   -- F392D00.Set_Shutter_Speed.

   -- For an object of type Auto_Speed, the dispatching call should
   -- also dispatch to the body declared for the root type:

   F392d00.Self_Test (Auto_Camera1);

   if Auto_Camera1.Shutter /= Tc_Expected_Basic_Speed then
      Report.Failed ("Call dispatched incorrectly for derived type");
   end if;

   -- Call to Self_Test from C392D02_0 invokes the dispatching call to
   -- Set_Shutter_Speed which should dispatch to the body explicitly declared
   -- for Auto_Speed:

   C392d02_0.Self_Test (Auto_Camera2);

   if Auto_Camera2.Shutter /= Tc_Expected_Speed then
      Report.Failed ("Call to explicit subprogram executed the wrong body");
   end if;

   Report.Result;

end C392d02;
