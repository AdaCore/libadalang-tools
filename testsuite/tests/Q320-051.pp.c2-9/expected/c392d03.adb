     --==================================================================--

with F392d00;
with C392d03_0;

with Report;

procedure C392d03 is

   type Focus_Ptr is access procedure (P1 : in out C392d03_0.Auto_Focus;
      P2                                  : in     F392d00.Depth_Of_Field);

   Basic_Camera   : F392d00.Remote_Camera;
   Auto_Camera1   : C392d03_0.Auto_Focus;
   Auto_Camera2   : C392d03_0.Auto_Focus;
   Flash_Camera1  : C392d03_0.Auto_Flashing;
   Flash_Camera2  : C392d03_0.Auto_Flashing;
   Special_Camera : C392d03_0.Special_Focus;
   Auto_Depth     : F392d00.Depth_Of_Field := 78;

   Tc_Expected_Basic_Depth : constant F392d00.Depth_Of_Field := 46;
   Tc_Expected_Auto_Depth  : constant F392d00.Depth_Of_Field := 52;
   Tc_Expected_Depth       : constant F392d00.Depth_Of_Field := 91;

   Fp : Focus_Ptr := C392d03_0.Focus'Access;

   use type F392d00.Depth_Of_Field;

begin
   Report.Test
     ("C392D03",
      "Dispatching for overridden primitive " &
      "subprograms: record extension declared in non-child " &
      "package, parent is tagged record");

-- Call the class-wide operation for Remote_Camera'Class, which itself makes a
-- dispatching call to Focus:

   -- For an object of type Remote_Camera, the dispatching call should dispatch
   -- to the body declared for the root type:

   F392d00.Self_Test (Basic_Camera);

   if Basic_Camera.Dof /= Tc_Expected_Basic_Depth then
      Report.Failed ("Call dispatched incorrectly for root type");
   end if;

   -- For an object of type Auto_Focus, the dispatching call should dispatch to
   -- the body declared for the derived type:

   F392d00.Self_Test (Auto_Camera1);

   if Auto_Camera1.Dof /= Tc_Expected_Auto_Depth then
      Report.Failed ("Call dispatched incorrectly for Auto_Focus type");
   end if;

   -- For an object of type Auto_Flash, the dispatching call should also
   -- dispatch to the body declared for the derived type:

   F392d00.Self_Test (Flash_Camera1);

   if Flash_Camera1.Dof /= Tc_Expected_Depth then
      Report.Failed ("Call dispatched incorrectly for Auto_Flash type");
   end if;

   -- For an object of Auto_Flash type, a non-dispatching call to Focus should
   -- execute the body declared for the derived type (even through it is
   -- declared in the private part).

   C392d03_0.Focus (Flash_Camera2, Auto_Depth);

   if Flash_Camera2.Dof /= Tc_Expected_Depth then
      Report.Failed
        ("Non-dispatching call to privately overriding " &
         "subprogram executed the wrong body");
   end if;

   -- For an object of Auto_Focus type, a non-dispatching call to Focus should
   -- execute the body declared for the derived type (even through it is
   -- declared in the private part).

   Fp.all (Auto_Camera2, Auto_Depth);

   if Auto_Camera2.Dof /= Tc_Expected_Auto_Depth then
      Report.Failed
        ("Non-dispatching call by using access to overriding " &
         "subprogram executed the wrong body");
   end if;

   -- For an object of type Special_Camera, the dispatching call should also
   -- dispatch to the body declared for the derived type:

   F392d00.Self_Test (Special_Camera);

   if Special_Camera.Dof /= Tc_Expected_Auto_Depth then
      Report.Failed ("Call dispatched incorrectly for Special_Camera type");
   end if;

   Report.Result;

end C392d03;
