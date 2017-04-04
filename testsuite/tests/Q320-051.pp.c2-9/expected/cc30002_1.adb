     --==================================================================--

package body Cc30002_1 is

   procedure Self_Test_Nondisp (C : in out Speed_Camera) is
   begin
      -- Nondispatching calls:
      Focus (C);
      Set_Shutter_Speed (C);
   end Self_Test_Nondisp;

   procedure Self_Test_Disp (C : in out Speed_Camera'Class) is
   begin
      -- Dispatching calls:
      Focus (C);
      Set_Shutter_Speed (C);
   end Self_Test_Disp;

   procedure Set_Shutter_Speed (C : in out Speed_Camera) is
   begin
      -- Artificial for testing purposes.
      C.Tc_Shutter_Called := Body_In_Instance;
   end Set_Shutter_Speed;

   procedure Focus (C : in out Speed_Camera) is
   begin
      -- Artificial for testing purposes.
      C.Tc_Focus_Called := Body_In_Instance;
   end Focus;

end Cc30002_1;
