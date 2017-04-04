     --==================================================================--

package body C392005_0 is

   procedure Focus (Cam : in out Remote_Camera; Depth : in Depth_Of_Field) is
   begin
      -- Artificial for testing purposes.
      Cam.Dof := 46;
   end Focus;

   -----------------------------------------------------------
   procedure Set_Shutter_Speed
     (C     : in out Remote_Camera;
      Speed : in     Shutter_Speed)
   is
   begin
      -- Artificial for testing purposes.
      C.Shutter := Thousand;
   end Set_Shutter_Speed;

   -----------------------------------------------------------
   function Set_Aperture (C : Remote_Camera) return Aperture is
   begin
      -- Artificial for testing purposes.
      return Thirty_Two;
   end Set_Aperture;

   -----------------------------------------------------------
   procedure Self_Test (C : in out Remote_Camera'Class) is
      Tc_Dummy_Depth : constant Depth_Of_Field := 23;
      Tc_Dummy_Speed : constant Shutter_Speed  := Four_Hundred;
   begin

      -- Test focus at various depths:
      Focus (C, Tc_Dummy_Depth);
      -- ...Additional calls to Focus.

      -- Test various shutter speeds:
      Set_Shutter_Speed (C, Tc_Dummy_Speed);
      -- ...Additional calls to Set_Shutter_Speed.

   end Self_Test;

   -----------------------------------------------------------
   function Tc_Get_Depth (C : Remote_Camera) return Depth_Of_Field is
   begin
      return C.Dof;
   end Tc_Get_Depth;

   -----------------------------------------------------------
   function Tc_Get_Speed (C : Remote_Camera) return Shutter_Speed is
   begin
      return C.Shutter;
   end Tc_Get_Speed;

end C392005_0;
