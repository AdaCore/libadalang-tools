     --==================================================================--

package body C392d01_0 is

   procedure Focus (C : in out Zoom_Camera; Depth : in F392d00.Depth_Of_Field)
   is
   begin
      -- Artificial for testing purposes.
      C.Dof := 83;
   end Focus;

   -----------------------------------------------------------
   -- Indirect call to F392D00.Self_Test since the main does not know that
   -- Zoom_Camera is a private extension of F392D00.Basic_Camera.
   procedure Self_Test (C : in out Zoom_Camera'Class) is
   begin
      F392d00.Self_Test (C);
      -- ...Additional self-testing.
   end Self_Test;

   -----------------------------------------------------------
   function Tc_Correct_Result
     (C : Zoom_Camera; D : F392d00.Depth_Of_Field; S : F392d00.Shutter_Speed)
      return Boolean
   is
      use type F392d00.Depth_Of_Field;
      use type F392d00.Shutter_Speed;
   begin
      return (C.Dof = D and C.Shutter = S);
   end Tc_Correct_Result;

end C392d01_0;
