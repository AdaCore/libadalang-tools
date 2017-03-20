     --==================================================================--

package body C392005_0.C392005_1 is

   procedure Focus (C : in out Auto_Speed; Depth : in Depth_Of_Field) is
   begin
      -- Artificial for testing purposes.
      C.Dof := 57;
   end Focus;

   ---------------------------------------------------------------
   procedure Set_Shutter_Speed
     (C     : in out Auto_Speed;
      Speed : in     Shutter_Speed)
   is
   begin
      -- Artificial for testing purposes.
      C.Shutter := Two_Fifty;
   end Set_Shutter_Speed;

   -----------------------------------------------------------
   function Set_Aperture (C : Auto_Speed) return Aperture is
   begin
      -- Artificial for testing purposes.
      return Sixteen;
   end Set_Aperture;

   -----------------------------------------------------------
   function Tc_Get_Aper (C : New_Camera) return Aperture is
   begin
      return C.Aper;
   end Tc_Get_Aper;

end C392005_0.C392005_1;
