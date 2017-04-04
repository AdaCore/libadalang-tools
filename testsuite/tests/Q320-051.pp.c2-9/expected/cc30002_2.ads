     --==================================================================--

with Cc30002_0;
package Cc30002_2 is

   type Aperture_Camera is new Cc30002_0.Camera with record
      Fstop : Natural;
      -- ...Other components.
   end record;

   procedure Set_Shutter_Speed (C : in out Aperture_Camera);
   procedure Focus (C : in out Aperture_Camera);

end Cc30002_2;
