     --==================================================================--

package body C392d01_0.C392d01_1 is

   procedure Set_Shutter_Speed
     (C     : in out Auto_Speed;
      Speed : in     F392d00.Shutter_Speed)
   is
   begin
      -- Artificial for testing purposes.
      C.Shutter := F392d00.Two_Fifty;
   end Set_Shutter_Speed;

   -------------------------------------------------------
   procedure Self_Test (C : in out Auto_Speed'Class) is
   begin
      -- Artificial for testing purposes.
      Set_Shutter_Speed (C, F392d00.Thousand);
      Focus (C, 27);
   end Self_Test;

end C392d01_0.C392d01_1;
