     --==================================================================--

package body C392d02_0 is

   procedure Set_Shutter_Speed (C : in out Auto_Speed;
      Speed                       : in     F392d00.Shutter_Speed)
   is
   begin
      -- Artificial for testing purposes.
      C.Shutter := F392d00.Four_Hundred;
   end Set_Shutter_Speed;

   ----------------------------------------------------
   procedure Self_Test (C : in out Auto_Speed'Class) is
   begin
      -- Should dispatch to the Set_Shutter_Speed explicitly declared for
      -- Auto_Speed.
      Set_Shutter_Speed (C, F392d00.Two_Fifty);
   end Self_Test;

end C392d02_0;
