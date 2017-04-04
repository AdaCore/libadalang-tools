-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with Tctouch;
package body Cc40001_0.Cc40001_1 is

   procedure Initialize (Cob : in out Object_In_Time) is
   begin
      Cob.Tc_Current_State := Initialized;
      Cob.Birth            := Ada.Calendar.Clock;
   end Initialize;

   procedure Adjust (Cob : in out Object_In_Time) is
   begin
      Cob.Tc_Current_State := Adjusted;
      Tctouch.Touch
        ('a');    ------------------------------------------------ a
      Tctouch.Touch
        (Cob.Id); ------------------------------------------------ ID
   end Adjust;

   procedure Finalize (Cob : in out Object_In_Time) is
   begin
      Cob.Tc_Current_State := Erroneous;
      Finalization_Count   := Finalization_Count + 1;
   end Finalize;

   procedure User_Operation (Cob : in out Object_In_Time; Name : String) is
   begin
      Cc40001_0.User_Operation (Simple_Object (Cob), Name);
      Cob.Activity         := Ada.Calendar.Clock;
      Cob.Tc_Current_State := Reset;
   end User_Operation;

   Tc_Time_Object : Object_In_Time ('g');

end Cc40001_0.Cc40001_1;
