-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with Tctouch;
package body Cc40001_0 is

   procedure User_Operation (Cob : in out Simple_Object; Name : String) is
   begin
      Cob.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end User_Operation;

   procedure Initialize (Cob : in out Simple_Object) is
   begin
      Cob.Tc_Current_State := Initialized;
   end Initialize;

   procedure Adjust (Cob : in out Simple_Object) is
   begin
      Cob.Tc_Current_State := Adjusted;
      Tctouch.Touch
        ('A');  -------------------------------------------------- A
      Tctouch.Touch
        (Cob.Id); ------------------------------------------------ ID
      -- note that the calls to touch will not be directly validated, it is
      -- expected that some number > 0 of calls will be made to this procedure,
      -- the subtests then clear (Flush) the Touch buffer and perform actions
      -- where an incorrect implementation might call this procedure. Such a
      -- call will fail on the attempt to "Validate" the null string.
   end Adjust;

   procedure Finalize (Cob : in out Simple_Object) is
   begin
      Cob.Tc_Current_State := Erroneous;
      Finalization_Count   := Finalization_Count + 1;
   end Finalize;

   Tc_Global_Object : Simple_Object ('G');

end Cc40001_0;
