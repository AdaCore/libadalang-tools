------------------------------------------------------------------- CC40001

with Report;
with Tctouch;
with Cc40001_0.Cc40001_1;
with Cc40001_0.Cc40001_2;
with Cc40001_0.Cc40001_3;
with Cc40001_0.Cc40001_4;
procedure Cc40001 is

   function Not_Adjusted (Co : Cc40001_0.Simple_Object) return Boolean is
      use type Cc40001_0.States;
   begin
      return Co.Tc_Current_State /= Cc40001_0.Adjusted;
   end Not_Adjusted;

   function Not_Adjusted
     (Co : Cc40001_0.Cc40001_1.Object_In_Time) return Boolean
   is
      use type Cc40001_0.States;
   begin
      return Co.Tc_Current_State /= Cc40001_0.Adjusted;
   end Not_Adjusted;

   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Subtest 1

   procedure Subtest_1 is
      Object_0 : Cc40001_0.Simple_Object ('T');
      Object_1 : Cc40001_0.Cc40001_1.Object_In_Time ('t');

      package Subtest_1_1 is new Cc40001_0.Cc40001_2
        (Object_0); -- classwide generic formal object

      package Subtest_1_2 is new Cc40001_0.Cc40001_2
        (Object_1); -- classwide generic formal object
   begin
      Tctouch.Flush;  -- clear out all "A" and "T" entries, no further calls
      -- to Touch should occur before the call to Validate

      -- set the objects TC_Current_State to "Reset"
      Cc40001_0.User_Operation (Object_0, "Subtest 1");
      Cc40001_0.Cc40001_1.User_Operation (Object_1, "Subtest 1");

      -- check that the objects TC_Current_State is "Adjusted"
      Subtest_1_1.Tc_Verify_State;
      Subtest_1_2.Tc_Verify_State;

      Tctouch.Validate ("", "No actions should occur here, subtest 1");

   end Subtest_1;

   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Subtest 2

   procedure Subtest_2 is
      Object_0 : Cc40001_0.Simple_Object ('T');
      Object_1 : Cc40001_0.Cc40001_1.Object_In_Time ('t');

      package Subtest_2_1 is -- generic formal object is discriminated private
      new Cc40001_0.Cc40001_3 (Cc40001_0.Simple_Object,
         Object_0, Not_Adjusted);

      package Subtest_2_2 is -- generic formal object is discriminated private
      new Cc40001_0.Cc40001_3
        (Cc40001_0.Cc40001_1.Object_In_Time, Object_1, Not_Adjusted);

   begin
      Tctouch.Flush;  -- clear out all "A" and "T" entries

      -- set the objects state to "Reset"
      Cc40001_0.User_Operation (Object_0, "Subtest 2");
      Cc40001_0.Cc40001_1.User_Operation (Object_1, "Subtest 2");

      Subtest_2_1.Tc_Verify_State;
      Subtest_2_2.Tc_Verify_State;

      Tctouch.Validate ("", "No actions should occur here, subtest 2");

   end Subtest_2;

   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Subtest 3

   procedure Subtest_3 is
      Object_0 : Cc40001_0.Simple_Object ('T');
      Object_1 : Cc40001_0.Cc40001_1.Object_In_Time ('t');

      package Subtest_3_1 is -- generic formal object is discriminated tagged
      new Cc40001_0.Cc40001_4 (Cc40001_0.Simple_Object,
         Object_0, Not_Adjusted);

      package Subtest_3_2 is -- generic formal object is discriminated tagged
      new Cc40001_0.Cc40001_4
        (Cc40001_0.Cc40001_1.Object_In_Time, Object_1, Not_Adjusted);
   begin
      Tctouch.Flush;  -- clear out all "A" and "T" entries

      -- set the objects state to "Reset"
      Cc40001_0.User_Operation (Object_0, "Subtest 3");
      Cc40001_0.Cc40001_1.User_Operation (Object_1, "Subtest 3");

      Subtest_3_1.Tc_Verify_State;
      Subtest_3_2.Tc_Verify_State;

      Tctouch.Validate ("", "No actions should occur here, subtest 3");

   end Subtest_3;

begin  -- Main test procedure.

   Report.Test
     ("CC40001",
      "Check that adjust and finalize are called on " &
      "the constant object created by the " &
      "evaluation of a generic association for a " &
      "formal object of mode in");

   -- check that the created constant objects are properly adjusted and
   -- subsequently finalized

   Cc40001_0.Finalization_Count := 0;

   Subtest_1;

   if Cc40001_0.Finalization_Count < 4 then
      Report.Failed ("Insufficient Finalizations for Subtest 1");
   end if;

   Cc40001_0.Finalization_Count := 0;

   Subtest_2;

   if Cc40001_0.Finalization_Count < 4 then
      Report.Failed ("Insufficient Finalizations for Subtest 2");
   end if;

   Cc40001_0.Finalization_Count := 0;

   Subtest_3;

   if Cc40001_0.Finalization_Count < 4 then
      Report.Failed ("Insufficient Finalizations for Subtest 3");
   end if;

   Report.Result;

end Cc40001;
