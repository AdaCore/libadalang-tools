     --===================================================================--

package body Cc51007_1 is

   procedure Handle (A : in out Low_Alert) is         -- Artificial for
   begin                                             -- testing.
      A.Time_Of_Arrival := Ada.Calendar.Time_Of (1_984, 1, 1);
      A.Message         := "Low Alert!";
   end Handle;

end Cc51007_1;
