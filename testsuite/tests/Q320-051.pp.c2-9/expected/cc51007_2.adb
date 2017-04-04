     --===================================================================--

with Ada.Calendar;
package body Cc51007_2 is

   procedure Handle (A : in out Medium_Alert) is      -- Artificial for
   begin                                             -- testing.
      A.Action_Officer  := Co;
      A.Time_Of_Arrival := Ada.Calendar.Time_Of (2_001, 1, 1);
      A.Message         := "Med Alert!";
   end Handle;

end Cc51007_2;
