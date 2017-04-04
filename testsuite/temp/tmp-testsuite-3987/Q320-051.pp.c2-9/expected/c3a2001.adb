----------------------------------------------------------------------------

with Report;
with Tctouch;
with C3a2001_5;
procedure C3a2001 is

begin  -- Main test procedure.

   Report.Test
     ("C3A2001",
      "Check that an abstract type can be declared " &
      "and used.  Check actual subprograms dispatch correctly");

   -- This Validate call must be _after_ the call to Report.Test
   Tctouch.Validate ("cgcicc", "Adding");

   C3a2001_5.Flipper (C3a2001_5.Short);
   Tctouch.Validate ("jbdbdbdb", "Flipping");

   C3a2001_5.Tripper (C3a2001_5.Short);
   Tctouch.Validate ("kbfbeee", "Tripping");

   C3a2001_5.Restore (C3a2001_5.Short);
   Tctouch.Validate ("lbfbfbfb", "Restoring");

   C3a2001_5.Failure (C3a2001_5.Short);
   Tctouch.Validate ("mbafbaa", "Circuits Failing");

   Report.Result;

end C3a2001;
