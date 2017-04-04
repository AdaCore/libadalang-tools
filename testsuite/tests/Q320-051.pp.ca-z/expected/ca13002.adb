--==================================================================--

with Ca13002_0.Ca13002_1.Ca13002_5;     -- Terminal_Driver.VT100.Cursor_Up,
-- implicitly with parent, CA13002_0.
with Ca13002_0.Ca13002_2.Ca13002_5;     -- Terminal_Driver.IBM3270.Cursor_Up.
with Ca13002_0.Ca13002_3;               -- Terminal_Driver.DOS_ANSI.
with Ca13002_0.Ca13002_4;               -- Terminal_Driver.WYSE.
with Report;
use Ca13002_0;                         -- All primitive subprograms directly
-- visible.

procedure Ca13002 is
   Expected_Calls : constant Ca13002_0.Tc_Calls_Arr :=
     ((True, False, False, False),
      (False, True, False, False),
      (False, False, True, False),
      (False, False, False, True));
begin
   Report.Test
     ("CA13002",
      "Check that two library units and/or subunits " &
      "may have the same simple names if they have distinct " &
      "expanded names");

   -- Note that the leaves all have the same name. Call the first grandchild.
   Ca13002_0.Ca13002_1.Ca13002_5;

   -- Call the second grandchild.
   Ca13002_0.Ca13002_2.Ca13002_5;

   -- Call the first subunit.
   Ca13002_0.Ca13002_3.Ca13002_5;

   -- Call the second subunit.
   Ca13002_0.Ca13002_4.Ca13002_5;

   if Tc_Calls /= Expected_Calls then
      Report.Failed ("Wrong result");
   end if;

   Report.Result;

end Ca13002;
