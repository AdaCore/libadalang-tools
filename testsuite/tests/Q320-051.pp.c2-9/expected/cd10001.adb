------------------------------------------------------------------- CD10001

with Report;
with Cd10001_0;

procedure Cd10001 is

begin  -- Main test procedure.

   Report.Test
     ("CD10001",
      "Check that representation items containing " &
      "nonstatic expressions are supported in the " &
      "case that the representation item is a name " &
      "that statically denotes a constant declared " &
      "before the entity");

   Cd10001_0.Tc_Check_Values;

   Report.Result;

end Cd10001;
