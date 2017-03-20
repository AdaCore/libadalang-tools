--==================================================================--

with Fa13a00_1.Ca13a01_5;                 -- Emergency Operation
-- implicitly with Basic Elevator
-- Operations

with Fa13a00_1.Ca13a01_6;                 -- Express Operation

with Report;

procedure Ca13a01 is

begin

   Report.Test
     ("CA13A01",
      "Check that subunits declared in non-generic " &
      "child units of a public parent have the same visibility " &
      "into its parent, its parent's siblings, and packages on " &
      "which its parent depends");

   -- Go to Penthouse.

   Fa13a00_1.Ca13a01_6;

   -- Call emergency operation.

   Fa13a00_1.Ca13a01_5.Emergency;

   if not Fa13a00_1.Tc_Operation then
      Report.Failed ("Incorrect elevator operation");
   end if;

   Report.Result;

end Ca13a01;
