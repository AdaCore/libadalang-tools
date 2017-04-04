--==================================================================--

-- Context clauses required for visibility needed by separate subunit.

with Fa13a00_0;                           -- Building Manager

with Fa13a00_1.Fa13a00_2;                 -- Floor Calculation (private)

with Fa13a00_1.Fa13a00_3;                 -- Move Elevator

use Fa13a00_0;

procedure Fa13a00_1.Ca13a01_6 is          -- Express Operation

   -- Other type definitions in real application.

   procedure Goto_Penthouse is separate;

begin
   Goto_Penthouse;

end Fa13a00_1.Ca13a01_6;
