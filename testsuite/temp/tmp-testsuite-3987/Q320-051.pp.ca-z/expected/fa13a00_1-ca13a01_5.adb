--==================================================================--

-- Context clauses required for visibility needed by separate subunit.

with Fa13a00_0;                           -- Building Manager

with Fa13a00_1.Fa13a00_3;                 -- Move Elevator

with Fa13a00_1.Ca13a01_4;                 -- Maintenance Operation (private)

use Fa13a00_0;

package body Fa13a00_1.Ca13a01_5 is

   procedure Emergency is separate;

end Fa13a00_1.Ca13a01_5;
