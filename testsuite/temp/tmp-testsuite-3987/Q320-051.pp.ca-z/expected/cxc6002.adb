------------------------------------------------------------------- CXC6002

with Report;
with Cxc6002_1;
procedure Cxc6002 is

begin  -- Main test procedure.

   Report.Test
     ("CXC6002",
      "Check semantics for volatile " & "composite types");

   ------------ Subtest 1, Volatile Copy Semantics, Object

   Cxc6002_1.Check_Copy_Semantics (Cxc6002_1.Hoover, Cxc6002_1.Hoover);

   ------------ Subtest 2, Volatile Copy Semantics, Component

   Cxc6002_1.Check_Copy_Semantics (Cxc6002_1.Grid (1), Cxc6002_1.Grid (1));

   Report.Result;

end Cxc6002;
