--------------------------------------------------------------------------

with Report;
with Cxc6001_0;
with Cxc6001_1;
procedure Cxc6001 is

begin  -- Main test procedure.

   Report.Test
     ("CXC6001", "Check semantics for atomic and volatile " & "types");

   ------------ Subtest 1, Atomic Reference Semantics, Object

   Cxc6001_0.Check_Reference_Semantics (Cxc6001_0.Plutonium);

   ------------ Subtest 2, Atomic Reference Semantics, Component

   Cxc6001_0.Check_Reference_Semantics (Cxc6001_0.Water);

   ------------ Subtest 3, Volatile Reference Semantics, Object

   Cxc6001_1.Check_Reference_Semantics (Cxc6001_1.Chlorine);

   ------------ Subtest 4, Volatile Reference Semantics, Component

   Cxc6001_1.Check_Reference_Semantics (Cxc6001_1.Smog);

   Report.Result;

end Cxc6001;
