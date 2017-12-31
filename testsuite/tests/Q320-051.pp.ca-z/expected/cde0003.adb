--==================================================================--

with Report;
with Cde0003_1;
procedure Cde0003 is

begin
   Report.Test
     ("CDE0003",
      "Check that actuals for generic formal " &
      "types and for formal subprograms with formal untagged " &
      "incomplete parameters do not need to be frozen at " & "instantiation.");

   if Cde0003_1.Pkg.Call_F (Cde0003_1.Z) /= Cde0003_1.F (Cde0003_1.Z)
     or else Cde0003_1.Pkg.Call_F (Cde0003_1.C) /= Cde0003_1.F (Cde0003_1.C)
   then
      Report.Failed
        ("Wrong result for generic function call with incomplete type");
   end if;

   Cde0003_1.Pkg.Call_P (Cde0003_1.C);

   if not Cde0003_1.Success then
      Report.Failed
        ("Wrong result for generic procedure call with incomplete type");
   end if;

   Report.Result;
end Cde0003;
