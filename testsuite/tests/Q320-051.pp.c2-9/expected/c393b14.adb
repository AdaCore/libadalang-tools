-- Alert_Foundation.Public_Child

--=======================================================================--

with Report;
with F393b00.C393b14_1;
procedure C393b14 is
-- Alert_Foundation.Public_Child;

begin
   Report.Test
     ("C393B14",
      "Check that an extended type can be derived " & "from an abstract type");

   F393b00.C393b14_1.Init;
   if not F393b00.C393b14_1.Check_Before then
      Report.Failed ("Wrong initialization");
   end if;

   F393b00.C393b14_1.Modify;
   if not F393b00.C393b14_1.Check_After then
      Report.Failed ("Wrong results from Handle");
   end if;

   Report.Result;
end C393b14;
