-- Alert_Foundation.Public_Child

--=======================================================================--

with Report;
with F393b00.C393b13_0;
-- Alert_foundation.Public_Child;
procedure C393b13 is
   package Child renames F393b00.C393b13_0;
   Ca : Child.Child_Alert (Child.Message'Length);

begin

   Report.Test
     ("C393B13",
      "Check that an extended type can be derived " & "from an abstract type");

   if Ca.Times_Handled /= 0 then
      Report.Failed ("Wrong initialization");
   end if;

   Child.Handle (Ca);
   if (Ca.Times_Handled /= 1) or (Ca.Msg /= Child.Message) then
      Report.Failed ("Wrong results from Handle");
   end if;

   Report.Result;

end C393b13;
