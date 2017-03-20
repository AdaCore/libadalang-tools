--==================================================================--

with Report;
with Cde0001_0.Cde0001_4;

procedure Cde0001 is

begin

   Report.Test
     ("CDE0001",
      "Check that the name of the private type, a " &
      "name that denotes a subtype of the private type, or a " &
      "name that denotes a composite type with a subcomponent " &
      "of a private type can be used in the declaration of a " &
      "generic formal type parameter without causing freezing " &
      "of the named type");

   if not Cde0001_0.Cde0001_4.Verify_Objects then
      Report.Failed ("Wrong values for formal objects");
   end if;

   if not Cde0001_0.Cde0001_4.Verify_Arrays then
      Report.Failed ("Wrong values for formal array types");
   end if;

   if not Cde0001_0.Cde0001_4.Verify_Access then
      Report.Failed ("Wrong values for formal access types");
   end if;

   Report.Result;

end Cde0001;
