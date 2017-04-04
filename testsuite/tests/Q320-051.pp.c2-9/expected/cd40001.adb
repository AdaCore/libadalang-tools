--==================================================================--

with Report;
with Cd40001_0;

procedure Cd40001 is

begin  -- Main test procedure.

   Report.Test
     ("CD40001",
      "Check that Enumeration_Representation_Clauses " &
      "are supported for codes in the range " &
      "System.Min_Int..System.Max_Int");

   Cd40001_0.Tc_Check_Press;

   Cd40001_0.Tc_Check_Add;

   Cd40001_0.Tc_Check_Minus;

   Report.Result;

end Cd40001;
