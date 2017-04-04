     --==================================================================--

with Fc50a00;   -- Tagged (actual) type declarations.
with Cc50a01_0; -- Generic stack abstraction.
with Cc50a01_1; -- Generic stack testing procedure.
with Cc50a01_2;
with Cc50a01_3;

with Report;
procedure Cc50a01 is

   package Count_Stacks renames Cc50a01_2;
   package Person_Stacks renames Cc50a01_3;

   procedure Tc_Count_Test is new Cc50a01_1
     (Fc50a00.Count_Type,
      Fc50a00.Tc_Default_Count,
      Count_Stacks);
   Count_Stack : Count_Stacks.Stack;

   procedure Tc_Person_Test is new Cc50a01_1
     (Fc50a00.Person_Type,
      Fc50a00.Tc_Default_Person,
      Person_Stacks);
   Person_Stack : Person_Stacks.Stack;

begin
   Report.Test
     ("CC50A01",
      "Check that a formal parameter of a " &
      "library-level generic unit may be a formal tagged " &
      "private type");

   Report.Comment ("Testing definite tagged type..");
   Tc_Count_Test (Count_Stack, Fc50a00.Tc_Count_Item);

   Report.Comment ("Testing indefinite tagged type..");
   Tc_Person_Test (Person_Stack, Fc50a00.Tc_Person_Item);

   Report.Result;
end Cc50a01;
