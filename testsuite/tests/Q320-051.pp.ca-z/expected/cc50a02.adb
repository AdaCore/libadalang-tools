     --==================================================================--

with Fc50a00;   -- Tagged (actual) type declarations.
with Cc50a02_0; -- Generic stack abstraction.
with Cc50a02_1; -- Generic stack testing procedure.

with Report;
procedure Cc50a02 is

   --
   -- Pass a nondiscriminated tagged actual:
   --

   package Count_Stacks is new Cc50a02_0 (Fc50a00.Count_Type);
   procedure Tc_Count_Test is new Cc50a02_1 (Fc50a00.Count_Type, Count_Stacks);
   Count_Stack : Count_Stacks.Stack;

   --
   -- Pass a discriminated tagged actual:
   --

   package Person_Stacks is new Cc50a02_0 (Fc50a00.Person_Type);
   procedure Tc_Person_Test is new Cc50a02_1 (Fc50a00.Person_Type,
      Person_Stacks);
   Person_Stack : Person_Stacks.Stack;

   --
   -- Pass a class-wide actual:
   --

   package People_Stacks is new Cc50a02_0 (Fc50a00.Person_Type'Class);
   procedure Tc_People_Test is new Cc50a02_1 (Fc50a00.Person_Type'Class,
      People_Stacks);
   People_Stack : People_Stacks.Stack;

begin
   Report.Test
     ("CC50A02",
      "Check that tagged actuals may be passed " &
      "to a formal (nontagged) private type");

   Report.Comment ("Testing definite tagged type..");
   Tc_Count_Test (Count_Stack, Fc50a00.Tc_Count_Item);

   Report.Comment ("Testing indefinite tagged type..");
   Tc_Person_Test (Person_Stack, Fc50a00.Tc_Person_Item);

   Report.Comment ("Testing class-wide type..");
   Tc_People_Test (People_Stack, Fc50a00.Tc_Viperson_Item);

   Report.Result;
end Cc50a02;
