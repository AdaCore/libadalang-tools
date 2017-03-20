     --==================================================================--

with Fc51d00;    -- Generic list abstraction.
with Cc51d02_0;  -- Tagged type declarations.
with Cc51d02_1;  -- Generic operation.

with Report;
procedure Cc51d02 is

   use Cc51d02_0;                                        -- All types & ops
   -- directly visible.

   -- Begin test code declarations: -----------------------

   Tc_Expected_1 : Blind_Id_Type'Class := Blind_Id_Type'(Ssn => "111223333");
   Tc_Expected_2 : Blind_Id_Type'Class :=
     Named_Id_Type'("444556666", "Doe      ");

   Tc_Initial_1 : Blind_Id_Type       := (Ssn => "777889999");
   Tc_Initial_2 : Named_Id_Type       := ("777889999", "Doe      ");
   Tc_Initial_3 : Blind_Id_Type'Class := Tc_Initial_2;

   -- End test code declarations. -------------------------

   package Id_Class_Lists is new Fc51d00 (Blind_Id_Type'Class);

   procedure Update_And_Write is new Cc51d02_1
     (Blind_Id_Type'Class,
      Id_Class_Lists);

   Blind_List  : Id_Class_Lists.List_Type;
   Named_List  : Id_Class_Lists.List_Type;
   Maimed_List : Id_Class_Lists.List_Type;

begin
   Report.Test
     ("CC51D02",
      "Formal private extension, class-wide actual: " &
      "body of primitive subprogram executed is that of actual " &
      "type. Check for subprograms declared in formal package");

   Update_And_Write (Blind_List, Tc_Initial_1);    -- Test root type actual.

   if (Id_Class_Lists.View_Element (1, Blind_List) not in Blind_Id_Type) then
      Report.Failed ("Result for root type actual is not in proper class");
   elsif (Id_Class_Lists.View_Element (1, Blind_List) /= Tc_Expected_1) then
      Report.Failed ("Wrong result for root type actual");
   end if;

   Update_And_Write (Named_List, Tc_Initial_2);    -- Test derived type actual.

   if (Id_Class_Lists.View_Element (1, Named_List) not in Named_Id_Type) then
      Report.Failed ("Result for derived type actual is not in proper class");
   elsif (Id_Class_Lists.View_Element (1, Named_List) /= Tc_Expected_2) then
      Report.Failed ("Wrong result for derived type actual");
   end if;

   -- In the subtest below, an object of a class-wide type (TC_Initial_3) is
   -- passed to Update_and_Write. It has been initialized with an object of
   -- type Named_ID_Type, so the result should be identical to
   -- that of the Named_ID_Type subtest (namely TC_Expected_2). Note that
   -- a new list of Named IDs is used (Maimed_List). This is to assure test
   -- validity, since Named_List has already been updated by a previous
   -- subtest.

   Update_And_Write (Maimed_List, Tc_Initial_3);   -- Test class-wide actual.

   if (Id_Class_Lists.View_Element (1, Maimed_List) not in Named_Id_Type) then
      Report.Failed ("Result for class-wide actual is not in proper class");
   elsif (Id_Class_Lists.View_Element (1, Maimed_List) /= Tc_Expected_2) then
      Report.Failed ("Wrong result for class-wide actual");
   end if;

   Report.Result;
end Cc51d02;
