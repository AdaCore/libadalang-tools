     --==================================================================--

with Fc51d00;    -- Generic list abstraction.
with Cc51d01_0;  -- Tagged type declarations.
with Cc51d01_1;  -- Generic operation.

with Report;
procedure Cc51d01 is

   use Cc51d01_0;                                        -- All types & ops
   -- directly visible.

   -- Begin test code declarations: -----------------------

   Tc_Expected_1 : Blind_Id_Type  := (Ssn => "111223333");
   Tc_Expected_2 : Named_Id_Type  := ("444556666", "Doe      ");
   Tc_Expected_3 : Ranked_Id_Type := ("444556666", "Doe      ", 0);

   Tc_Initial_1 : Blind_Id_Type  := (Ssn => "777889999");
   Tc_Initial_2 : Named_Id_Type  := ("777889999", "Doe      ");
   Tc_Initial_3 : Ranked_Id_Type := ("777889999", "Doe      ", 0);

   -- End test code declarations. -------------------------

   -- Begin instantiations and list declarations: ---------

   -- At this point in an application, the generic list package would be
   -- instantiated for one of the visible tagged types. Next, the generic
   -- subprogram would be instantiated for the same tagged type and the
   -- preceding list package instance.
   --
   -- In order to cover all the important cases, this test instantiates several
   -- packages and subprograms (probably more than would typically appear
   -- in user code).

   -- Support for lists of blind IDs:

   package Blind_Lists is new Fc51d00 (Blind_Id_Type);
   procedure Update_And_Write is new Cc51d01_1 (Blind_Id_Type, Blind_Lists);
   Blind_List : Blind_Lists.List_Type;

   -- Support for lists of named IDs:

   package Named_Lists is new Fc51d00 (Named_Id_Type);
   procedure Update_And_Write is new                     -- Overloads subprog
   Cc51d01_1
     (Elem_Type => Named_Id_Type,              -- for Blind_ID_Type.
      List_Mgr  => Named_Lists);
   Named_List : Named_Lists.List_Type;

   -- Support for lists of ranked IDs:

   package Ranked_Lists is new Fc51d00 (Ranked_Id_Type);
   procedure Update_And_Write is new                     -- Overloads.
   Cc51d01_1
     (Elem_Type => Ranked_Id_Type,
      List_Mgr  => Ranked_Lists);
   Ranked_List : Ranked_Lists.List_Type;

-- End instantiations and list declarations. -----------

begin
   Report.Test
     ("CC51D01",
      "Formal private extension, specific tagged " &
      "type actual: body of primitive subprogram executed is " &
      "that of actual type. Check for subprograms declared in " &
      "a formal package");

   Update_And_Write (Blind_List, Tc_Initial_1);

   if (Blind_Lists.View_Element (1, Blind_List) /= Tc_Expected_1) then
      Report.Failed ("Wrong result for root tagged type");
   end if;

   Update_And_Write (Named_List, Tc_Initial_2);

   if (Named_Lists.View_Element (1, Named_List) /= Tc_Expected_2) then
      Report.Failed ("Wrong result for type derived directly from root");
   end if;

   Update_And_Write (Ranked_List, Tc_Initial_3);

   if (Ranked_Lists.View_Element (1, Ranked_List) /= Tc_Expected_3) then
      Report.Failed ("Wrong result for type derived indirectly from root");
   end if;

   Report.Result;
end Cc51d01;
