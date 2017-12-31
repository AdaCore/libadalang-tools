     --==================================================================--

with Fc70c00_0;  -- Generic list type abstraction.
with Fc70c00_1;  -- Generic list operations.
with Cc70c02_0;  -- Additional generic list operations.

with Report;
procedure Cc70c02 is

   type Points is range 0 .. 100;                     -- Discrete type.

   package Lists_Of_Points is new Fc70c00_0 (Points); -- Points lists.

   package Basic_Point_Ops is new                     -- Basic points-list ops.
   Fc70c00_1 (Lists_Of_Points);

   package Points_List_Ops is new                     -- More points-list ops.
   Cc70c02_0 (Lists  => Lists_Of_Points,
      Basic_List_Ops => Basic_Point_Ops);

   Scores : Lists_Of_Points.List_Type;                -- List of points.

   -- Begin test code declarations: -----------------------

   type Tc_Score_Array is array (1 .. 3) of Points;

   Tc_List_Values : constant Tc_Score_Array := (23, 15, 0);

   Tc_Correct_List_Values : Boolean := False;

   procedure Tc_Initialize_List (L : in out Lists_Of_Points.List_Type) is
   begin                                  -- Initial list contains 3 scores
      for I in Tc_Score_Array'Range loop  -- with the values 23, 15, and 0.
         Points_List_Ops.Add_Element (L, Tc_List_Values (I));
      end loop;
   end Tc_Initialize_List;

   procedure Tc_Verify_List (L : in out Lists_Of_Points.List_Type;
      Expected                 : in     Tc_Score_Array; Ok : out Boolean)
   is
      Actual : Tc_Score_Array;
   begin
      Basic_Point_Ops.Reset (L);
      for I in Tc_Score_Array'Range loop
         Points_List_Ops.Read_Element (L, Actual (I));
      end loop;
      Ok := (Actual = Expected);
   end Tc_Verify_List;

-- End test code declarations. -------------------------

begin

   Report.Test
     ("CC70C02",
      "Check that a generic formal package may be " &
      "passed as an actual to another formal package");

   Tc_Initialize_List (Scores);
   Tc_Verify_List (Scores, Tc_List_Values, Tc_Correct_List_Values);

   if not Tc_Correct_List_Values then
      Report.Failed ("List contains incorrect values");
   end if;

   Report.Result;

end Cc70c02;
