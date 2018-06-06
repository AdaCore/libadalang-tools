     --==================================================================--

with Fc70b00;    -- Generic list abstraction.
with Cc70b02_0;  -- Generic discrete type operations.
with Cc70b02_1;  -- Generic discrete list operations.

with Report;
procedure Cc70b02 is

   type Points is range 0 .. 100;                   -- Discrete type.

   package Points_Ops is new Cc70b02_0 (Points);    -- Points-type operations.
   package Lists_Of_Points is new Fc70b00 (Points); -- Points lists.
   package Points_List_Ops is new                   -- Points-list operations.
   Cc70b02_1 (Points_Ops, Lists_Of_Points);

   Scores : Lists_Of_Points.List_Type;              -- List of points.

   -- Begin test code declarations: -----------------------

   type Tc_Score_Array is array (1 .. 3) of Points;

   Tc_Initial_Values : constant Tc_Score_Array := (23, 15, 0);
   Tc_Final_Values   : constant Tc_Score_Array := (46, 30, 0);

   Tc_Correct_Initial_Values : Boolean := False;
   Tc_Correct_Final_Values   : Boolean := False;

   procedure Tc_Initialize_List (L : in out Lists_Of_Points.List_Type) is
   begin                                  -- Initial list contains 3 scores
      for I in Tc_Score_Array'Range loop  -- with the values 23, 15, and 0.
         Lists_Of_Points.Add_Element (L, Tc_Initial_Values (I));
      end loop;
   end Tc_Initialize_List;

   procedure Tc_Verify_List
     (L  : in out Lists_Of_Points.List_Type; Expected : in Tc_Score_Array;
      Ok :    out Boolean)
   is
      Actual : Tc_Score_Array;
   begin                                  -- Verify that all scores have been
      Lists_Of_Points.Reset (L);          -- set to zero.
      for I in Tc_Score_Array'Range loop
         Lists_Of_Points.Read_Element (L, Actual (I));
      end loop;
      Ok := (Actual = Expected);
   end Tc_Verify_List;

   -- End test code declarations. -------------------------

begin
   Report.Test
     ("CC70B02",
      "Check that a library-level generic package " &
      "may have a formal package as a formal parameter, and that " &
      "the generic formal actual part may specify explicit actual " &
      "parameters (including a formal parameter of a previously " &
      "declared formal package). Check that a use clause is legal " &
      "in the generic formal part");

   Tc_Initialize_List (Scores);
   Tc_Verify_List (Scores, Tc_Initial_Values, Tc_Correct_Initial_Values);

   if not Tc_Correct_Initial_Values then
      Report.Failed ("List contains incorrect initial values");
   end if;

   Points_List_Ops.Double_List (Scores);
   Tc_Verify_List (Scores, Tc_Final_Values, Tc_Correct_Final_Values);

   if not Tc_Correct_Final_Values then
      Report.Failed ("List contains incorrect final values");
   end if;

   Report.Result;
end Cc70b02;
