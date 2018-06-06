     --==================================================================--

with Cc70001_2;            -- Generic "zeroing" op for lists of discrete types.
with Cc70001_3;            -- Types for application.
with Cc70001_4.Cc70001_5;  -- Discrete list abstraction + additional ops.

with Report;
procedure Cc70001 is

   package Lists_Of_Scores renames Cc70001_4;
   package Score_Ops renames Cc70001_4.Cc70001_5;

   Scores : Lists_Of_Scores.List_Type;                -- List of points.

   procedure Reset_All_Scores is new Cc70001_2        -- Operation on lists of
     (Elem_Type => Cc70001_3.Points,                  -- points.
      List_Mgr  => Lists_Of_Scores, List_Ops => Score_Ops);

   -- Begin test code declarations: -----------------------

   type Tc_Score_Array is array (1 .. 3) of Cc70001_3.Points;

   Tc_Initial_Values : constant Tc_Score_Array := (2, 4, 6);
   Tc_Final_Values   : constant Tc_Score_Array := (0, 0, 0);

   Tc_Correct_Initial_Values : Boolean := False;
   Tc_Correct_Final_Values   : Boolean := False;

   procedure Tc_Initialize_List (L : in out Lists_Of_Scores.List_Type) is
   begin                                  -- Initial list contains 3 scores
      for I in Tc_Score_Array'Range loop  -- with the values 2, 4, and 6.
         Score_Ops.Add_Element (L, Tc_Initial_Values (I));
      end loop;
   end Tc_Initialize_List;

   procedure Tc_Verify_List
     (L  : in out Lists_Of_Scores.List_Type; Expected : in Tc_Score_Array;
      Ok :    out Boolean)
   is
      Actual : Tc_Score_Array;
   begin                                  -- Verify that all scores have been
      Lists_Of_Scores.Reset (L);          -- set to zero.
      for I in Tc_Score_Array'Range loop
         Score_Ops.Read_Element (L, Actual (I));
      end loop;
      Ok := (Actual = Expected);
   end Tc_Verify_List;

   -- End test code declarations. -------------------------

begin
   Report.Test
     ("CC70001",
      "Check that the template for a generic formal " &
      "package may be a child package, and that a child instance " &
      "which is an instance of the template may be passed as an " &
      "actual to the formal package. Check that the visible part " &
      "of the generic formal package includes the first list of " &
      "basic declarative items of the package specification");

   Tc_Initialize_List (Scores);
   Tc_Verify_List (Scores, Tc_Initial_Values, Tc_Correct_Initial_Values);

   if not Tc_Correct_Initial_Values then
      Report.Failed ("List contains incorrect initial values");
   end if;

   Reset_All_Scores (Scores);
   Tc_Verify_List (Scores, Tc_Final_Values, Tc_Correct_Final_Values);

   if not Tc_Correct_Final_Values then
      Report.Failed ("List contains incorrect final values");
   end if;

   Report.Result;
end Cc70001;
