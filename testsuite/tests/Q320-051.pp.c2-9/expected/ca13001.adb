--==================================================================--

with Ca13001_0;
with Ca13001_1.Ca13001_5;        -- Implicitly with parent, CA13001_1.
with Report;

procedure Ca13001 is

   Mommy          : Ca13001_0.Family         := Ca13001_0.Mother;
   Daddy          : Ca13001_0.Family         := Ca13001_0.Father;
   Bg             : Ca13001_0.Family         := Ca13001_0.Teen;
   Bg_Clunker     : Ca13001_1.Transportation := Ca13001_1.Clunker;
   Get_Key        : Ca13001_1.Key_Type;
   Get_Transit    : Boolean                  := False;
   Return_Transit : Boolean                  := False;

begin
   Report.Test
     ("CA13001",
      "Check that a protected subunit declared in " &
      "a child unit of a private parent have the same visibility " &
      "into its parent, its parent's siblings, and packages on " &
      "which its parent depends");

   -- Get transportation for mother to go to work.
   Ca13001_0.Destination (Ca13001_0.Mother) := Ca13001_0.Work;
   Ca13001_1.Ca13001_5.Provide_Transportation (Mommy, Get_Key, Get_Transit);
   if not Get_Transit then
      Report.Failed ("Failed to get mother transportation");
   end if;

   -- Get transportation for teen to go to school.
   Ca13001_0.Destination (Ca13001_0.Teen) := Ca13001_0.School;
   Get_Transit                            := False;
   Ca13001_1.Ca13001_5.Provide_Transportation (Bg, Get_Key, Get_Transit);
   if not Get_Transit then
      Report.Failed ("Failed to get teen transportation");
   end if;

   -- Get transportation for father to go to the beach.
   Ca13001_0.Destination (Ca13001_0.Father) := Ca13001_0.Beach;
   Get_Transit                              := False;
   Ca13001_1.Ca13001_5.Provide_Transportation (Daddy, Get_Key, Get_Transit);
   if Get_Transit and not Ca13001_1.Walking then
      Report.Failed ("Failed to make daddy to walk to the beach");
   end if;

   -- Return the clunker.
   Ca13001_1.Ca13001_5.Return_Transportation (Bg_Clunker, Return_Transit);
   if not Return_Transit then
      Report.Failed ("Failed to get back the clunker");
   end if;

   Report.Result;

end Ca13001;
