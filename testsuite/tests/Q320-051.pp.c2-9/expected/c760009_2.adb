-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

package body C760009_2 is

   procedure Initialize (Av : in out Control) is
   begin
      Initialized := Initialized + 1;
      C760009_1.Tc_Trace ("Initialize _2.Control");
   end Initialize;

   procedure Finalize (Av : in out Control) is
   begin
      Finalized := Finalized + 1;
      C760009_1.Tc_Trace ("Finalize _2.Control");
   end Finalize;

end C760009_2;
