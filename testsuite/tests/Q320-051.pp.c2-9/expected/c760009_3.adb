-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with C760009_1;
package body C760009_3 is

   procedure Initialize (Ac : in out Master_Control) is
   begin
      Ac.Data := 42;
      C760009_2.Initialize (C760009_2.Control (Ac));
      C760009_1.Tc_Trace ("Initialize Master_Control");
   end Initialize;

   procedure Validate (Ac : in out Master_Control) is
   begin
      if Ac.Data not in 0 .. 1_000 then
         Report.Failed ("C760009_3.Control did not Initialize");
      end if;
   end Validate;

end C760009_3;
