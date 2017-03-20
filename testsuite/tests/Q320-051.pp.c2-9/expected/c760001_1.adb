-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body C760001_1 is

   procedure Initialize (Tc : in out Test_Controlled) is
   begin
      if Tc.Last_Proc_Called /= None then
         Report.Failed ("Initialize for Test_Controlled");
      end if;
      Tc.Last_Proc_Called := Init;
      C760001_0.Initialize (C760001_0.Root_Controlled (Tc));
   end Initialize;

   procedure Adjust (Tc : in out Test_Controlled) is
   begin
      Tc.Last_Proc_Called := Adj;
      C760001_0.Adjust (C760001_0.Root_Controlled (Tc));
   end Adjust;

   procedure Finalize (Tc : in out Test_Controlled) is
   begin
      Tc.Last_Proc_Called := Fin;
   end Finalize;

   procedure Initialize (Tc : in out Nested_Controlled) is
   begin
      if Tc.Last_Proc_Called /= None then
         Report.Failed ("Initialize for Nested_Controlled");
      end if;
      Tc.Last_Proc_Called := Init;
      C760001_0.Initialize (C760001_0.Root_Controlled (Tc));
   end Initialize;

   procedure Adjust (Tc : in out Nested_Controlled) is
   begin
      Tc.Last_Proc_Called := Adj;
      C760001_0.Adjust (C760001_0.Root_Controlled (Tc));
   end Adjust;

   procedure Finalize (Tc : in out Nested_Controlled) is
   begin
      Tc.Last_Proc_Called := Fin;
   end Finalize;

end C760001_1;
