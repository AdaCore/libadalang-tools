   --=====================================================================--

with Report;
package body C432003_0 is

   procedure Avoid_Optimization (Rec : in out Root) is
   begin
      Rec.S1 := Report.Ident_Str (Rec.S1);
   end Avoid_Optimization;

   procedure Avoid_Optimization (Rec : in out Multi_Root) is
   begin
      Rec.S1 := Report.Ident_Str (Rec.S1);
   end Avoid_Optimization;

end C432003_0;
