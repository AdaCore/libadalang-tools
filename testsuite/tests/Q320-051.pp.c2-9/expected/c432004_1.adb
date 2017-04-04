--==================================================================--

package body C432004_1 is

   function Tc_Correct_Result
     (Rec : Sampletype_C'Class;
      P   : Periods) return Boolean
   is
   begin
      return (Rec.Period = P);
   end Tc_Correct_Result;

   -------------------------------------------------------------
   function Tc_Correct_Result
     (Rec : Sampletype_H'Class;
      P   : Periods;
      E   : C432004_0.Eras) return Boolean
   is
   begin
      return (Rec.Period = P) and C432004_0.Tc_Correct_Result (Rec, E);
   end Tc_Correct_Result;

end C432004_1;
