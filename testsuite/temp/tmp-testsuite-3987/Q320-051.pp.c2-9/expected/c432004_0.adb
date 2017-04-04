--==================================================================--

package body C432004_0 is

   function Tc_Correct_Result
     (Rec : Sampletype_F'Class;
      E   : Eras) return Boolean
   is
   begin
      return (Rec.Era = E);
   end Tc_Correct_Result;

end C432004_0;
