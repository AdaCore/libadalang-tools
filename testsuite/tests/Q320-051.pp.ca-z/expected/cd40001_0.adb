--==================================================================--

with Report;
package body Cd40001_0 is

   procedure Tc_Check_Press is
      My_Press_First : Press_The_Bounds := Negative_Large;
      My_Press_Last  : Press_The_Bounds := Positive_Large;
   begin
      if Tc_Compare_Press (My_Press_First) /= System.Min_Int or
        Tc_Compare_Press (My_Press_Last) /= System.Max_Int
      then
         Report.Failed
           ("Expected enumeration size of System.Min_Int and System.Max_Int " &
            "not available for this implementation");
      end if;
   end Tc_Check_Press;

   ---------------------------------------------------------------------------
   procedure Tc_Check_Add is
      My_Monday    : Add_The_Bounds := Monday;
      My_Tuesday   : Add_The_Bounds := Tuesday;
      My_Wednesday : Add_The_Bounds := Wednesday;
      My_Thursday  : Add_The_Bounds := Thursday;
      My_Friday    : Add_The_Bounds := Friday;
      My_Saturday  : Add_The_Bounds := Saturday;
   begin
      if Tc_Compare_Add (My_Monday) /= (System.Min_Int) or
        Tc_Compare_Add (My_Thursday) /= (System.Min_Int + 3) or
        Tc_Compare_Add (My_Wednesday) /= (System.Min_Int + 2) or
        Tc_Compare_Add (My_Tuesday) /= (System.Min_Int + 1) or
        Tc_Compare_Add (My_Saturday) /= (System.Min_Int + 5) or
        Tc_Compare_Add (My_Friday) /= (System.Min_Int + 4)
      then
         Report.Failed
           ("Expected enumeration size of System.Min_Int, System.Min_Int + 1 " &
            "through System.Min_Int + 5 not available for this implementation");
      end if;
   end Tc_Check_Add;

   ---------------------------------------------------------------------------
   procedure Tc_Check_Minus is
      My_Jan : Minus_The_Bounds := Jan;
      My_Feb : Minus_The_Bounds := Feb;
      My_Mar : Minus_The_Bounds := Mar;
      My_Apr : Minus_The_Bounds := Apr;
   begin
      if Tc_Compare_Minus (My_Jan) /= (System.Max_Int - 3) or
        Tc_Compare_Minus (My_Feb) /= (System.Max_Int - 2) or
        Tc_Compare_Minus (My_Mar) /= (System.Max_Int - 1) or
        Tc_Compare_Minus (My_Apr) /= (System.Max_Int)
      then
         Report.Failed
           ("Expected enumeration size of System.Max_Int, System.Max_Int - 1 " &
            "through System.Max_Int - 3 not available for this implementation");
      end if;
   end Tc_Check_Minus;

end Cd40001_0;
