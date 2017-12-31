     --==================================================================--

with Report;
package body F3a2a00 is

   procedure Tc_Display_Results (Actual : in Tc_Result_Kind;
      Expected : in Tc_Result_Kind; Message : in String)
   is
   begin
      if Actual /= Expected then
         case Actual is
            when Ok =>
               Report.Failed ("No exception raised: " & Message);
            when P_E =>
               Report.Failed ("Program_Error raised: " & Message);
            when O_E =>
               Report.Failed ("Unexpected exception raised: " & Message);
         end case;
      end if;
   end Tc_Display_Results;

end F3a2a00;
