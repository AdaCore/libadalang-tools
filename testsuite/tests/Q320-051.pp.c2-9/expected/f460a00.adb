     --==================================================================--

with Report;
package body F460a00 is

   procedure Tc_Check_Results
     (Actual  : in Tc_Result_Kind; Expected : in Tc_Result_Kind;
      Message : in String)
   is
   begin
      if Actual /= Expected then
         case Actual is
            when Ok | Un_Init =>
               Report.Failed ("No exception raised: " & Message);
            when Pe_Exception =>
               Report.Failed ("Program_Error raised: " & Message);
            when Others_Exception =>
               Report.Failed ("Unexpected exception raised: " & Message);
         end case;
      end if;
   end Tc_Check_Results;

end F460a00;
