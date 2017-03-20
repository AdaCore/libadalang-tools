with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
package body Cd5003g_Pack2 is
   procedure Cd5003g_Proc2 is
      type Fixd is delta 0.1 range -10.0 .. 10.0;

      Test_Var : Fixd := 0.0;
      for Test_Var use at Spprt13.Variable_Address;
      use System;

      function Ident_Fixd (P : Fixd) return Fixd is
      begin
         if Equal (3, 3) then
            return P;
         else
            return 0.0;
         end if;
      end Ident_Fixd;
   begin
      Test
        ("CD5003G",
         "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT " &
         "BE GIVEN FOR A GENERIC PROCEDURE BODY " &
         "CONTAINING AN ADDRESS CLAUSE AS LONG AS " &
         "A 'WITH' CLAUSE IS GIVEN FOR THE UNIT " &
         "CONTAINING THE GENERIC PROCEDURE " &
         "SPECIFICATION");

      Test_Var := Ident_Fixd (3.3);

      if Test_Var /= 3.3 then
         Failed ("INCORRECT VALUE FOR TEST_VAR");
      end if;

      if Test_Var'Address /= Spprt13.Variable_Address then
         Failed ("INCORRECT ADDRESS FOR TEST_VAR");
      end if;

      Result;
   end Cd5003g_Proc2;
end Cd5003g_Pack2;
