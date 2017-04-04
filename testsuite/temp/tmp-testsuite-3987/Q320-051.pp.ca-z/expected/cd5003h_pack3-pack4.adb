with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
separate (Cd5003h_Pack3)
package body Pack4 is
   Test_Var : Integer := 0;
   for Test_Var use at Spprt13.Variable_Address;
   use System;
begin
   Test
     ("CD5003H",
      "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT BE " &
      "GIVEN FOR A GENERIC PACKAGE BODY SUBUNIT " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS " &
      "A 'WITH' CLAUSE IS GIVEN FOR THE UNIT " &
      "CONTAINING THE GENERIC PACKAGE SPECIFICATION.");

   Test_Var := Ident_Int (3);

   if Test_Var /= 3 then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;
end Pack4;
