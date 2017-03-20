with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
package body Cd5003a_Pkg2 is
   Test_Var : Integer;
   for Test_Var use at Spprt13.Variable_Address;
   use System;

   procedure Require_Body is
   begin
      null;
   end Require_Body;
begin
   Test
     ("CD5003A",
      "CHECK THAT A 'WITH' CLAUSE NAMING 'SYSTEM' " &
      "NEED NOT BE GIVEN FOR A PACKAGE BODY " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS A " &
      "'WITH' CLAUSE IS GIVEN FOR THE SPECIFICATION");

   Test_Var := Ident_Int (3);

   if Test_Var /= 3 then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;

end Cd5003a_Pkg2;
