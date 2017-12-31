with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
procedure Cd5003b is
   type Enum is (A0, A1, A2, A3, A4, A5);

   Test_Var : Enum := A0;
   for Test_Var use at Spprt13.Variable_Address;
   use System;

   function Ident_Enum (P : Enum) return Enum is
   begin
      if Equal (3, 3) then
         return P;
      else
         return A0;
      end if;
   end Ident_Enum;

begin
   Test
     ("CD5003B",
      "CHECK THAT A 'WITH' CLAUSE NAMING 'SYSTEM' " &
      "NEED NOT BE GIVEN FOR A PROCEDURE BODY " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS A " &
      "'WITH' CLAUSE IS GIVEN FOR THE PROCEDURE " & "SPECIFICATION");

   Test_Var := Ident_Enum (A3);

   if Test_Var /= A3 then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;

   Result;
end Cd5003b;
