with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
separate (Cd5003i_Pack3)
procedure Proc2 is
   type Fixd is delta 0.1 range -10.0 .. 10.0;

   Test_Var : Fixd;
   for Test_Var use at Spprt13.Variable_Address;

   use System;

   function Ident (P : Fixd) return Fixd is
   begin
      if Equal (3, 3) then
         return P;
      else
         return 0.0;
      end if;
   end Ident;
begin
   Test
     ("CD5003I",
      "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT BE " &
      "GIVEN FOR A GENERIC PROCEDURE BODY SUBUNIT " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS A " &
      "'WITH' CLAUSE IS GIVEN FOR THE UNIT " &
      "CONTAINING THE GENERIC PROCEDURE SPECIFICATION");

   Test_Var := Ident (3.3);

   if Test_Var /= 3.3 then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;

   Result;
end Proc2;
