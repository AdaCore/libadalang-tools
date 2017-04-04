with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
separate (Cd5003c)
package body Cd5003c_Pack2 is
   type Atype is array (1 .. 10) of Integer;

   Test_Var : Atype := (others => 0);
   for Test_Var use at Spprt13.Variable_Address;
   use System;

   function Ident (P : Atype) return Atype is
   begin
      if Equal (3, 3) then
         return P;
      else
         return (others => 0);
      end if;
   end Ident;
begin
   Test
     ("CD5003C",
      "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT " &
      "BE GIVEN FOR A PACKAGE BODY SUBUNIT " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS A " &
      "'WITH' CLAUSE IS GIVEN FOR THE UNIT " &
      "CONTAINING THE PACKAGE SPECIFICATION");

   Test_Var := Ident (Atype'(others => 3));

   if Test_Var /= Atype'(others => 3) then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;
end Cd5003c_Pack2;
