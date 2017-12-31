with Spprt13;
with Report; use Report;
pragma Elaborate (Spprt13);
pragma Elaborate (Report);
package body Cd5003f_Pack2 is
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

   procedure Require_Body is
   begin
      null;
   end Require_Body;
begin
   Test
     ("CD5003F",
      "A 'WITH' CLAUSE NAMING 'SYSTEM' NEED NOT " &
      "BE GIVEN FOR A GENERIC PACKAGE BODY " &
      "CONTAINING AN ADDRESS CLAUSE AS LONG AS A " &
      "'WITH' CLAUSE IS GIVEN FOR THE GENERIC " & "PACKAGE SPECIFICATION");

   Test_Var := Ident (Atype'(others => 3));

   if Test_Var /= Atype'(others => 3) then
      Failed ("INCORRECT VALUE FOR TEST_VAR");
   end if;

   if Test_Var'Address /= Spprt13.Variable_Address then
      Failed ("INCORRECT ADDRESS FOR TEST_VAR");
   end if;
end Cd5003f_Pack2;
