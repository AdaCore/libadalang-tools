with Report, Ca1108a_Pkg;
use Report, Ca1108a_Pkg;
procedure Ca1108a is

   Var1, Var2 : Integer;

begin

   Test
     ("CA1108A",
      "WITH_ AND USE_CLAUSES GIVEN FOR A PACKAGE " &
      "SPEC APPLY TO THE BODY AND ITS SUBUNITS");

   Proc;

   Var1 := 1;
   Var2 := 1;
   Call_Subs (Var1, Var2);
   if Var1 /= 4 then
      Failed ("OTHER_PKG VARIABLE NOT VISIBLE IN SUBUNIT");
   end if;

   if Var2 /= 6 then
      Failed ("OTHER_PKG FUNCTION NOT VISIBLE IN SUBUNIT " & "OF SUBUNIT");
   end if;

   Result;

end Ca1108a;
