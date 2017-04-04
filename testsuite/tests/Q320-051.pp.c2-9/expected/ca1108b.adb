with Report, Ca1108b_Pkg;
use Report, Ca1108b_Pkg;
procedure Ca1108b is

   Var1, Var2 : Integer;

begin

   Test
     ("CA1108B",
      "IF DIFFERENT WITH_CLAUSES GIVEN FOR PACKAGE " &
      "SPEC AND BODY, ALL NAMED UNITS ARE VISIBLE " &
      "IN THE BODY AND ITS SUBUNITS");

   Proc;

   Var1 := 0;
   Var2 := 1;
   Call_Subs (Var1, Var2);
   if Var1 /= 1 then
      Failed ("FIRST_PKG FUNCTION NOT VISIBLE IN SUBUNIT");
   end if;

   if Var2 /= 3 then
      Failed ("LATER_PKG FUNCTION NOT VISIBLE IN SUBUNIT");
   end if;

   Result;

end Ca1108b;
