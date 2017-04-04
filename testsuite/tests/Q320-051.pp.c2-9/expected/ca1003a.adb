with Report, Ca1003a_P, Ca1003a_Pkg, Ca1003a_F;
use Report;

procedure Ca1003a is

   I : Integer := Ident_Int (0);

begin
   Test ("CA1003A", "INDEPENDENT UNITS IN A SINGLE FILE");

   Ca1003a_P (I);
   if I /= 1 then
      Failed ("INDEPENDENT PROCEDURE NOT INVOKED");
   end if;

   Ca1003a_Pkg.I := Ca1003a_Pkg.I + Ident_Int (10);
   if Ca1003a_Pkg.I /= 10 then
      Failed ("INDEPENDENT PACKAGE VARIABLE ACCESSED INCORRECTLY");
   end if;

   if Ca1003a_F (Ident_Int (5)) /= -5 then
      Failed ("INDEPENDENT FUNCTION NOT INVOKED");
   end if;

   Result;
end Ca1003a;
