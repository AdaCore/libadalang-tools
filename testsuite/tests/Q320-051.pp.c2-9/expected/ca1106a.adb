with Report; use Report;
with Ca1106a_1, Ca1106a_2, Ca1106a_3;
use Ca1106a_1;
procedure Ca1106a is

   package Ca1106a_2x is new Ca1106a_2 (Integer);
   function Ca1106a_3x is new Ca1106a_3 (Integer);

   use Ca1106a_2x;

begin
   Test
     ("CA1106A",
      "CHECK THAT A WITH CLAUSE FOR A PACKAGE BODY " &
      "(GENERIC OR NONGENERIC) OR FOR A GENERIC " &
      "SUBPROGRAM BODY CAN NAME THE CORRESPONDING " &
      "SPECIFICATION, AND A USE CLAUSE CAN ALSO BE " &
      "GIVEN");

   if I /= 1 then
      Failed ("INCORRECT VALUE FROM NONGENERIC PACKAGE");
   end if;

   if J /= 2 then
      Failed ("INCORRECT VALUE FROM GENERIC PACKAGE");
   end if;

   if Ca1106a_3x /= 3 then
      Failed ("INCORRECT VALUE FROM GENERIC SUBPROGRAM");
   end if;

   Result;
end Ca1106a;
