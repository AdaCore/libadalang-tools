with Report, Ca1005a_F, Ca1005a_P;
use Report;

procedure Ca1005a is

   I : Integer := Ident_Int (7);

begin
   Test
     ("CA1005A",
      "SUBPROGRAM DECLARATIONS AND BODIES " & "SUBMITTED TOGETHER");

   if Ca1005a_F (Ident_Int (2)) /= 3 then
      Failed ("FUNCTION NOT EXECUTED");
   end if;

   Ca1005a_P (I);
   if I /= -7 then
      Failed ("PROCEDURE NOT EXECUTED");
   end if;

   Result;
end Ca1005a;
