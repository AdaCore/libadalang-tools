with Report, Ca1004a_Pkg;
use Report;

procedure Ca1004a is

   I : Integer := Ident_Int (0);

begin
   Test ("CA1004A", "A PACKAGE DECLARATION AND BODY SUBMITTED " & "TOGETHER");

   Ca1004a_Pkg.I := Ca1004a_Pkg.I + Ident_Int (5);
   if Ca1004a_Pkg.I /= 15 then
      Failed
        ("PACKAGED VARIABLE NOT ACCESSIBLE OR " & "PACKAGE BODY NOT EXECUTED");
   end if;

   Ca1004a_Pkg.P (I);
   if I /= 1 then
      Failed ("PACKAGED PROCEDURE NOT EXECUTED");
   end if;

   Result;
end Ca1004a;
