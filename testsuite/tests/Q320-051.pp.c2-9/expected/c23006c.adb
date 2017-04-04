with C23006c_Proc, C23006cproc, C23006c_Func, C23006cfunc;
with Report; use Report;
procedure C23006c is
   X1, X2 : Integer;
begin
   Test
     ("C23006C",
      "CHECK UNDERSCORES ARE SIGNIFICANT " & "FOR LIBRARY SUBPROGRAM");

   C23006c_Proc (X1);
   C23006cproc (X2);
   if X1 + Ident_Int (1) /= X2 then
      Failed ("INCORRECT PROCEDURE IDENTIFICATION");
   end if;

   if C23006c_Func + Ident_Int (1) /= C23006cfunc then
      Failed ("INCORRECT FUNCTION IDENTIFICATION");
   end if;

   Result;
end C23006c;
