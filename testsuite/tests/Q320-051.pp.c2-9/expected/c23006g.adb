with C23006g_Instp, C23006ginstp, C23006g_Instf, C23006ginstf;
with Report; use Report;
procedure C23006g is
   X1, X2 : Integer;
begin
   Test
     ("C23006G",
      "CHECK THAT UNDERSCORES ARE SIGNFICANT IN NAMES " &
      "USED FOR A LIBRARY SUBPROGRAM INSTANTIATION");
   C23006g_Instp (X1);
   C23006ginstp (X2);

   if X1 + Ident_Int (1) /= X2 then
      Failed ("INCORRECT PROCEDURE IDENTIFICATION");
   end if;

   if C23006g_Instf + Ident_Int (1) /= C23006ginstf then
      Failed ("INCORRECT FUNCTION IDENTIFICATION");
   end if;

   Result;
end C23006g;
