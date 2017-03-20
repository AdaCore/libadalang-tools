with C23006f_Inst, C23006finst;
with Report; use Report;
procedure C23006f is
begin
   Test
     ("C23006F",
      "CHECK THAT UNDERSCORES ARE SIGNIFICANT IN " &
      "NAMES USED FOR A LIBRARY PACKAGE INSTANTIATION");

   if C23006f_Inst.A + Ident_Int (1) /= C23006finst.A then
      Failed ("INCORRECT PACKAGE IDENTIFICATION - 1");
   end if;

   Result;
end C23006f;
