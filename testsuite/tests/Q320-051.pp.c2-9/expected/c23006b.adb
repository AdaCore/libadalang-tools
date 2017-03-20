with C23006bpkg, C23006b_Pkg;
use C23006bpkg, C23006b_Pkg;
with Report; use Report;
procedure C23006b is
begin
   Test
     ("C23006B",
      "CHECK UNDERSCORES ARE SIGNIFICANT " &
      "FOR LIBRARY PACKAGE IDENTIFIERS");

   if A + Ident_Int (4) /= D then
      Failed ("INCORRECT PACKAGE IDENTIFICATION");
   end if;

   Result;
end C23006b;
