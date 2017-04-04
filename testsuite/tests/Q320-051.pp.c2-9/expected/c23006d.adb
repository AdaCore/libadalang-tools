with C23006dpkg, C23006d_Inst;
use C23006d_Inst;
with Report; use Report;
procedure C23006d is

   package P2 is new C23006dpkg;
   use P2;

begin
   Test
     ("C23006D",
      "CHECK UNDERSCORES ARE SIGNIFICANT " &
      "FOR GENERIC LIBRARY PACKAGE IDENTIFIERS");

   if A + Ident_Int (4) /= D then
      Failed ("INCORRECT PACKAGE IDENTIFICATION - 1");
   end if;

   Result;
end C23006d;
