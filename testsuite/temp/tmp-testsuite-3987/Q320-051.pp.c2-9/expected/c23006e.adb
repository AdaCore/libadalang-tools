with P1, F1, C230063proc, C23006egfunc;
with Report; use Report;
procedure C23006e is

   X1, X2 : Integer;
   procedure P2 is new C230063proc;
   function F2 is new C23006egfunc;

begin
   Test
     ("C23006E",
      "CHECK UNDERSCORES ARE SIGNIFICANT " &
      "FOR GENERIC LIBRARY SUBPROGRAM IDENTIFIERS");

   P1 (X1);
   P2 (X2);
   if X1 + Ident_Int (1) /= X2 then
      Failed ("INCORRECT PROCEDURE IDENTIFICATION");
   end if;

   if F1 + Ident_Int (1) /= F2 then
      Failed ("INCORRECT FUNCTION IDENTIFICATION");
   end if;

   Result;
end C23006e;
