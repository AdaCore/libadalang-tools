with Report;    use Report;
with C38108b_P; use C38108b_P;
procedure C38108b is
   Val_1, Val_2 : L;
begin

   Test
     ("C38108B",
      "CHECK THAT INCOMPLETE TYPE CAN BE DECLARED IN " &
      "PRIVATE PART WITHOUT FULL DECLARATION - " &
      "LIBRARY PACKAGE");

   Assign (2, Val_1);
   Assign (2, Val_2);
   if not "=" (Val_1, Val_2) then
      Failed ("INCOMPLETE TYPE NOT FULLY DECLARED");
   end if;

   Result;
end C38108b;
