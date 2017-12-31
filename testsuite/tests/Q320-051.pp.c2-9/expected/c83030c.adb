with Report; use Report;
with C83030c_Decl1, C83030c_Decl2;
use C83030c_Decl1, C83030c_Decl2;
procedure C83030c is
begin
   Test
     ("C83030C",
      "CHECK THAT WITHIN A GENERIC SUBPROGRAM BODY " &
      "COMPILED AS A SUBUNIT IN THE SAME COMPILATION," &
      " NON-HOMOGRAPH SUBPROGRAMS DECLARED OUTSIDE " &
      "THE GENERIC UNIT, AND HAVING THE SAME " & "IDENTIFIER, ARE NOT HIDDEN");

   One :
   declare
      procedure Proc1 is new C83030c_Decl2.C83030c_Proc1;
   begin
      if Global /= Ident_Int (Integer'First) then
         Failed ("INCORRECT VALUE FOR START OF TEST ONE");
      end if;
      Proc1;
      if Global /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR END OF TEST ONE");
      end if;

      Global := Ident_Int (Integer'First);
      Switch := True;
   end One;

   Two :
   declare
      procedure Proc2 is new C83030c_Decl2.C83030c_Proc2 (Integer);
   begin
      if Global /= Ident_Int (Integer'First) then
         Failed ("INCORRECT VALUE FOR START OF TEST TWO");
      end if;
      Proc2 (1);
      if Global /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR END OF TEST TWO");
      end if;

      Switch := True;
   end Two;

   Three :
   declare
      function Func3 is new C83030c_Decl2.C83030c_Func3;
   begin
      if Func3 /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR END OF TEST THREE");
      end if;

      Switch := True;
   end Three;

   Four :
   declare
      function Func4 is new C83030c_Decl2.C83030c_Func4 (Integer);
   begin
      if Func4 /= Ident_Int (Integer'First) then
         Failed ("INCORRECT VALUE FOR END OF TEST FOUR");
      end if;

      Global := Integer'First;
      Switch := True;
   end Four;

   Result;
end C83030c;
