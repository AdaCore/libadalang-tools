with Report; use Report;
with C83024e_P1;
with C83024e_P2;
with C83024e_P3;
with C83024e_P4;
use C83024e_P1;
use C83024e_P2;
use C83024e_P3;
use C83024e_P4;
procedure C83024e1m is

begin
   Test
     ("C83024E",
      "CHECK THAT A DECLARATION IN THE DECLARATIVE " &
      "REGION OF A GENERIC PACKAGE HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   declare
      package New_C83024e_Pack1 is new C83024e_Pack1 (C83024e_P1.A);
   begin
      if C83024e_P1.A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 6");
      end if;
   end;

   declare
      package New_C83024e_Pack2 is new C83024e_Pack2 (A => C83024e_P2.Obj);
   begin
      if C83024e_P2.Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 15");
      end if;
   end;

   declare
      package New_C83024e_Pack3 is new C83024e_Pack3 (C83024e_P3.A);
   begin
      if C83024e_P3.A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 25");
      end if;
   end;

   declare
      package New_C83024e_Pack4 is new C83024e_Pack4 (C83024e_P4.Obj, Flo);
   begin
      if C83024e_P4.Obj /= Ident_Int (6) then
         Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 60");
      end if;
   end;

   Result;
end C83024e1m;
