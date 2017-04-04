with Report;       use Report;
with C83025c_Pack; use C83025c_Pack;
procedure C83025c is

   procedure New_Inner is new Inner;

   procedure New_Inner2 is new Inner2;

   function New_Inner3 is new Inner3;

   function New_Inner4 is new Inner4 (Y => Eobj);

   procedure New_Inner5 is new Inner5;

begin
   Test
     ("C83025C",
      "CHECK THAT A DECLARATION IN A DECLARATIVE " &
      "REGION OF A GENERIC SUBPROGRAM HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   A := Ident_Int (2);
   B := A;

   New_Inner (A);

   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE PASSED OUT - 7");
   end if;

   A := Ident_Int (2);

   New_Inner2 (A => Obj);

   if Obj /= Ident_Int (4) then
      Failed ("INCORRECT VALUE PASSED OUT - 16");
   end if;

   A := Ident_Int (2);

   B := A;

   if New_Inner3 (A) /= Ident_Int (3) then
      Failed ("INCORRECT VALUE PASSED OUT - 27");
   end if;

   A := Ident_Int (2);

   B := A;

   if New_Inner4 (A) /= Ident_Int (3) then
      Failed ("INCORRECT VALUE PASSED OUT - 37");
   end if;

   Obj := 1;

   Flo := 6.25;

   New_Inner5 (Obj, Flo);

   if Obj /= Ident_Int (6) then
      Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 42");
   end if;

   if Y /= 5 then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 50");
   end if;

   if Z /= 5 then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 51");
   end if;

   Result;
end C83025c;
