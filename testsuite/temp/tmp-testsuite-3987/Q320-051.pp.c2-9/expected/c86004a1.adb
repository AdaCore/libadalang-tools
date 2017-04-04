with C86004a_Swap;
with Report; use Report;
procedure C86004a1 is
   subtype Int is Integer range 0 .. 10;
   A : Int := Ident_Int (10);
   B : Int := Ident_Int (0);
   procedure Switch is new Standard.C86004a_Swap (Int);
begin
   Switch (A, B);

   if A /= Ident_Int (0) then
      Failed ("STANDARD.GENERIC PROCEDURE - 1");
   end if;

   if B /= Ident_Int (10) then
      Failed ("STANDARD.GENERIC PROCEDURE - 2");
   end if;
end C86004a1;
