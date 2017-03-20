procedure C86004a2 is
   subtype Int is Integer range 0 .. 10;
   A : Int := Ident_Int (10);
   B : Int := Ident_Int (0);
begin
   declare
      procedure Switch is new Standard.C86004a_Swap (Int);
   begin
      Switch (A, B);
   end;
   if A /= Ident_Int (0) then
      Failed ("STANDARD.GENERIC PROCEDURE - B-0");
   end if;
   if B /= Ident_Int (10) then
      Failed ("STANDARD.GENERIC PROCEDURE - B-10");
   end if;
end C86004a2;
