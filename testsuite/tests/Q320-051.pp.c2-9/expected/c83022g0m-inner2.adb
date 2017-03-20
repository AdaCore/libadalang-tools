separate (C83022g0m)
procedure Inner2 (X : in Integer := C83022g0m.A; A : in out Integer) is
   C : Integer := A;
begin
   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH -10");
   end if;

   if C83022g0m.A /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
   end if;

   if C83022g0m.B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 12");
   end if;

   if C /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 13");
   end if;

   if X /= Ident_Int (2) then
      Failed ("INCORRECT VALUE PASSED IN - 14");
   end if;

   if Equal (1, 1) then
      A := Ident_Int (4);
   else
      A := 1;
   end if;
end Inner2;
