separate (C83022g0m)
procedure Inner5 (X : in out Integer) is
   C : Integer := A;
   A : Integer := Ident_Int (3);
begin
   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 41");
   end if;

   if C83022g0m.A /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 42");
   end if;

   if C83022g0m.B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 43");
   end if;

   if C /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 44");
   end if;

   if X /= Ident_Int (2) then
      Failed ("INCORRECT VALUE PASSED IN - 45");
   end if;

   if Equal (1, 1) then
      X := A;
   else
      X := C83022g0m.A;
   end if;
end Inner5;
