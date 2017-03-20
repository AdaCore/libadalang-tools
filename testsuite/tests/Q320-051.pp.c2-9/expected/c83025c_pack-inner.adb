separate (C83025c_Pack)
procedure Inner (X : in out Integer) is
   C : Integer := A;
   A : Integer := Ident_Int (3);
begin
   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 1");
   end if;

   if C83025c_Pack.A /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 2");
   end if;

   if C83025c_Pack.B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 3");
   end if;

   if C /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 4");
   end if;

   if X /= Ident_Int (2) then
      Failed ("INCORRECT VALUE PASSED IN - 5");
   end if;

   if Y /= 2.0 then
      Failed ("INCORRECT VALUE INNER HOMOGRAPH - 6");
   end if;

   if Equal (1, 1) then
      X := A;
   else
      X := C83025c_Pack.A;
   end if;
end Inner;
