separate (C83025c_Pack)
function Inner4 (X : Integer; Z : Enum := Y) return Integer is
   C : Integer := A;
   A : Integer := Ident_Int (3);
begin
   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 30");
   end if;

   if C83025c_Pack.A /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 31");
   end if;

   if C83025c_Pack.B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 32");
   end if;

   if C /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 33");
   end if;

   if X /= Ident_Int (2) then
      Failed ("INCORRECT VALUE PASSED IN - 34");
   end if;

   if Y /= One then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 35");
   end if;

   if Z /= One then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 36");
   end if;

   if Equal (1, 1) then
      return A;
   else
      return X;
   end if;
end Inner4;
