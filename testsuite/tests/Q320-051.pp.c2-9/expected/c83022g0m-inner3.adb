separate (C83022g0m)
function Inner3 (X : Integer) return Integer is
   C : Integer := A;
   A : Integer := Ident_Int (3);
begin
   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
   end if;

   if C83022g0m.A /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
   end if;

   if C83022g0m.B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
   end if;

   if C /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 23");
   end if;

   if X /= Ident_Int (2) then
      Failed ("INCORRECT VALUE PASSED IN - 24");
   end if;

   if Equal (1, 1) then
      return A;
   else
      return X;
   end if;
end Inner3;
