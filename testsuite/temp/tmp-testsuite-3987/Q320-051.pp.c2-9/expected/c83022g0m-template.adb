separate (C83022g0m)
procedure Template (X : in Integer := A; Y : in out Integer) is
begin  -- TEMPLATE
   if X /= Ident_Int (2) then
      Failed ("INCORRECT RESULTS FOR VARIABLE - 30");
   end if;

   if Y /= Ident_Int (5) then
      Failed ("INCORRECT RESULTS FOR VARIABLE - 31");
   end if;

   Y := Ident_Int (2 * X);

   if C83022g0m.A /= Ident_Int (2) then
      Failed ("INCORRECT RESULTS FOR OUTER HOMOGRAPH - " & "32");
   end if;
end Template;
