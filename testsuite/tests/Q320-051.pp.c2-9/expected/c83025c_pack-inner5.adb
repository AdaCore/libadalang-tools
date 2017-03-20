separate (C83025c_Pack)
procedure Inner5 (X : in out Integer; F : in Float; Z : Character := Y) is
begin
   X := Integer (F);

   if Y /= 'A' then
      Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 40");
   end if;

   if Z /= 'A' then
      Failed ("INCORRECT VALUE FOR INNER VARIABLE - 41");
   end if;
end Inner5;
