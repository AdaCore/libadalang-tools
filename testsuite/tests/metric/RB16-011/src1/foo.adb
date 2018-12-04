procedure Foo (I : in out Integer) is
begin
   if I > 0 then
      goto Fin;
   end if;
   <<Fin>>
end;
