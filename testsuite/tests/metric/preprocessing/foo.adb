procedure Foo (I : in out Integer) is
begin
   if I > 0 then
      goto Fin;
      #if DEBUG then
      if I < 10 then
      #else
      if I < 20 then
      #end if;
         goto Fin;
      end if;
   end if;
   <<Fin>>
end;
