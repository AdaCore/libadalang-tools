procedure Nonstatic is
   One : Integer := 1;
   type Nonstatic is range One .. 10;
begin
   for X in Nonstatic loop
      null;
   end loop;
   for X in Nonstatic'Range loop
      null;
   end loop;
   for X in Nonstatic range Nonstatic'Range loop
      null;
   end loop;
   for X in Nonstatic'First .. Nonstatic'last loop
      null;
   end loop;
   for X in Nonstatic range Nonstatic'First .. Nonstatic'last loop
      null;
   end loop;
   for X in One .. 10 loop
      null;
   end loop;
end Nonstatic;
