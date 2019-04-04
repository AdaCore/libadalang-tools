procedure Fors is
   type Static is range 1 .. 10;
   One : Integer := 1;
   type Nonstatic is range One .. 10;
begin
   for X in Static loop
      null;
   end loop;
   for X in Static'Range loop
      null;
   end loop;
   for X in Static range Static'Range loop
      null;
   end loop;
   for X in Static'First .. Static'last loop
      null;
   end loop;
   for X in Static range Static'First .. Static'last loop
      null;
   end loop;
   for X in 1 .. 10 loop
      null;
   end loop;

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
end Fors;
