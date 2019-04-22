procedure Static is
   type Static is range 1 .. 10;
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
end Static;
