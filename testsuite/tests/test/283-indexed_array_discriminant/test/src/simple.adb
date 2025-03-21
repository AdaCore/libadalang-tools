package body Simple
with SPARK_Mode
is

   procedure Check_Record (R : My_Record) is
   begin
      if R.Size = 10 then
         for N in R.Data'Range loop
            pragma Assert (R.Data (N) /= 42);
         end loop;
      end if;
   end Check_Record;

end Simple;
