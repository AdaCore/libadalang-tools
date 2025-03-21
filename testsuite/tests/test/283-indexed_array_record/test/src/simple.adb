package body Simple
with SPARK_Mode
is

   procedure Check_Town (T : Town) is
      Total_Age : Integer := 0;
   begin
      for N in T.People'Range loop
         Total_Age := Total_Age + Integer (T.People (N).Age);
      end loop;
      pragma Assert (Total_Age > 420);
   end Check_Town;

end Simple;
