package body My_Package is

   procedure My_Procedure is
      Start_Index : constant Integer := 1;
      End_Index   : constant Integer := 10;

   begin
      for J in Start_Index .. End_Index loop
         null;
         exit when J = Start_Index + 2;
         declare
            Dummy : Integer;
         begin
            for K in Start_Index .. End_Index loop
               exit when J = Start_Index + 2;
            end loop;
         end;
      end loop;
   end My_Procedure;

end My_Package;
