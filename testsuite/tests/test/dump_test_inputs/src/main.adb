with Matrix_Data; use Matrix_Data;
with Number;

with Ada.Text_IO;

procedure Main is
begin
   Reset_Matrixes;
   Perform_Calculations (Matrix_1, Matrix_2);

   if Number.Overflow then
      Ada.Text_IO.Put ("overflow detected");
   else
      for I in 1 .. 4 loop
         for J in 1 .. 4 loop
            Ada.Text_IO.Put
              (Number.Number_Type'Image (Matrix_Result (I, J)) & " ");
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end if;

end Main;
