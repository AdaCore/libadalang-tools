with Ada.Text_IO; use Ada.Text_IO;

procedure Testgeneral is
   type My_Int is range 0 .. 1000;

   Arr_One : array (1 .. 1) of My_Int := (1=> V);

begin
   Put_Line (Arr_One (1)'Image);
end Testgeneral;

