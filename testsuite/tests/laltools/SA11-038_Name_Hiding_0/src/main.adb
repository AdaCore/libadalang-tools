with Ada.Text_IO; use Ada.Text_IO;
with Z; use Z;

procedure Main is
   type A is array (0 .. 1) of Natural;
   Var_A : A := (0, others => 0);
   Var_Y : Y := Y'First;
begin
   declare
      type B is array (0 .. 2) of Natural;
      Var_BA : A := (2, others => 0);
      Var_BB : B := (4, others => 0);
   begin
      Put_Line (Var_A'Length'Image);
      Put_Line (Var_BA'Length'Image);
      Put_Line (Var_BB'Length'Image);
   end;
end Main;
