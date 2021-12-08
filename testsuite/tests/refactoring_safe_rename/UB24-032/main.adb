with Ada.Finalization; use Ada.Finalization;

procedure Main is
   A : constant Natural := 1;

   function F (X : Natural; Y, Z : Natural ) return Natural is
     (A + X + Y + Z + 1);

   B : constant Natural := F (A, 1, 2) with Unreferenced;

begin
   null;
end Main;
