with B.A;
pragma Unreferenced (B.A);
with Ada.Text_IO; use Ada.Text_IO;

package body A is

   function Hoho return String is ("Hoho");

   Procedure Hihi is
   begin
      Put_Line (Hoho);
   end Hihi;

end A;
