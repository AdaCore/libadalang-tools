with D.A;
pragma Unreferenced (D.A);
with Ada.Text_IO; use Ada.Text_IO;

package body C is

   function Hoho return String is ("Hoho");

   Procedure Hihi is
   begin
      Put_Line (Hoho);
   end Hihi;

end C;
