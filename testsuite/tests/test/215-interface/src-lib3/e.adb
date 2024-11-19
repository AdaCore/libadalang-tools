with F.A;
pragma Unreferenced (F.A);
with Ada.Text_IO; use Ada.Text_IO;

package body E is

   function Hoho return String is ("Hoho");

   Procedure Hihi is
   begin
      Put_Line (Hoho);
   end Hihi;

end E;
