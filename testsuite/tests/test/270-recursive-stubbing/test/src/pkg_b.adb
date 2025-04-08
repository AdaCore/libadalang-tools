with Ada.Text_IO; use Ada.Text_IO;

package body Pkg_B is
   procedure Print_Magic_Number is
   begin
      Put_Line (Positive'Image (Magic_Number));
   end Print_Magic_Number;
end Pkg_B;
