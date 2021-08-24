with Input;
with System;

package body Square is

   function Square return Integer is
      A : Integer := Input.Read_Number;

      Add : System.Address := System.To_Address (-1);
   begin
      return A * A;
   end Square;

end Square;
