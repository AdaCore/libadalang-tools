with Ada.Text_IO;

package body My_Package is

   ------------------
   -- My_Procedure --
   ------------------

   procedure My_Procedure is
      procedure Nested (B : Boolean) is null;

   begin
      Nested (True and False);
   end My_Procedure;

end My_Package;
