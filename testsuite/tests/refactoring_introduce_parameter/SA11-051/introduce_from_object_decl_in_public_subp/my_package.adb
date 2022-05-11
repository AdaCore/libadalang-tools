with Ada.Text_IO;

package body My_Package is

   type My_Float is new Float;

   ------------------
   -- My_Procedure --
   ------------------

   procedure My_Procedure is
      type My_Integer is new Integer;

      MI : constant My_Integer := 1;
      MF : constant My_Float := 1.0;

   begin
      Ada.Text_IO.Put_Line (MI'Image);
      Ada.Text_IO.Put_Line (MF'Image);
   end My_Procedure;

end My_Package;
