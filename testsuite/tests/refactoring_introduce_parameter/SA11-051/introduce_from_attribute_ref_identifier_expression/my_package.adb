with Ada.Text_IO;

package body My_Package is

   type My_Float is new Float;

   MF : constant My_Float := 1.0;

   ------------------------
   -- My_Dummy_Procedure --
   ------------------------

   procedure My_Dummy_Procedure is null;

   procedure My_Procedure;
   --  My_Procedure

   ------------------
   -- My_Procedure --
   ------------------

   procedure My_Procedure is
   begin
      Ada.Text_IO.Put_Line (MF'Image);
      Ada.Text_IO.Put_Line (MF'Image);

      declare
         MF : constant My_Float := 1.0;

      begin
         Ada.Text_IO.Put_Line (MF'Image);
      end;
   end My_Procedure;

end My_Package;
