with Ada.Text_IO;
with Test_Package;

package body My_Package is

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
      C3 : constant Test_Package.Corge := (Q => (B => (F => (I => 3))));
      C4 : constant Test_Package.Corge := (Q => (B => (F => (I => 4))));

      procedure Nested;

      ------------
      -- Nested --
      ------------

      procedure Nested is
         C3 : constant Test_Package.Corge := (Q => (B => (F => (I => 3))));

      begin
         Ada.Text_IO.Put_Line (C3.Q.B.F.I'Image);
      end Nested;

   begin
      Dummy (C3.Q.B.F.I);
      Dummy (C3.Q.B.F.I);
      Dummy (C4.Q.B.F.I);

      declare
         C3 : constant Test_Package.Corge := (Q => (B => (F => (I => 3))));

      begin
         Ada.Text_IO.Put_Line (C3.Q.B.F.I'Image);
      end;

      Nested;
   end My_Procedure;

   procedure Dummy (I : Integer) is null;

end My_Package;
