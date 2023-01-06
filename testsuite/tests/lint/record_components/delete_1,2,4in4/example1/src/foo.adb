with Ada.Text_IO;
package body Foo is
   type Baz is record
      A, C, Z, B : Integer;
   end record;

   procedure Bar is
      A : constant Baz := (1, 2, 3, 1);
      B : constant Baz := (A => 1, C => 3, Z => 2, B => 1);
   begin
      Ada.Text_IO.Put_Line (A.Z'Image);
      Ada.Text_IO.Put_Line (B.Z'Image);
   end Bar;
   
begin
   Ada.Text_IO.Put_Line ("Package elaboration");
end Foo;
