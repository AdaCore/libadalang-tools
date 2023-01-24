with Ada.Text_IO;
package body Foo is
   type Baz is record
      A, Z : Integer;
      C : Integer := 1;
      B : Float := 2.0;
   end record;

   procedure Bar is
      A : constant Baz := (1, 2, others=> <>);
      B : constant Baz := (A => 1, Z => 2, C => 3, others => <>);
   begin
      Ada.Text_IO.Put_Line (A.A'Image);
      Ada.Text_IO.Put_Line (B.Z'Image);
      Ada.Text_IO.Put_Line (B.C'Image);
   end Bar;
   
begin
   Ada.Text_IO.Put_Line ("Package elaboration");
end Foo;
