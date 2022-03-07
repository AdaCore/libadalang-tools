with Ada.Text_IO;
package body Foo is
   type Baz is record
      A, C, Z : Integer;
      B : Float := 1.0;
   end record;
   
   procedure Bar is
      A : constant Baz := (1, 2, 3, 2.0);
      B : constant Baz := (A => 1, C => 3, Z => 2, others => <>);
   begin
      Ada.Text_IO.Put_Line (A.A'Image);
      Ada.Text_IO.Put_Line (B.B'Image);
   end Bar;
   
begin
   Ada.Text_IO.Put_Line ("Package elaboration");
end Foo;
