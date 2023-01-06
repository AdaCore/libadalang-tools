with Ada.Text_IO;
package body Foo is
   type Baz is record
      A, C, Z, B, D : Integer; 
   end record;

   function Baz_Constructor (B : Boolean) return Baz is
   begin
      if B then
         return (1, 2, 2, 3, 5);
      else
         return (2, 1, 2, 4, 7);
      end if;
   end Baz_Constructor;
   
   procedure Bar is
      A : constant Baz := (1, 2, 3, 1, 4);
      B : constant Baz := (A => 1, C => 3, Z => 2, B => 1, D=> 4);
   begin
      Ada.Text_IO.Put_Line (A.D'Image);
      Ada.Text_IO.Put_Line (B.Z'Image);
   end Bar;
   
begin
   Ada.Text_IO.Put_Line ("Package elaboration");
end Foo;
