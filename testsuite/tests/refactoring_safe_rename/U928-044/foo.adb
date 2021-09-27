with Ada.Text_IO;
package body Foo is
   package body Bar is
      D : constant := 1;
      procedure Baz (B : A) is
      begin
         Ada.Text_IO.Put_Line (D'Image);
      end Baz;
   end Bar;
end Foo;

